from __future__ import absolute_import, division, print_function

from funcy import keep
import json

from langkit.passes import GlobalPass
from contextlib import contextmanager

from collections import defaultdict

try:
    import libpythonlang as lpl
except ImportError:
    lpl = None

templates = {}

def fqn(prop):
    return "{}.{}".format(prop.struct.name.camel, prop._original_name.lower)


class DSLWalker(object):
    """
    DSLWalker can be used to traverse the *Python* syntax tree in which the
    language description is written (with the current langkit DSL).

    This walker is supposed to be used during the unparsing to "follow" the
    tree of AbstractExpressions (e.g. when emitting a Let expression, the
    cursor of the DSLWalker should be on the Python call ``Let(...)``).

    The cursor of the walker can be moved using a bunch of helper functions,
    such as:
      - ``property(name)``: given that the cursor is on a class defining a Node
            type, move the cursor to the property named ``name`` defined in
            that node type.
      - ``[method_]call(name)``: move the cursor to the next Python call to the
            function/method named ``name``.
      - ``arg(index)``: Given that the cursor is on a call (using the afore-
            mentionned ``[method_]call(...)`` construct, move the cursor to
            the ``index``th argument.
      - etc.
    For user-friendliness purposes, these all return a context manager: this
    is to make sure that once a nested construct has been unparsed, the cursor
    moves to after this construction. So it would typically be used as such:
    .. code::
        with my_walker.call('If'):
            with my_walker.arg(0):
                # my_walker's cursor is on the condition of the If expression.
                ...


    The sole purpose of the walker is to be able to unparse the comments
    written in the DSL. This functionality is made available through the
    ``emit_comments`` method of the walker: it will simply return all the
    comments that have been written between the last call to ``emit_comments``
    and the current position of the cursor.
    """
    if lpl is not None:
        ctx = lpl.AnalysisContext()

    class NoOpWalker(object):
        """
        Helper object that accepts any method call and attribute access without
        doing anything. Useful for substituting an actual DSLWalker when it's
        out of sync, without having to write manual checks in the user code.
        """
        def __init__(self):
            pass

        def __getattr__(self, item):
            return self

        def __call__(self, *args, **kwargs):
            return self

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc_val, exc_tb):
            return self

        def emit_comments(self):
            return ""

    @staticmethod
    def class_from_location(loc):
        """
        Construct a DSLWalker and place its cursor on the Python class
        definition that is located at the given location.

        :type loc: langkit.diagnostics.Location
        :rtype: DSLWalker
        """
        if lpl is None or loc is None:
            return DSLWalker.NoOpWalker()

        unit = DSLWalker.ctx.get_from_file(loc.file)
        class_def = unit.root.find(
            lambda n:
            n.is_a(lpl.ClassDef) and
            n.sloc_range.start.line == loc.line
        )

        if not class_def:
            return DSLWalker.NoOpWalker()

        return DSLWalker(class_def)

    def __init__(self, from_node):
        self.current_node = from_node
        self.last_token = from_node.token_start
        self.current_token = self.last_token

    @contextmanager
    def _with_current_node(self, new_node):
        old_node = self.current_node
        self.current_node = new_node
        self.current_token = new_node.token_start
        yield
        self.current_node = old_node
        self.current_token = new_node.token_end

    def property(self, prop):
        """
        Given that the current cursor is on a class defining a Node type,
        move the cursor to the property named ``name`` defined in that node
        type.

        .. note:: is a context manager.

        :type prop: str
        """
        pname = prop._indexing_name

        fun_decl = self.current_node.find(
            lambda n: n.is_a(lpl.FuncDef) and n.f_name.text == pname
        )

        if fun_decl is not None:
            # property is defined with the following syntax:
            # @langkit_property(...)
            # def prop(x):
            #     v0 = Var(...)
            #     ...
            #     return ...
            self.last_token = fun_decl.token_start
            return self._with_current_node(fun_decl.f_body)

        assignment = self.current_node.find(
            lambda n:
            n.is_a(lpl.AssignStmt) and
            n.f_l_value.text == pname and
            n.f_r_values[0][0].is_a(lpl.CallExpr) and
            'property' in n.f_r_values[0][0].f_prefix.text.lower()
        )

        if assignment is not None:
            # property is defined with the following syntax:
            # prop = *Property(...)
            self.last_token = assignment.f_r_values.token_start
            content = assignment.f_r_values[0][0].f_suffix
            return self._with_current_node(content[0].f_expr)

        raise LookupError("Could not find proprety {}".format(pname))

    def var_assignment(self, index):
        """
        Move the cursor to the ``index``th variable assignment:
          - if the current cursor is in a FuncDef, it looks for the
            x = Var(...) pattern and moves to what's inside ``Var``.
          - if the current cursor is in a LambdaDef, it looks at the default
            value of the ``index``th parameter of the lambda.

        .. note:: is a context manager.

        :type index: int
        """
        if self.current_node.is_a(lpl.LambdaDef):
            params = self.current_node.f_args.f_single_params
            return self._with_current_node(params[index].f_default_value)

        var_calls = self.current_node.findall(
            lambda n: n.is_a(lpl.CallExpr) and n.f_prefix.text == "Var"
        )
        return self._with_current_node(var_calls[index].f_suffix)

    def returned_expr(self):
        """
        Moves the cursor to the expression that will be returned by this
        FuncDef or LambdaDef.

        .. note:: is a context manager.
        """
        if self.current_node.is_a(lpl.LambdaDef):
            return self._with_current_node(self.current_node.f_expr)

        return_stmt = self.current_node.find(
            lambda n: n.is_a(lpl.ReturnStmt)
        )
        assert len(return_stmt.f_exprs) == 1
        return self._with_current_node(return_stmt.f_exprs[0])

    def call(self, fun_name):
        """
        Moves the cursor to the next call to a function named ``fun_name``.

        .. note:: is a context manager.

        :type fun_name: str
        """
        def matches(n):
            return (
                n.is_a(lpl.CallExpr) and n.f_prefix.text == fun_name and
                n.token_start >= self.last_token
            )

        call_node = (
            self.current_node
            if matches(self.current_node)
            else self.current_node.find(matches)
        )

        return self._with_current_node(call_node)

    def method_call(self, method_name):
        """
        Moves the cursor to the next call to a method named ``method_name``.

        .. note:: is a context manager.

        :type method_name: str
        """
        def matches(n):
            if n.is_a(lpl.CallExpr):
                if n.f_prefix.is_a(lpl.DottedName):
                    return n.f_prefix.f_suffix.text == method_name
            return False

        call_node = (
            self.current_node
            if matches(self.current_node)
            else self.current_node.find(matches)
        )

        return self._with_current_node(call_node)

    def boolean_binop(self, kind):
        """
        Moves the cursor to the next occurrence of a boolean binop of the
        given kind. This handles boolean binops introduced with the ``&`` and
        ``|`` operators, the ``And`` and ``Or`` calls and also the ``any_of``
        method call.

        .. note:: is a context manager.

        :type kind: str
        """
        from langkit.expressions import BinaryBooleanOperator

        if kind == BinaryBooleanOperator.AND:
            call_name = 'And'
            expr_type = lpl.AndExpr
        else:
            call_name = 'Or'
            expr_type = lpl.OrExpr

        def matches(n):
            if n.is_a(lpl.CallExpr):
                if n.f_prefix.text == call_name:
                    return True
                elif n.f_prefix.is_a(lpl.DottedName):
                    if n.f_prefix.f_suffix.text == 'any_of':
                        return kind == BinaryBooleanOperator.OR
            elif n.is_a(expr_type):
                return True
            return False

        binop_node = (
            self.current_node
            if matches(self.current_node)
            else self.current_node.find(matches)
        )

        return self._with_current_node(binop_node)

    def self_arg(self):
        """
        If the cursor is on a method call, moves the cursor to the prefix
        expression of the call. (i.e. ``self`` from the method POV).

        .. note:: is a context manager.
        """
        assert self.current_node.is_a(lpl.CallExpr)
        assert self.current_node.f_prefix.is_a(lpl.DottedName)
        return self._with_current_node(self.current_node.f_prefix.f_prefix)

    def arg(self, index):
        """
        If the cursor is on a function/method or boolean binop call, moves the
        cursor to the ``index``th argument/operand of the call.

        .. note:: is a context manager.

        :type index: int
        """

        if self.current_node.is_a(lpl.CallExpr):
            return self._with_current_node(
                self.current_node.f_suffix[index].f_expr
            )
        elif self.current_node.is_a(lpl.AndExpr, lpl.OrExpr):
            if index == 0:
                return self._with_current_node(
                    self.current_node.f_left
                )
            elif index == 1:
                return self._with_current_node(
                    self.current_node.f_right
                )

        assert False

    def arg_count(self):
        """
        If the cursor is on a function/method call or boolean binop, return
        the number of arguments/operands that are passed in the call.

        :rtype: int
        """
        if self.current_node.is_a(lpl.CallExpr):
            return len(self.current_node.f_suffix)
        elif self.current_node.is_a(lpl.AndExpr, lpl.OrExpr):
            return 2

        assert False

    def missed_comments(self):
        """
        Generates all the comments that have been missed between the last call
        to ``missed_comments`` and the current cursor position.
        """
        cur_tok = self.current_token
        while self.last_token < cur_tok:
            token = self.last_token
            if token.kind == 'Comment':
                yield token.text
            self.last_token = self.last_token.next

    def emit_comments(self):
        """
        Same as ``missed_comments`` but returns all the comments aggregated in
        a single string.
        :rtype: str
        """
        return "".join("{}$hl".format(c) for c in self.missed_comments())

    def where(self):
        """
        Prints the current location of the walker.
        """
        print(self.current_node)


def sf(strn):
    """
    Real string splicing in cpython.

    Usage:

    >>> a = 12
    >>> b = 15
    >>> r = range(5)
    >>> sf("${a - b} : ${', '.join(str(c) for c in r)}")

    :param basestring strn: The string to format, containing regular python
                            expressions in ${} blocks.
    :rtype: unicode
    """

    from mako.template import Template
    import inspect

    frame = inspect.stack()[1][0]
    if not templates.get(strn):
        t = templates[strn] = Template(
            "\n".join(map(str.strip, strn.splitlines()))
        )
    else:
        t = templates[strn]

    return t.render(**dict(frame.f_locals, **frame.f_globals))


def node_name(node):
    from langkit.compiled_types import ASTNodeType, T
    from langkit.dsl import ASTNode
    from langkit.utils.types import issubtype

    if isinstance(node, T.Defer):
        node = node.get()

    if issubtype(node, ASTNode):
        return node._name.camel

    assert isinstance(node, ASTNodeType), (
        'Unexpected node type: {}'.format(repr(node))
    )

    if node.is_root_list_type:
        return 'ASTList[{}]'.format(node_name(node.element_type))

    return node.dsl_name


def emit_rule(rule, top_level=False):
    from langkit.parsers import (
        _Transform, _Row, Opt, List, Or, _Token, NoBacktrack,
        _Extract, DontSkip, Skip, Null, Parser, resolve, Defer, Predicate,
        Discard
    )
    if isinstance(rule, _Transform):
        inner = emit_rule(rule.parser)
        if len(inner) > 50:
            return "{}($i$hl{}$d$hl)".format(node_name(rule.typ), inner)
        else:
            return "{}({})".format(node_name(rule.typ), inner)
    elif isinstance(rule, _Row):
        return " $sl".join(emit_rule(r) for r in rule.parsers)
    elif isinstance(rule, Opt):
        if rule._booleanize:
            return '{}({})'.format(node_name(rule.booleanized_type),
                                   emit_rule(rule.parser))
        elif isinstance(rule.parser, _Row):
            return "?({})".format(emit_rule(rule.parser))
        else:
            return "?{}".format(emit_rule(rule.parser))
    elif isinstance(rule, _Extract):
        return 'pick({})'.format(emit_rule(rule.parser))
    elif isinstance(rule, List):
        kind= '*' if rule.empty_valid else '+'
        subparser = emit_rule(rule.parser)
        sep = ", {}".format(emit_rule(rule.sep)) if rule.sep else ""
        return ("{list_cls}{kind}({subparser}{sep})"
                .format(list_cls=node_name(rule.type),
                        kind=kind, subparser=subparser, sep=sep)
                if rule.list_cls else
                "list{kind}({subparser}{sep})"
                .format(kind=kind, subparser=subparser, sep=sep))
    elif isinstance(rule, Null):
        return "null({})".format(
            emit_rule(rule.type) if isinstance(rule.type, Parser)
            else type_name(rule.type)
        )
    elif isinstance(rule, Or):
        body = ' | '.join(emit_rule(r) for r in rule.parsers)
        if len(body) > 50:
            return "or($i$hl| {}$d$hl)".format(
                '$hl| '.join(emit_rule(r) for r in rule.parsers)
            )
        else:
            return "or({})".format(body)
    elif isinstance(rule, DontSkip):
        return "{}.dont_skip({})".format(
            emit_rule(rule.subparser),
            " ".join((emit_rule(resolve(d)) for d in rule.dontskip_parsers))
        )
    elif isinstance(rule, Skip):
        return "skip({})".format(node_name(rule.dest_node))
    elif isinstance(rule, _Token):
        if rule._original_string:
            return '"{}"'.format(rule._original_string)
        else:
            return "@{}{}".format(
                format_token_name(rule._val.name),
                '("{}")'.format(rule.match_text)
                if rule.match_text else ""
            )
    elif isinstance(rule, NoBacktrack):
        return "/"
    elif isinstance(rule, Defer):
        return str(rule)
    elif isinstance(rule, Predicate):
        return "{} |> when({})".format(
            emit_rule(rule.parser), rule.property_name
        )
    elif isinstance(rule, Discard):
        return "discard({})".format(emit_rule(rule.parser))
    else:
        raise NotImplementedError(rule.__class__)


def unparsed_name(strn):
    # Special cases: some variable names are keywords. Rename them.
    if strn == "val":
        return "value"
    elif strn == "match":
        return "matched"
    else:
        return strn


def var_name(var_expr, default="_"):
    from langkit.compiled_types import Argument
    if isinstance(var_expr, Argument):
        ret = var_expr.name.lower
    else:
        ret = (
            var_expr.source_name.lower if var_expr.source_name else default
        )

    return unparsed_name(ret)


def is_a(expr, *names):
    return any(expr.__class__.__name__ == n for n in names)


def needs_parens(expr):
    from langkit.expressions import (FieldAccess, Literal, AbstractVariable,
                                     BigIntLiteral, Map, Quantifier, EnvGet)
    return not (
        isinstance(expr, (FieldAccess, Literal, AbstractVariable, BigIntLiteral, EnvGet,
                          Map, Quantifier))
        or is_a(expr, "as_entity", "as_bare_entity", "children",
              "env_parent", "rebindings_parent", "parents", "parent", "root",
              "append_rebinding", "concat_rebindings", "env_node",
              "rebindings_new_env", "rebindings_old_env", "get_value",
              "solve", "is_referenced_from", "env_group", "length",
              "can_reach", "as_int", "unique", "env_orphan")
    )


def emit_indent_expr(expr, **ctx):
    strn = emit_expr(expr, **ctx)
    starts_with_comment = len(strn) > 0 and strn[0] == '#'
    if len(strn) > 40 or starts_with_comment:
        return "$i$hl{}$d$hl".format(strn)
    else:
        return strn


def prepend_comments(expr, **ctx):
    arg_expr = ctx.pop('arg_expr', None)
    if arg_expr is None:
        return emit_expr(expr, **ctx), False

    walker = ctx.get('walker')
    with walker.arg(arg_expr):
        coms = walker.emit_comments()
        strn = emit_expr(expr, **ctx)

    if coms == "":
        return strn, False

    return coms + strn, True


def emit_paren_expr(expr, **ctx):
    from langkit.expressions import Let

    strn, has_coms = prepend_comments(expr, **ctx)

    if not needs_parens(expr) and not has_coms:
        return emit_paren(strn) if len(strn) > 40 else strn
    elif isinstance(expr, Let):
        return strn
    else:
        return emit_paren(strn, force=has_coms)


def emit_paren(strn, force=False):
    if force or len(strn) > 40:
        return "($i$hl{}$d$hl)".format(strn)
    else:
        return "({})".format(strn)


def emit_nl(strn):
    if len(strn) > 40:
        return "$i$hl{}$d$hl".format(strn)
    else:
        return "{}".format(strn)


def emit_expr(expr, **ctx):
    from langkit.expressions import (
        Literal, Let, FieldAccess, AbstractVariable, SelfVariable,
        EntityVariable, LogicTrue, LogicFalse, unsugar, Map, All, Any,
        GetSymbol, Match, Eq, BinaryBooleanOperator, Then, OrderingTest,
        Quantifier, If, IsNull, Cast, DynamicVariable, IsA, Not, SymbolLiteral,
        No, Cond, New, CollectionSingleton, Concat, EnumLiteral, EnvGet,
        ArrayLiteral, Arithmetic, PropertyError, CharacterLiteral, Predicate,
        StructUpdate, BigIntLiteral, RefCategories, Bind, Try, Block, Contains
    )

    def is_a(*names):
        return any(expr.__class__.__name__ == n for n in names)


    then_underscore_var = ctx.get('then_underscore_var')
    overload_coll_name = ctx.get('overload_coll_name')
    walker = ctx.get('walker')

    def emit_lambda(expr, vars):
        vars_str = ", ".join(var_name(var) for var in vars)
        return "({}) => {}".format(vars_str, ee(expr))
        del vars_str

    def emit_method_call(receiver, name, args=[]):
        if not args:
            return "{}.{}".format(receiver, name)
        else:
            return "{}.{}{}".format(
                receiver, name, emit_paren(", ".join(args))
            )

    def emit_let(expr):
        if len(expr.vars) == 0:
            with walker.returned_expr():
                return walker.emit_comments() + ee(expr.expr)

        vars_defs = ""
        for i, (var, abs_expr) in enumerate(zip(expr.vars, expr.var_exprs)):
            with walker.var_assignment(i):
                vars_defs += "{}val {} = {};$hl".format(
                    walker.emit_comments(), var_name(var), ee(abs_expr)
                )
            vars_defs += walker.emit_comments()

        with walker.returned_expr():
            return "{{$i$hl{}$hl{}{}$hl$d}}".format(
                vars_defs, walker.emit_comments(), ee(expr.expr)
            )

    def ee(expr, **extra_ctx):
        full_ctx = dict(ctx, **extra_ctx)
        return emit_expr(expr, **full_ctx)

    def ee_pexpr(expr, **extra_ctx):
        # We don't want to carry an arg_expr data left in the context from a
        # previous call, it needs to be specified in each call to ee_pexpr.
        ctx.pop('arg_expr', None)

        full_ctx = dict(ctx, **extra_ctx)
        return emit_paren_expr(expr, **full_ctx)

    expr = unsugar(expr)

    if isinstance(expr, Literal):
        return str(expr.literal).lower()
    elif isinstance(expr, SymbolLiteral):
        return json.dumps(expr.name)
    elif isinstance(expr, PropertyError):
        return "raise PropertyError({})".format(
            repr(expr.message) if expr.message else ""
        )
    elif isinstance(expr, IsA):
        return "{} isa {}".format(
            ee_pexpr(expr.expr),
            "{}".format(" | ".join(type_name(t) for t in expr.astnodes))
        )
    elif isinstance(expr, LogicTrue):
        return "%true"
    elif isinstance(expr, LogicFalse):
        return "%false"
    elif is_a("bind"):

        bind = "bind {} = {};$hl".format(
            ee(expr.expr_0), ee(expr.expr_1)
        )

        return "{{$i$hl{}$hl{}$hl$d}}".format(
            bind, ee(expr.expr_2)
        )
    elif isinstance(expr, Let):
        if isinstance(expr, Block):
            return emit_let(expr)
        else:
            with walker.call('Let'):
                with walker.arg(0):
                    return walker.emit_comments() + emit_let(expr)
    elif isinstance(expr, Map):
        op_name = expr.kind
        args = []
        vars = [expr.element_var]
        if expr.requires_index:
            vars.append(expr.index_var)
        if op_name in ["map", "mapcat"]:
            args.append(emit_lambda(expr.expr, vars))
        elif op_name == "filter":
            args.append(emit_lambda(expr.filter_expr, vars))
        elif op_name == "filter_map":
            args.append(emit_lambda(expr.expr, vars))
            args.append(emit_lambda(expr.filter_expr, vars))
        elif op_name == "take_while":
            args.append(emit_lambda(expr.take_while_expr, vars))

        if overload_coll_name:
            op_name = overload_coll_name
            del ctx['overload_coll_name']

        coll = ee(expr.collection)

        return emit_method_call(coll, op_name, args)

    elif isinstance(expr, Quantifier):
        return emit_method_call(
            ee(expr.collection),
            expr.kind,
            [emit_lambda(expr.expr, [expr.element_var])]
        )

    elif isinstance(expr, Contains):
        return emit_method_call(ee(expr.collection), "contains", [ee(expr.item)])

    elif isinstance(expr, If):
        with walker.call('If'):
            with walker.arg(0):
                coms = walker.emit_comments()
                cond_strn = ee_pexpr(expr.cond)

            res = "{}if {} then {} else {}".format(
                coms,
                cond_strn,
                ee_pexpr(expr._then, arg_expr=1),
                ee_pexpr(expr.else_then, arg_expr=2)
            )
            return res

    elif isinstance(expr, Cond):
        with walker.call('Cond'):
            branches = expr.branches
            res = ""

            for i, b in enumerate(branches):
                # condition
                with walker.arg(i * 2):
                    coms = walker.emit_comments()
                    cond_strn = ee_pexpr(b[0])
                expr_strn = ee_pexpr(b[1], arg_expr=i * 2 + 1)

                res += "{}{} {} then {}$hl".format(
                    coms,
                    "if" if i == 0 else "elif",
                    cond_strn,
                    expr_strn
                )

            with walker.arg(len(branches) * 2):
                coms = walker.emit_comments()
                else_strn = ee_pexpr(expr.else_expr)

            res += "{}else {}".format(coms, else_strn)

            return res

    elif isinstance(expr, IsNull):
        return "{}.is_null".format(ee(expr.expr))

    elif isinstance(expr, Cast):
        return "{}.to[{}]{}".format(
            ee(expr.expr),
            type_name(expr.dest_type),
            "!" if expr.do_raise else "",
        )

    elif isinstance(expr, All):
        return ee(expr.equation_array, overload_coll_name="logic_all")
    elif isinstance(expr, Any):
        return ee(expr.equation_array, overload_coll_name="logic_any")

    elif isinstance(expr, Match):
        with walker.method_call("match"):
            res = ""
            with walker.self_arg():
                coms = walker.emit_comments()
                matched_expr_strn = ee(expr.matched_expr)

            res += "{}match {} {{$i".format(coms, matched_expr_strn)

            for i, (typ, var, e) in enumerate(expr.matchers):
                with walker.arg(i):
                    coms = walker.emit_comments()
                    if coms and i > 0:
                        coms = "$hl" + coms

                    res += "$hl{}case {}{} => {}".format(
                        coms,
                        var_name(var),
                        (" " + sf(": ${type_name(typ)}")) if typ else "",
                        ee(e)
                    )

            res += "$d$hl}"

        return res

    elif isinstance(expr, Eq):
        return "{} = {}".format(ee(expr.lhs), ee(expr.rhs))

    elif isinstance(expr, BinaryBooleanOperator):
        with walker.boolean_binop(expr.kind):
            def emit_bool_op_rec(expr, depth):
                if depth == 2:
                    lhs = emit_paren_expr(expr.lhs, arg_expr=0, **ctx)
                else:
                    lhs = emit_bool_op_rec(expr.lhs, depth - 1)

                return "{} {} {}".format(
                    lhs,
                    expr.kind,
                    emit_paren_expr(expr.rhs, arg_expr=depth - 1, **ctx)
                )

            return emit_bool_op_rec(expr, walker.arg_count())

    elif isinstance(expr, Not):
        return "not {}".format(emit_paren_expr(expr.expr, **ctx))

    elif isinstance(expr, Then):
        if expr.var_expr.source_name is None:
            assert expr.underscore_then
            # Match is like a function call in the Python DSL, but is a regular
            # expression in the new syntax, so we don't want to use the ?
            # syntax on it.
            if not isinstance(expr.then_expr, Match):
                return "{}?{}".format(
                    ee(expr.expr),
                    ee(expr.then_expr, then_underscore_var=expr.var_expr)
                )

        return emit_method_call(
            ee_pexpr(expr.expr),
            "do",
            keep([
                "({}) => {}".format(var_name(expr.var_expr), ee(expr.then_expr)),
                "default_val={}".format(ee_pexpr(expr.default_val))
                if expr.default_val else None
            ])
        )

    elif isinstance(expr, OrderingTest):
        return "{} {} {}".format(
            ee_pexpr(expr.lhs),
            expr.OPERATOR_IMAGE[expr.operator],
            ee_pexpr(expr.rhs)
        )

    elif isinstance(expr, GetSymbol):
        return "{}.symbol".format(ee(expr.node_expr))
    elif is_a("as_entity", "as_bare_entity", "children",
              "env_parent", "rebindings_parent", "parents", "parent", "root",
              "append_rebinding", "concat_rebindings", "env_node",
              "rebindings_new_env", "rebindings_old_env", "get_value",
              "solve", "is_referenced_from", "env_group", "length",
              "can_reach", "as_int", "unique", "env_orphan",
              "is_visible_from", "as_array", "rebind_env"):
        exprs = expr.sub_expressions
        return emit_method_call(ee(exprs[0]), type(expr).__name__,
                                map(ee, exprs[1:]))

    elif isinstance(expr, EnumLiteral):
        return expr.value.dsl_name

    elif isinstance(expr, Try):
        return "try $sl$i{}$sl$d {}".format(
            ee_pexpr(expr.try_expr),
            "or {}".format(ee_pexpr(expr.else_expr)) if expr.else_expr is not None else ""
        )

    elif isinstance(expr, Arithmetic):
        return "{} {} {}".format(ee_pexpr(expr.l), expr.op, ee_pexpr(expr.r))

    elif isinstance(expr, EnvGet):
        args = [ee(expr.symbol)]
        if expr.sequential_from:
            args.append("from={}".format(ee(expr.sequential_from)))
        if expr.only_first:
            args.append("only_first={}".format(ee(expr.only_first)))
        if expr.categories:
            args.append('categories={}'.format(ee(expr.categories)))
        return emit_method_call(ee(expr.env), "get", args)

    elif is_a("at"):
        # Recognize find
        if (isinstance(expr.expr_0, Map) and expr.expr_0.kind == 'filter' and
                ee(expr.expr_1) == "0"):
            return ee(expr.expr_0, overload_coll_name="find")

        return "{}?({})".format(ee(expr.expr_0), ee(expr.expr_1))
    elif is_a("at_or_raise"):
        return "{}({})".format(ee(expr.expr_0), ee(expr.expr_1))
    elif isinstance(expr, FieldAccess):
        args = []
        if expr.arguments:
            for arg in expr.arguments.args:
                args.append(ee(arg))
            for name, arg in expr.arguments.kwargs.items():
                args.append("{}={}".format(name, ee(arg)))

        return emit_method_call(ee(expr.receiver), expr.field, args)

    elif isinstance(expr, Concat):
        return "{} & {}".format(ee_pexpr(expr.array_1), ee_pexpr(expr.array_2))

    elif isinstance(expr, EntityVariable):
        return "self"
    elif isinstance(expr, SelfVariable):
        return "node"
    elif isinstance(expr, DynamicVariable):
        return expr.argument_name.lower
    elif isinstance(expr, AbstractVariable):
        if then_underscore_var:
            if id(then_underscore_var) == id(expr):
                return ""
        return var_name(expr)
    elif isinstance(expr, No):
        return "null".format(type_name(expr.expr_type))
        # TODO: Emit valid null values for other types, eg. [] for arrays.
    elif isinstance(expr, CollectionSingleton):
        if then_underscore_var:
            return emit_method_call(ee(expr.expr), "singleton")
        else:
            return "[{}]".format(ee(expr.expr))
    elif isinstance(expr, New):
        return "{}{}".format(
            type_name(expr.struct_type),
            emit_paren(", ".join(
                "{}={}".format(unparsed_name(k), ee(v))
                for k, v in expr.field_values.items()
            ))
        )
    elif isinstance(expr, StructUpdate):
        return '{}.update({})'.format(
            ee(expr.expr),
            ', '.join('{}={}'.format(name, ee(field_expr))
                      for name, field_expr in sorted(expr.assocs.items()))
        )
    elif isinstance(expr, ArrayLiteral):
        if not len(expr.elements):
            return '[]'
        elif isinstance(expr.elements[0], CharacterLiteral):
            return repr(u"".join(e.literal for e in expr.elements))[1:]
        return "[{}]".format(", ".join(ee(el) for el in expr.elements))
    elif isinstance(expr, CharacterLiteral):
        # Get rid of the 'u' unicode prefix
        return repr(expr.literal)[1:]
    elif isinstance(expr, BigIntLiteral):
        return 'BigInt({})'.format(str(expr.expr)
                                   if isinstance(expr.expr, (int, long)) else
                                   ee(expr.expr))
    elif isinstance(expr, RefCategories):
        return 'RefCats({})'.format(', '.join(
            '{}={}'.format(name, ee(value))
            for name, value in
            list(sorted(expr.cat_map.items())) + [("others", expr.default)]
        ))
    elif isinstance(expr, Predicate):
        return "%predicate({})".format(", ".join(keep([
            fqn(expr.pred_property),
            ee(expr.exprs[0]),
        ] + [ee(e) for e in expr.exprs[1:]])))
    elif is_a("domain"):
        return "%domain({}, {})".format(ee(expr.expr_0), ee(expr.expr_1))
    elif isinstance(expr, Bind):
        return "%eq({})".format(", ".join(keep([
            ee(expr.from_expr), ee(expr.to_expr),
            "eq_prop={}".format(fqn(expr.eq_prop)) if expr.eq_prop else ""
            "conv_prop={}".format(fqn(expr.conv_prop)) if expr.conv_prop else ""
        ])))
    else:
        # raise NotImplementedError(type(expr))
        return repr(expr)


def emit_doc(doc):
    from inspect import cleandoc
    doc = "$hl".join(["## "+ l for l in cleandoc(doc).split("\n")])
    return sf("${doc}")


def emit_prop(prop, walker):
    quals = ""

    if prop.is_public:
        quals += "@export "

    if prop.abstract:
        quals += "abstract "

    if prop.memoized:
        quals += "@memoized "

    args = ", ".join("{} : {}{}".format(
        var_name(arg), type_name(arg.type),
        " = {}".format(emit_expr(arg.abstract_default_value))
        if arg.abstract_default_value else ""
    ) for arg in prop.natural_arguments)

    doc = prop.doc

    res = ""
    if doc:
        res += "$hl{}".format(emit_doc(doc))

    res += "$hl{}fun {} ({}): {}".format(
        quals, prop.original_name.lower, args, type_name(prop.type)
    )

    if prop.expr:
        with walker.property(prop):
            res += " = $sl{}".format(emit_expr(prop.expr, walker=walker))

    return res


def emit_field(field):
    from langkit.compiled_types import BaseField, Field

    if isinstance(field, BaseField):
        return "{}{}{} : {}".format(
            "@abstract " if isinstance(field, Field) and field.abstract else "",
            "@parse_field " if isinstance(field, Field) else "",
            unparsed_name(field._indexing_name), type_name(field.type)
        )
    else:
        raise NotImplementedError()


def type_name(type):
    from langkit.compiled_types import ASTNodeType, resolve_type

    type = resolve_type(type)

    def bases(typ):
        t = typ.base
        while t is not None:
            yield t
            t = t.base

    if isinstance(type, ASTNodeType):
        if (type.is_list_type
                and not any(t.is_generic_list_type for t in bases(type.base))):
            return "ASTList[{}]".format(type_name(type.element_type))
        else:
            return type.raw_name.camel
    elif type.is_array_type:
        return "Array[{}]".format(type_name(type.element_type))
    elif type.is_entity_type:
        return type_name(type.element_type)
    elif type.is_struct_type:
        return type.api_name.camel
    elif type.is_ast_node:
        return "{}.node".format(type.name.camel)
    elif type.name.camel == 'Integer':
        return 'Int'
    elif type.name.camel == 'Boolean':
        return 'Bool'
    elif type.name.camel == 'SymbolType':
        return 'Symbol'
    elif type.name.camel == 'BigIntegerType':
        return 'BigInt'
    else:
        return type.name.camel


def emit_node_type(node_type):

    from langkit.compiled_types import ASTNodeType, CompiledTypeRepo

    walker = DSLWalker.class_from_location(node_type.location)
    base = None
    qual_node = ""
    enum_members = ""
    builtin_properties = []
    abstract_qual = ""
    type_kind = "struct"

    if isinstance(node_type, ASTNodeType):
        if node_type.is_generic_list_type:
            return ""

        base = node_type.base

        if base and base.is_generic_list_type:
            return ""

        qual_node = "@qualifier " if node_type.is_bool_node else ""

        builtin_properties = node_type.builtin_properties()

        abstract_qual = (
            "@root_node " if node_type.is_root_node
            else "@abstract " if node_type.abstract else ""
        )

        type_kind = "class"

        if node_type.is_enum_node:
            abstract_qual = ""
            if not node_type.is_bool_node:
                type_kind = "enum class"
                enum_members = "$i({})$d ".format(
                    ", $sl".join(alt.name.camel
                                 for alt in node_type.alternatives)
                )
    else:
        if node_type.is_entity_type:
            return ""
        elif node_type == CompiledTypeRepo.entity_info:
            return ""
        elif node_type == CompiledTypeRepo.entity_info:
            return ""
        elif node_type == CompiledTypeRepo.env_metadata:
            return ""

    parse_fields = [
        f for f in node_type.get_fields(include_inherited=False)
        if not "[internal]" in f._indexing_name
    ]

    properties = node_type.get_properties(include_inherited=False)
    doc = node_type.doc

    strbase = ": {} ".format(type_name(base)) if base else ""

    def is_builtin_prop(prop):
        return any(
            builtin_name == prop.name.lower
            for builtin_name, _ in builtin_properties
        ) or (prop.original_name.lower == "as_bool" and node_type.is_bool_node)

    if base and base.is_enum_node:
        return ""

    return sf("""
    % if doc:
    ${emit_doc(doc)}$hl
    % endif
    ${abstract_qual}${qual_node}${type_kind} ${type_name(node_type)} ${enum_members}${strbase}{$i$hl
    % for field in parse_fields:
    ${emit_field(field)}$hl
    % endfor
    % for prop in properties:
    % if not prop.is_internal and not is_builtin_prop(prop):
    ${emit_prop(prop, walker)}$hl
    % endif
    % endfor
    $d
    }$hl
    """.strip())

    del base, parse_fields, enum_qual, properties


def format_token_name(name):
    name = name.lower
    if name in ('val', 'if', 'elif', 'else', 'try', 'raise', 'then', 'in',
                'class', 'not', 'and', 'or', 'null', 'generic', 'private',
                'case'):
        return name + "_tok"
    return name


def format_pattern(pat):
    return json.dumps(pat.decode('string_escape'))


def unparse_token_decl(token, newline_afters, is_pre):
    from langkit.lexer import WithSymbol, WithText, WithTrivia

    result = []

    if is_pre:
        result.append('@pre_rule')
    if token in newline_afters:
        result.append('@newline_after')

    options = []
    if token.start_ignore_layout:
        options += ['start_ignore_layout=true']
    if token.end_ignore_layout:
        options += ['end_ignore_layout=true']

    # Check WithText last, as WithTrivia derives from it
    if isinstance(token, WithSymbol):
        kind = 'symbol'
    elif isinstance(token, WithTrivia):
        kind = 'trivia'
    elif isinstance(token, WithText):
        kind = 'text'
    else:
        assert False

    if kind != 'text' or options:
        result.append('@{}({})'.format(kind, ', '.join(options)))
    result.append(format_token_name(token.name))

    return ' '.join(result)


def unparse_lexer_rule_set(newline_afters, rule_set):
    from langkit.lexer import (
        WithSymbol, WithText, WithTrivia, Literal, NoCaseLit, Pattern, Ignore,
        Case
    )

    def unparse_matcher(matcher):
        if isinstance(matcher, NoCaseLit):
            return 'no_case("{}")'.format(matcher.to_match)
        elif isinstance(matcher, Literal):
            return '"{}"'.format(matcher.to_match)
        elif isinstance(matcher, Pattern):
            return "p{}".format(format_pattern(matcher.pattern))
        else:
            assert False

    def unparse_rule_matchers(rule_set):
        if len(rule_set) == 1:
            return unparse_matcher(rule_set[0][0].matcher)
        elif len(rule_set) > 1:
            return "or($i$hl{}$d$hl)".format("$hl".join(
                unparse_matcher(rule.matcher) for rule, _ in rule_set
            ))
        return "bordel"

    def unparse_case_alt(alt):
        send_str = "send({}, {})".format(
            format_token_name(alt.send.name),
            alt.match_size
        )
        if alt.prev_token_cond is not None:
            return "if previous_token isa {} then {}".format(
                " | ".join(
                    format_token_name(cond.name)
                    for cond in alt.prev_token_cond
                ),
                send_str
            )
        else:
            return "else {}".format(send_str)

    def unparse_case_action(matcher, action):
        template = "$hlmatch {} {{$i$hl{}$d$hl}}"
        return template.format(
            unparse_matcher(matcher),
            "$hl".join(unparse_case_alt(alt) for alt in action.all_alts)
        )

    first_rule, is_pre = rule_set[0]

    # all rules in the rule_set have the same action
    action = first_rule.action

    if isinstance(action, Ignore):
        return "{}@ignore _ <- {}".format("@pre_rule " if is_pre else "",
                                          unparse_rule_matchers(rule_set))

    if isinstance(action, Case.CaseAction):
        assert len(rule_set) == 1
        return unparse_case_action(first_rule.matcher, action)

    return '{} <- {}'.format(
        unparse_token_decl(action, newline_afters, is_pre),
        unparse_rule_matchers(rule_set)
    )


def compute_newline_afters(newline_after):
    res = defaultdict(lambda _: False)
    for tok in newline_after:
        res[tok] = True
    return res


def unparse_lexer(ctx, f):
    """
    Unparse the lexer for the current language to the given file.
    """
    from langkit.lexer import TokenAction

    # Prepare unparsing of lexer
    lexer_annotations = "@track_indent$hl" if ctx.lexer.track_indent else ""
    lexer_annotations += "".join(
        '@spacing({}, {})$hl'.format(tf1.name.lower, tf2.name.lower)
        for tf1, m in ctx.lexer.spacing_table.iteritems()
        for tf2, v in m.iteritems()
        if v
    )
    lexer_newline_rule_index = next(
        i
        for i, r in enumerate(ctx.lexer.rules)
        if r.signature == ('RuleAssoc',
                           ('Literal', '\n'),
                           ('WithText', 'Newline', False, False))
    ) if ctx.lexer.track_indent else -1

    newline_afters = compute_newline_afters(ctx.lexer.newline_after)

    # These tokens are implicitly created: do not create explicit declarations
    # for them.
    token_blacklist = {
        ctx.lexer.tokens.Termination,
        ctx.lexer.tokens.LexingFailure,
    }
    if ctx.lexer.track_indent:
        token_blacklist.update({
            ctx.lexer.tokens.Indent,
            ctx.lexer.tokens.Dedent,
            ctx.lexer.tokens.Newline,
        })

    # Mapping from tokens to their owning families
    token_to_family = {t: f
                       for f in ctx.lexer.tokens.token_families
                       for t in f.tokens}

    # Set of tokens created by at lesat one simple rules (i.e. not with cases)
    simple_rule_tokens = set()

    rule_sets = []
    i = 0
    while i < len(ctx.lexer.rules):
        cur_set = []
        cur_action = ctx.lexer.rules[i].action

        while (i < len(ctx.lexer.rules)
               and ctx.lexer.rules[i].action == cur_action):

            # If this is a simple token production rule, keep a note about this
            # token.
            rule = ctx.lexer.rules[i]
            if isinstance(rule.action, TokenAction):
                simple_rule_tokens.add(rule.action)

            if i < lexer_newline_rule_index:
                cur_set.append((ctx.lexer.rules[i], True))
            elif i > lexer_newline_rule_index:
                cur_set.append((ctx.lexer.rules[i], False))
            i += 1

        if len(cur_set) > 0:
            rule_sets.append(cur_set)

    result = []
    last_family = [None]

    def open_family(family):
        if last_family[0] == family:
            return

        if last_family[0]:
            close_family()

        # Keep the token family implicit
        if family and family.name.lower != 'default_family':
            result.append('$hl$hlfamily {} {{$i'
                          .format(family.name.lower))
            last_family[0] = family

    def close_family():
        if last_family[0]:
            result.append("$d$hl}$hl")
        last_family[0] = None

    # Emit stub declarations for tokens that are never produced by simple
    # lexing rules.
    for t in sorted(set(ctx.lexer.tokens.tokens) - simple_rule_tokens,
                    key=lambda t: t.name):
        # Skip automatically created tokens
        if t in token_blacklist:
            continue
        open_family(token_to_family.get(t))
        result.append('$hl' +
                      unparse_token_decl(t, newline_afters, is_pre=False))

    # Emit lexing rules
    for rule_set in rule_sets:
        tok = rule_set[0][0].action
        family = token_to_family.get(tok)
        unparsed_rule = unparse_lexer_rule_set(newline_afters, rule_set)

        open_family(token_to_family.get(tok))
        result.append("$hl" + unparsed_rule)
    close_family()

    unparsed_rules = '\n'.join(result)
    template = """
    ${lexer_annotations}lexer ${ctx.lang_name.lower}_lexer {$i$hl
    % for name, pattern, loc in ctx.lexer.patterns:
        val ${name} = p${format_pattern(pattern)}$hl
    % endfor

    ${unparsed_rules}

    $d$hl
    }$hl
    """

    f.write(pp(sf(template)))


def unparse_grammar(ctx, f):
    """
    Unparse the grammar for the current language to the given file.
    """
    sorted_rules = sorted(
        ((name, rule)
         for name, rule in ctx.grammar.rules.items()
         if not rule.is_dont_skip_parser),
        key=lambda (_, rule): rule._id
    )

    template = """
    grammar ${ctx.lang_name.lower}_grammar {$i$hl
    % for name, rule in sorted_rules:
        ${('@main_rule '
           if name == ctx.grammar.main_rule_name
           else '')}${name} <- ${emit_rule(rule, True)}$hl
    % endfor
    $d$hl
    }$hl
    """

    f.write(pp(sf(template)))


def unparse_nodes(ctx, f):
    """
    Unparse the nodes for the current language to the given file.
    """
    from langkit.diagnostics import check_source_language, Severity
    check_source_language(
        predicate=lpl is not None,
        message="libpythonlang not found, comments cannot be unparsed",
        severity=Severity.warning,
        do_raise=False
    )

    types = keep(emit_node_type(t)
                 for t in ctx.astnode_types + ctx._struct_types)

    template = """
    % for t in types:
        $hl
        ${t}
    % endfor
    """
    f.write(pp(sf(template)))


def unparse_lang(ctx):
    """
    Unparse the language currently being compiled.
    """
    # If there is no unparse script, then the pass does nothing
    if not ctx.emitter.unparse_script:
        return
    unparse_script = ctx.emitter.unparse_script

    # By setting the context's emitter to `None`, we disable the sanity checks
    # done to ensure that we don't intermix codegen and semantic checks in the
    # langkit code generator, because we break that invariant in the unparser.
    emitter = ctx.emitter
    ctx.emitter = None

    f = None
    try:
        for action, value in unparse_script.actions:
            if action == 'to':
                if f is not None:
                    f.close()
                f = open(value, 'w')
            elif action == 'import':
                f.write('import {}\n'.format(value))
            elif action == 'nodes':
                unparse_nodes(ctx, f)
            elif action == 'lexer':
                unparse_lexer(ctx, f)
            elif action == 'grammar':
                unparse_grammar(ctx, f)
            else:
                assert False
    finally:
        if f is not None:
            f.close()

    ctx.emitter = emitter


def create():
    return GlobalPass('Unparse language definition', unparse_lang)


def pp(strn, indent_step=4, line_size=80):
    from cStringIO import StringIO
    import re
    buf = re.split("(\$hl|\$sl|\$i|\$d)", strn)
    file_str = StringIO()
    indent_level = 0
    current_line = ""

    def write_line():
        file_str.write(current_line.rstrip() + "\n")

    for i in range(len(buf)):
        el = buf[i].replace("\n", "")

        if el in ['$hl', '$sl']:

            len_next_line = 0
            if el == '$sl':
                for j in range(i + 1, len(buf)):
                    if buf[j] == '$hl':
                        break
                    len_next_line += len(buf[j])

            if el == '$hl' or len(current_line) + len_next_line > line_size:
                write_line()
                current_line = " " * indent_level
        elif el == '$i':
            indent_level += indent_step
        elif el == '$d':
            indent_level -= indent_step
            # Reset current line if there is only whitespace on it
            if current_line.strip() == "":
                current_line = " " * indent_level
        else:
            current_line += el
    if current_line:
        write_line()
    return file_str.getvalue()
