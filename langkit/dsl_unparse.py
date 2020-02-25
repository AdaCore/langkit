from __future__ import absolute_import, division, print_function

from funcy import keep
import json

from langkit.passes import GlobalPass
from contextlib import contextmanager

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
            return n.is_a(lpl.CallExpr) and n.f_prefix.text == fun_name

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
        If the cursor is on a function/method call, moves the cursor to the
        ``index``th argument of the call.

        .. note:: is a context manager.

        :type index: int
        """
        assert self.current_node.is_a(lpl.CallExpr)
        return self._with_current_node(
            self.current_node.f_suffix[index].f_expr
        )

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


def emit_rule(rule, top_level=False):
    from langkit.parsers import (
        _Transform, node_name, _Row, Opt, List, Or, _Token, NoBacktrack,
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
        r = emit_rule(rule.parser)
        if top_level:
            return "({})".format(r)
        return r
    elif isinstance(rule, List):
        sep_str = ", {}".format(emit_rule(rule.sep)) if rule.sep else ""
        return "list{}({}{})".format(
            '*' if rule.empty_valid else '+', emit_rule(rule.parser), sep_str
        )
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
                rule._val.name.camel,
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
        strn = emit_expr(expr, **ctx)
        return emit_paren(strn) if len(strn) > 40 else strn
    elif isinstance(expr, Let):
        return emit_expr(expr, **ctx)
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
        return "{} {} {}".format(
            emit_paren_expr(expr.lhs),
            expr.kind, emit_paren_expr(expr.rhs)
        )

    elif isinstance(expr, Not):
        return "not {}".format(emit_paren_expr(expr.expr))

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

        return res

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
    elif type.name.camel == 'BigIntegerType':
        return 'BigInt'
    else:
        return type.name.camel


def emit_node_type(node_type):

    from langkit.compiled_types import ASTNodeType, CompiledTypeRepo

    walker = DSLWalker.class_from_location(node_type.location)
    base = None
    enum_qual = ""
    builtin_properties = []
    abstract_qual = ""
    type_kind = "struct"

    if isinstance(node_type, ASTNodeType):
        if node_type.is_generic_list_type:
            return ""

        base = node_type.base

        if base and base.is_generic_list_type:
            return ""

        enum_qual = (
            "@qualifier " if node_type.is_bool_node
            else "@enum_node$i({}) $d".format(
                ", $sl".join(alt.name.camel for alt in node_type.alternatives)
            )
            if node_type.is_enum_node else ""
        )

        builtin_properties = node_type.builtin_properties()

        abstract_qual = (
            "@root_node " if node_type.is_root_node
            else "@abstract " if node_type.abstract else ""
        )

        type_kind = "class"
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

    def is_builtin_prop(prop):
        return any(
            builtin_name == prop.name.lower
            for builtin_name, _ in builtin_properties
        )

    if base and base.is_enum_node:
        return ""

    return sf("""
    % if doc:
    ${emit_doc(doc)}$hl
    % endif
    % if base:
    ${abstract_qual}${enum_qual}${type_kind} ${type_name(node_type)} : ${type_name(base)} {$i$hl
    % else:
    ${abstract_qual}${enum_qual}${type_kind} ${type_name(node_type)} {$i$hl
    % endif
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


def unparse_lang(ctx):
    """
    Unparse the language currently being compiled.
    """

    dest_file = ctx.emitter.unparse_destination_file

    # If there is no destination file, then the pass does nothing.
    if not ctx.emitter.unparse_destination_file:
        return

    # By setting the context's emitter to `None`, we disable the sanity checks
    # done to ensure that we don't intermix codegen and semantic checks in the
    # langkit code generator, because we break that invariant in the unparser.
    emitter = ctx.emitter
    ctx.emitter = None

    sorted_rules = sorted(
        ((name, rule)
         for name, rule in ctx.grammar.rules.items()
         if not rule.is_dont_skip_parser),
        key=lambda (_, rule): rule._id
    )

    template = """
    grammar ${ctx.lang_name}Grammar {$i$hl
    % for name, rule in sorted_rules:
        ${('@main_rule '
           if name == ctx.grammar.main_rule_name
           else '')}${name} <- ${emit_rule(rule, True)}$hl
    % endfor
    $d$hl
    }$hl

    <% types = keep(emit_node_type(t) for t in ctx.astnode_types + ctx._struct_types) %>

    % for t in types:
        $hl
        ${t}
    % endfor
    """

    from langkit.diagnostics import check_source_language, Severity
    check_source_language(
        predicate=lpl is not None,
        message="libpythonlang not found, comments cannot be unparsed",
        severity=Severity.warning,
        do_raise=False
    )

    lang_def = pp(sf(template))
    with open(dest_file, 'w') as f:
        f.write(lang_def)

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
