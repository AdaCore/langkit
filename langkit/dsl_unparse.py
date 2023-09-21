from collections import defaultdict
from contextlib import contextmanager
import json
import sys
from typing import Dict

from funcy import keep, lmap

from langkit.diagnostics import print_error, Location
from langkit.passes import GlobalPass
from langkit import names

libpythonlang_available = True
try:
    import libpythonlang as lpl
except ImportError:
    libpythonlang_available = False

templates: Dict[str, str] = {}


def fqn(prop):
    return "{}.{}".format(prop.struct.name.camel, prop._original_name)


class DSLWalker:
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
    if libpythonlang_available:
        ctx = lpl.AnalysisContext()

    class NoOpWalker:
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
        if not libpythonlang_available or loc is None:
            return DSLWalker.NoOpWalker()

        unit = DSLWalker.ctx.get_from_file(loc.file)

        if unit.diagnostics:
            for diag in unit.diagnostics:
                print_error(diag.message,
                            Location.from_sloc_range(unit, diag.sloc_range))
            sys.exit(1)

        class_def = unit.root.lookup(lpl.Sloc(loc.line, 99))

        # If there is a decorator in place of the class def, go get the class
        # def in the tree.
        if class_def.is_a(lpl.Decorator):
            class_def = class_def.parent.parent[1]
        assert class_def.is_a(lpl.ClassDef)

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

    def first_method_call(self, dispatch_table):
        """
        Given a dictionnary of method name to handling function, moves the
        cursor to the first method call matching one of the names defined in
        the dictionnary and calls its associated handling function.
        """
        def matches(n):
            if n.is_a(lpl.CallExpr):
                if n.f_prefix.is_a(lpl.DottedName):
                    return n.f_prefix.f_suffix.text in dispatch_table
            return False

        call_node = (
            self.current_node
            if matches(self.current_node)
            else self.current_node.find(matches)
        )

        if call_node is not None:
            with self._with_current_node(call_node):
                return dispatch_table[call_node.f_prefix.f_suffix.text]()

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

    def is_call_to(self, names):
        """
        Return whether the cursor is currently on a call to a function of one
        of the given names.

        :type names: list[str]
        """
        return (self.current_node.is_a(lpl.CallExpr)
                and self.current_node.f_prefix.text in names)

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

    def arg_keyword(self, index):
        """
        If the cursor is on a function/method call, return the keyword name
        used in the argument assocation corresponding to the ``index`` th
        argument passed to that call.

        :type index: int
        :rtype: str
        """
        assert self.current_node.is_a(lpl.CallExpr)
        kw = self.current_node.f_suffix[index].f_name
        return kw.text if kw is not None else None

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

    :param str strn: The string to format, containing regular Python
        expressions in ${} blocks.
    :rtype: str
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


def root_list_name(node):
    from langkit.compiled_types import T
    assert node.is_root_list_type
    return f"ASTList[{node_name(T.root_node)}, {node_name(node.element_type)}]"


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

    return root_list_name(node) if node.is_root_list_type else node.dsl_name


def emit_rule(rule, top_level=False):
    from langkit.parsers import (
        _Transform, _Row, Opt, List, Or, _Token, Cut, StopCut,
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
    elif isinstance(rule, Cut):
        return "/"
    elif isinstance(rule, StopCut):
        return "stop_cut({})".format(emit_rule(rule.parser))
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


def unparsed_name(strn_or_name, suffix="_var"):
    if isinstance(strn_or_name, names.Name):
        strn = strn_or_name.lower
    else:
        strn = strn_or_name

    # Special cases: some variable names are keywords. Rename them.
    if strn == "val":
        return "value"
    elif strn in ('if', 'is', 'elif', 'else', 'try', 'raise', 'then', 'in',
                  'class', 'not', 'and', 'or', 'null', 'generic', 'private',
                  'case', 'match'):
        return strn + suffix
    else:
        return strn


def var_name(var_expr, default="_"):
    from langkit.compiled_types import Argument
    if isinstance(var_expr, Argument):
        ret = var_expr.name.lower
    else:
        ret = var_expr.source_name or default

    return unparsed_name(ret)


def expr_is_a(expr, *names):
    return any(expr.__class__.__name__ == n for n in names)


def needs_parens(expr):
    from langkit.expressions import (FieldAccess, Literal, AbstractVariable,
                                     BigIntLiteral, Map, Quantifier, EnvGet,
                                     Super)
    return not (
        isinstance(expr, (FieldAccess, Literal, AbstractVariable, BigIntLiteral, EnvGet,
                          Map, Quantifier, Super, int))
        or expr_is_a(expr, "as_entity", "as_bare_entity", "children",
              "env_parent", "rebindings_parent", "parents", "parent", "root",
              "append_rebinding", "concat_rebindings", "env_node",
              "rebindings_new_env", "rebindings_old_env", "get_value",
              "solve", "is_referenced_from", "env_group", "length",
              "can_reach", "as_int", "unique", "env_orphan",
              "is_visible_from", "as_array", "rebind_env")
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
        ArrayLiteral, Arithmetic, BaseRaiseException, CharacterLiteral,
        Predicate, StructUpdate, BigIntLiteral, RefCategories, Bind, Try,
        Block, Contains, PropertyDef, DynamicLexicalEnv, Super, Join, String,
        NPropagate, Find
    )

    def is_a(*names):
        return any(expr.__class__.__name__ == n for n in names)


    then_underscore_var = ctx.get('then_underscore_var')
    walker = ctx.get('walker')

    def emit_lambda(expr, vars):
        vars_str = ", ".join(var_name(var) for var in vars)
        return "({}) => {}".format(vars_str, ee(expr))
        del vars_str

    def emit_method_call(receiver, name, args=[],
                         force_parens=True, as_multiline=False):
        return "{}.{}{}".format(
            receiver, name,
            emit_paren(", ".join(args), as_multiline)
            if force_parens or args else ""
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
        return "s" + json.dumps(expr.name)

    elif isinstance(expr, BaseRaiseException):
        return "raise[{}] {}({})".format(
            type_name(expr.expr_type),
            expr.exc_name.camel,
            json.dumps(expr.message) if expr.message else ""
        )

    elif isinstance(expr, IsA):
        return "{} is {}".format(
            ee_pexpr(expr.expr),
            "{}".format(
                " | ".join(
                    type_name(t, strip_entity=True) for t in expr.astnodes
                )
            ),
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

        coll = None

        def emit_self():
            nonlocal coll
            with walker.self_arg():
                coll = ee(expr.collection)

        def handle_map():
            emit_self()
            with walker.arg(0):
                args.append(emit_lambda(expr.expr, vars))

        def handle_filter():
            emit_self()
            with walker.arg(0):
                args.append(emit_lambda(expr.filter_expr, vars))

        def handle_filtermap():
            emit_self()
            with walker.arg(0):
                args.append(emit_lambda(expr.expr, vars))
            with walker.arg(walker.arg_count() - 1):
                args.append(emit_lambda(expr.filter_expr, vars))

        def handle_keep():
            emit_self()
            with walker.arg(0):
                args.append(emit_lambda(expr.expr, vars))
                args.append(emit_lambda(expr.filter_expr, vars))

        def handle_take_while():
            emit_self()
            with walker.arg(0):
                args.append(emit_lambda(expr.take_while_expr, vars))

        def set_op_name_and_then(name, fun):
            def do():
                nonlocal op_name
                op_name = name
                return fun()
            return do

        raw_dispatch_table = {
            'map':        handle_map,
            'mapcat':     handle_map,
            'logic_all':  handle_map,
            'logic_any':  handle_map,
            'filter':     handle_filter,
            'filtermap':  handle_filtermap,
            'take_while': handle_take_while
        }

        dispatch_table = {
            k: set_op_name_and_then(k, v)
            for k, v in raw_dispatch_table.items()
        }
        dispatch_table['keep'] = set_op_name_and_then(
            'filtermap', handle_keep
        )

        walker.first_method_call(dispatch_table)

        return emit_method_call(coll, op_name, args)

    elif isinstance(expr, Quantifier):
        return emit_method_call(
            ee(expr.collection),
            expr.kind,
            [emit_lambda(expr.expr, [expr.element_var])]
        )

    elif isinstance(expr, Contains):
        return emit_method_call(ee(expr.collection), "contains", [ee(expr.item)])

    elif isinstance(expr, Find):
        return emit_method_call(
            ee(expr.collection),
            "find",
            [emit_lambda(expr.expr, [expr.element_var])],
        )

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
        return "{}.as[{}]{}".format(
            ee(expr._expr),
            type_name(expr.dest_type, strip_entity=True),
            "!" if expr.do_raise else "",
        )

    elif isinstance(expr, All):
        return ee(expr.equation_array)

    elif isinstance(expr, Any):
        return ee(expr.equation_array)

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
                        sf(": ${type_name(typ)}") if typ else "",
                        ee(e)
                    )

            res += "$d$hl}"

        return res

    elif isinstance(expr, Eq):
        return "{} == {}".format(ee(expr.lhs), ee(expr.rhs))

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
        if isinstance(expr.expr, Eq):
            return "{} != {}".format(ee(expr.expr.lhs), ee(expr.expr.rhs))
        return "not {}".format(emit_paren_expr(expr.expr, **ctx))

    elif isinstance(expr, Then):
        def emit_final_call(self_expr, param_name, then_expr, default_expr):
            return emit_method_call(
                self_expr,
                "do",
                keep([
                    "({}) => {}".format(param_name, then_expr),
                    "default_val={}".format(default_expr)
                    if default_expr else None
                ])
            )

        if expr.var_expr.source_name is None:
            assert expr.underscore_then
            # Match is like a function call in the Python DSL, but is a regular
            # expression in the new syntax, so we don't want to use the ?
            # syntax on it. Likewise for IsA.
            if not isinstance(expr.then_expr, (Match, IsA)):
                # If the "then" expression also implies a "?", do not emit it
                # twice.
                fmt = '{}{}' if expr_is_a(expr.then_expr, 'at') else '{}?{}'

                return fmt.format(
                    ee(expr.expr),
                    ee(expr.then_expr, then_underscore_var=expr.var_expr)
                )
            else:
                return emit_final_call(
                    ee_pexpr(expr.expr),
                    var_name(expr.var_expr),
                    ee(expr.then_expr),
                    ee_pexpr(expr.default_val)
                    if expr.default_val else None
                )

        def handle_then_call():
            with walker.self_arg():
                self_expr = ee_pexpr(expr.expr)

            with walker.arg(0):
                then_expr = ee(expr.then_expr)

            if expr.default_val is not None:
                with walker.arg(1):
                    default_expr = ee_pexpr(expr.default_val)
            else:
                default_expr = None

            return emit_final_call(
                self_expr, var_name(expr.var_expr), then_expr, default_expr
            )

        def handle_or_call():
            with walker.self_arg():
                self_expr = ee_pexpr(expr.expr)

            with walker.arg(0):
                fallback_expr = ee_pexpr(expr.default_val)

            return emit_final_call(
                self_expr, "e", "e", fallback_expr
            )

        def handle_designated_env_macro():
            self_expr = ee_pexpr(expr.expr)

            then_expr = ee(expr.then_expr)

            if expr.default_val is not None:
                default_expr = ee_pexpr(expr.default_val)
            else:
                default_expr = None

            return emit_final_call(
                self_expr, var_name(expr.var_expr), then_expr, default_expr
            )

        if walker.is_call_to(["named_env", "direct_env"]):
            return handle_designated_env_macro()

        return walker.first_method_call({
            'then': handle_then_call,
            '_or': handle_or_call
        })

    elif isinstance(expr, OrderingTest):
        return "{} {} {}".format(
            ee_pexpr(expr.lhs),
            expr.OPERATOR_IMAGE[expr.operator],
            ee_pexpr(expr.rhs)
        )

    elif isinstance(expr, GetSymbol):
        return "{}.symbol".format(ee(expr.node_expr))

    elif is_a("as_entity", "as_bare_entity", "children", "env_parent",
              "rebindings_parent", "parents", "parent", "root", "env_node",
              "rebindings_new_env", "rebindings_old_env"):
        # Field like expressions
        exprs = expr.sub_expressions
        return emit_method_call(ee(exprs[0]), type(expr).__name__,
                                lmap(ee, exprs[1:]), False)

    elif is_a("append_rebinding", "concat_rebindings", "env_node", "get_value",
              "solve", "is_referenced_from", "env_group", "length", "can_reach",
              "as_int", "unique", "env_orphan", "is_visible_from", "as_array",
              "rebind_env"):
        # Method like expressions
        exprs = expr.sub_expressions
        return emit_method_call(ee(exprs[0]), type(expr).__name__,
                                lmap(ee, exprs[1:]))

    elif isinstance(expr, EnumLiteral):
        return expr.value.dsl_name

    elif isinstance(expr, Try):
        return "try $sl$i{}$sl$d {}".format(
            ee_pexpr(expr.try_expr),
            "else {}".format(ee_pexpr(expr.else_expr)) if expr.else_expr is not None else ""
        )

    elif isinstance(expr, Arithmetic):
        return "{} {} {}".format(ee_pexpr(expr.l), expr.op, ee_pexpr(expr.r))

    elif isinstance(expr, EnvGet):
        args = [ee(expr.symbol)]
        if expr.sequential_from:
            args.append("from={}".format(ee(expr.sequential_from)))
        if expr.categories:
            args.append('categories={}'.format(ee(expr.categories)))
        return emit_method_call(
            ee(expr.env),
            "get_first" if expr.only_first else "get",
            args
        )

    elif is_a("at"):
        # Recognize find
        if (isinstance(expr.expr_0, Map) and expr.expr_0.kind == 'filter' and
                ee(expr.expr_1) == "0"):
            return ee(expr.expr_0)

        return "{}?({})".format(ee(expr.expr_0), ee(expr.expr_1))

    elif is_a("at_or_raise"):
        return "{}({})".format(ee(expr.expr_0), ee(expr.expr_1))

    elif isinstance(expr, FieldAccess):
        args = []
        has_any_commented_arg = False
        is_property = isinstance(expr.constructed_node_data, PropertyDef)
        if expr.arguments:
            with walker.method_call(expr.field):
                field_coms = walker.emit_comments()
                receiver_str = ee(expr.receiver)

                for i in range(walker.arg_count()):
                    kw = walker.arg_keyword(i)
                    with walker.arg(i):
                        arg_coms = walker.emit_comments()
                        if kw is None:
                            args.append(arg_coms + ee(expr.arguments.args[i]))
                        else:
                            args.append(arg_coms + "{}={}".format(
                                kw, ee(expr.arguments.kwargs[kw])
                            ))
                        has_any_commented_arg |= arg_coms != ""
        else:
            field_coms = ""
            receiver_str = ee(expr.receiver)

        return field_coms + emit_method_call(
            receiver_str,
            expr.field,
            args,
            as_multiline=has_any_commented_arg,
            force_parens=is_property
        )

    elif isinstance(expr, Super):
        args = []
        for arg in expr.arguments.args:
            args.append(ee(arg))
        for kw, arg in expr.arguments.kwargs.items():
            args.append("{}={}".format(kw, ee(arg)))
        return "super({})".format(", ".join(args))

    elif isinstance(expr, Concat):
        result = ""
        with walker.method_call("concat"):
            with walker.self_arg():
                result += ee_pexpr(expr.array_1)
            result += " & "
            with walker.arg(0):
                result += ee_pexpr(expr.array_2)
        return result

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
        return "null[{}]".format(type_name(expr.expr_type))
        # TODO: Emit valid null values for other types, eg. [] for arrays.

    elif isinstance(expr, CollectionSingleton):
        if then_underscore_var:
            return emit_method_call(ee(expr.expr), "singleton")
        else:
            return "[{}]".format(ee(expr.expr))

    elif isinstance(expr, New):
        # The order of arguments in the source is lost during the call to New's
        # constructor. Sort arguments by field declaration order to get the
        # most likely order.
        fields = sorted(expr.struct_type.required_fields_in_exprs.items(),
                        key=lambda assoc: assoc[1]._serial)
        field_exprs = [(name, expr.field_values[name])
                       for name, _ in fields
                       if name in expr.field_values]

        return "{}{}".format(
            type_name(expr.struct_type),
            emit_paren(", ".join("{}={}".format(unparsed_name(k), ee(v))
                                 for k, v in field_exprs))
        )

    elif isinstance(expr, StructUpdate):
        return '{}.update({})'.format(
            ee(expr.expr),
            ', '.join('{}={}'.format(name, ee(field_expr))
                      for name, field_expr in sorted(expr.assocs.items()))
        )

    elif isinstance(expr, ArrayLiteral):
        if not len(expr.elements):
            return '[]: {}'.format(type_name(expr.element_type))
        return "[{}]".format(", ".join(ee(el) for el in expr.elements))

    elif isinstance(expr, CharacterLiteral):
        return repr(expr.value)

    elif isinstance(expr, String):
        return json.dumps(expr.value)

    elif isinstance(expr, BigIntLiteral):
        if isinstance(expr.expr, int):
            return f'{expr.expr}b'
        else:
            return f'{ee_pexpr(expr.expr)}.as_big_int()'
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
            "conv_prop={}".format(fqn(expr.conv_prop)) if expr.conv_prop else ""
        ])))

    elif isinstance(expr, NPropagate):
        return "%propagate({})".format(", ".join(keep([
            ee(expr.dest_var),
            fqn(expr.comb_prop),
        ] + [ee(v) for v in expr.arg_vars])))

    elif isinstance(expr, DynamicLexicalEnv):
        return "DynamicLexicalEnv({})".format(", ".join(keep([
            fqn(expr.assocs_getter),
            f"assoc_resolver={fqn(expr.assoc_resolver)}"
            if expr.assoc_resolver else '',
            "transitive_parent={}".format(ee(expr.transitive_parent))
            if not expr.transitive_parent else '',
        ])))

    elif is_a("to_symbol"):
        return "{}.to_symbol".format(ee(expr.expr_0))

    elif isinstance(expr, Join):
        return "{}.join({})".format(ee(expr.separator), ee(expr.strings))

    else:
        # raise NotImplementedError(type(expr))
        return repr(expr)


def emit_doc(doc):
    from inspect import cleandoc
    doc = "$hl".join(["## "+ l for l in cleandoc(doc).split("\n")])
    return sf("${doc}")


def emit_prop(prop, walker):
    quals = ""

    # When a property declared in the DSL happens to be the root of a property
    # dispatching tree, it is hidden by an artificial property (the
    # "dispatcher", see CompileCtx.lower_properties_dispatching). The actual
    # "is_public" and abstract expression for this root property are still
    # attached to the dispatcher.
    assert not prop.is_artificial_dispatcher
    prop_for_walker = (prop.dispatcher
                       if prop.is_dispatching_root else
                       prop)

    if prop_for_walker.is_public:
        quals += "@export "

    if prop.external:
        quals += "@external "
        if prop.uses_entity_info:
            quals += "@uses_entity_info "
        if prop.uses_envs:
            quals += "@uses_envs "
    elif not prop.constructed_expr and not prop.abstract_runtime_check:
        quals += "@abstract "

    if prop.memoized:
        quals += "@memoized "
    elif prop.lazy_field:
        quals += "@lazy "

    if prop.activate_tracing:
        quals += "@trace "

    if prop.dynamic_vars:
        vars = []
        for dynvar in prop.dynamic_vars:
            name = dynvar.dsl_name
            val = prop.dynamic_var_default_value(dynvar)
            vars.append(
                name
                if val is None else
                "{}={}".format(name, emit_expr(val))
            )
        quals += "@with_dynvars({}) ".format(", ".join(vars))

    args = ", ".join("{}: {}{}".format(
        var_name(arg), type_name(arg.type),
        " = {}".format(emit_expr(arg.abstract_default_value))
        if arg.abstract_default_value else ""
    ) for arg in prop.natural_arguments)

    doc = prop.doc

    res = ""
    if doc:
        res += "$hl{}".format(emit_doc(doc))

    if prop.lazy_field:
        res += "$hl{}{}: {}".format(
            quals, prop.original_name, type_name(prop.type)
        )
    else:
        res += "$hl{}fun {}({}): {}".format(
            quals, prop.original_name, args, type_name(prop.type)
        )

    if prop.abstract_runtime_check:
        # TODO: include the dynamic type of Self in the error message ("not
        # implemented on type XXX"). We probably need to extend the DSL for
        # this.
        res += (
            f" = raise[{type_name(prop.type)}]"
            f" PropertyError(\"Property {prop.qualname} not implemented\")"
        )
    elif prop_for_walker.expr:
        with walker.property(prop_for_walker):
            res += " = $sl{}".format(emit_expr(prop_for_walker.expr,
                                               walker=walker))

    return res


def emit_field(field):
    from langkit.compiled_types import (
        BaseField, Field, MetadataField, UserField
    )

    if isinstance(field, BaseField):
        result = "{}{}{}{}{}{}: {}".format(
            "@abstract " if isinstance(field, Field) and field.abstract else "",
            "@parse_field " if isinstance(field, Field) else "",
            "@null_field " if field.null else "",
            "@nullable " if (
                isinstance(field, Field)
                and not field.is_overriding
                and field.nullable
            ) else "",
            "@use_in_equality " if (
                isinstance(field, MetadataField)
                and field.use_in_equality
            ) else "",
            unparsed_name(field._indexing_name), type_name(field.type)
        )
        if (
            isinstance(field, UserField)
            and field.abstract_default_value is not None
        ):
            result += " = {}".format(emit_expr(field.abstract_default_value))
        return result
    else:
        raise NotImplementedError()


def type_name(type, strip_entity=False):
    from langkit.compiled_types import ASTNodeType, resolve_type
    from langkit.compile_context import get_context

    type = resolve_type(type)

    if strip_entity and type.is_entity_type:
        type = type.element_type

    def bases(typ):
        t = typ.base
        while t is not None:
            yield t
            t = t.base

    if isinstance(type, ASTNodeType):
        return node_name(type)
    elif type.is_character_type:
        return "Char"
    elif type.is_string_type:
        return "String"
    elif type.is_array_type:
        return "Array[{}]".format(type_name(type.element_type))
    elif type.is_iterator_type:
        return "Iterator[{}]".format(type_name(type.element_type))
    elif type.is_entity_type:
        return "Entity[{}]".format(type_name(type.element_type))
    elif type.is_struct_type:
        return type.api_name.camel
    elif type.is_ast_node:
        return "{}.node".format(type.dsl_name)
    elif type.is_lexical_env_type or type.is_analysis_unit_type:
        return "{}[{}]".format(type.dsl_name,
                               type_name(get_context().root_grammar_class))

        return "".format
    else:
        return type.dsl_name


def emit_node_type(node_type):

    from langkit.compiled_types import ASTNodeType, CompiledTypeRepo, T

    walker = DSLWalker.class_from_location(node_type.location)
    base = None
    enum_members = ""
    builtin_properties = []
    traits = []
    quals = []
    token_node = ''
    type_kind = "struct"
    base_name = ""

    if isinstance(node_type, ASTNodeType):
        if node_type.is_generic_list_type:
            return ""

        base = node_type.base
        base_name = type_name(base) if base else ""

        if base and base.is_generic_list_type:
            return ""

        if node_type.is_bool_node:
            quals.append("qualifier")

        builtin_properties = node_type.builtin_properties()

        if node_type.is_root_node:
            traits.append(f'Node[{type_name(node_type)}]')

        if node_type.abstract and not node_type.is_enum_node:
            quals.append("abstract")

        if node_type.is_token_node:
            traits.append('TokenNode')

        if node_type.is_error_node:
            traits.append('ErrorNode')

        if node_type.has_abstract_list:
            quals.append("has_abstract_list")

        type_kind = "class"

        if node_type.is_enum_node:
            type_kind = "enum class"
            if not node_type.is_bool_node:
                enum_members = (
                    ", $sl".join(alt.name.camel
                                 for alt in node_type.alternatives)
                )
    else:
        if node_type.is_entity_type:
            return ""
        elif node_type in (CompiledTypeRepo.entity_info,
                           T.env_assoc,
                           T.inner_env_assoc):
            return ""
        elif (
            node_type == CompiledTypeRepo.env_metadata
            and node_type.location is None
        ):
            return ""

        if node_type == T.env_md:
            quals.append("metadata")

    parse_fields = [
        f for f in node_type.get_fields(include_inherited=False)
        if not "[internal]" in f._indexing_name
    ]

    properties = node_type.get_properties(include_inherited=False)
    doc = node_type._doc

    strbase = ": {} ".format(base_name) if base_name else ""
    strtraits = "implements {} ".format(", ".join(traits)) if traits else ""

    def is_builtin_prop(prop):
        return any(
            builtin_name == prop.name.lower
            for builtin_name, _ in builtin_properties
        ) or (prop.original_name == "as_bool" and node_type.is_bool_node)

    def to_emit_dispatcher(prop):
        return prop.is_dispatcher and prop.is_artificial_dispatcher

    if base and base.is_enum_node:
        return ""

    return sf("""
    % if doc:
    ${emit_doc(doc)}$hl
    % endif
    ${''.join(f'@{q} ' for q in quals)}
    ${type_kind} ${type_name(node_type)} ${strbase}${strtraits}{$i$hl
    % if enum_members:
    case ${enum_members}
    $hl
    % endif
    % for field in parse_fields:
    ${emit_field(field)}$hl
    % endfor
    % for prop in properties:
    % if not prop.is_internal and not is_builtin_prop(prop) and not prop.is_artificial_dispatcher and not prop.artificial:
    ${emit_prop(prop, walker)}$hl
    % endif
    % endfor
    $d
    }$hl
    """.strip())

    del base, parse_fields, enum_qual, properties


def emit_enum_type(enum_type):
    literals = ", ".join(l.name.lower for l in enum_type.values)
    return sf("""
    % if enum_type.doc:
    ${emit_doc(enum_type.doc)}$hl
    % endif
    enum ${enum_type.dsl_name} {$i$hl
        case ${literals}$hl
    $d}$hl
    """.strip())


def format_token_name(name):
    return unparsed_name(name, "_tok")


def format_pattern(pat):
    # Decode backslash encodings in "pat"
    escaped = {
        '\f': '\\f',
        '\n': '\\n',
        '\r': '\\r',
        '\t': '\\t',
        '"': '\\"',
    }
    return '"{}"'.format(''.join(escaped.get(c, c) for c in pat))


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
            return "if previous_token is {} then {}".format(
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
            result.append('$hl$hl')
            # Handle spacing annotations
            spacings = ctx.lexer.spacing_table[family]
            for otherfam, has_spacing in spacings.items():
                if has_spacing:
                    result.append(
                        f'@unparse_spacing(with={otherfam.name.lower})$hl'
                    )
            result.append('family {} {{$i'
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
        key=lambda assoc: assoc[1]._id,
    )

    def doc(name: str) -> str:
        """
        Return doc for the given parsing rule.
        """
        doc = ctx.grammar.user_defined_rules_docs[name]
        return (
            "".join(f"## {line}\n$hl" for line in doc.split("\n"))
            if doc
            else ""
        )

    def annotations(name: str) -> str:
        """
        Return annotations for the given parsing rule.
        """
        if name == ctx.grammar.main_rule_name:
            return "@main_rule "
        elif name in ctx.grammar.entry_points:
            return "@entry_point "
        else:
            return ""

    template = """
    @with_lexer(${ctx.lang_name.lower}_lexer)$hl
    grammar ${ctx.lang_name.lower}_grammar {$i$hl
    % for name, rule in sorted_rules:
        ${doc(name)}${annotations(name)}${name} <- ${emit_rule(rule, True)}$hl
    % endfor
    $d
    }$hl
    """

    f.write(pp(sf(template)))


def unparse_nodes(ctx, f):
    """
    Unparse the nodes for the current language to the given file.
    """
    from langkit.compiled_types import CompiledTypeRepo
    from langkit.diagnostics import check_source_language, Severity
    check_source_language(
        predicate=libpythonlang_available,
        message="libpythonlang not found, comments cannot be unparsed",
        severity=Severity.warning,
        do_raise=False
    )

    if CompiledTypeRepo.dynamic_vars:
        f.write("\n")
    for dynvar in CompiledTypeRepo.dynamic_vars:
        name = dynvar.argument_name.lower
        typ = type_name(dynvar.type)
        decl = ""
        if dynvar.doc:
            decl += emit_doc(dynvar.doc) + "$hl\n"
        decl += f"dynvar {name}: {typ}$hl"
        f.write(pp(decl))

    enum_types = [emit_enum_type(t)
                  for t in ctx.enum_types
                  if not t.is_builtin_type]

    types = keep(emit_node_type(t)
                 for t in ctx.astnode_types + ctx._struct_types
                 if not t.is_builtin_type)

    template = """
    % for t in enum_types:
        $hl
        ${t}
    % endfor
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
                f = open(value, 'w', encoding='utf-8')
            elif action == 'import':
                f.write('import {}\n\n'.format(value))
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
    from io import StringIO
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
