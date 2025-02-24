from __future__ import annotations

import dataclasses
import enum
from functools import reduce
from typing import Callable

import liblktlang as L

from langkit.compile_context import CompileCtx
from langkit.compiled_types import CompiledType, EnumType, T
from langkit.diagnostics import Location, check_source_language, error
import langkit.expressions as E
from langkit.expressions import AbstractExpression, LocalVars, PropertyDef
import langkit.frontend.func_signatures as S
from langkit.frontend.resolver import Resolver
from langkit.frontend.scopes import Scope
from langkit.frontend.static import (
    denoted_char,
    denoted_str,
    parse_static_bool,
    parse_static_str,
)
from langkit.frontend.utils import lkt_context, name_from_lower
import langkit.names as names


def extract_var_name(ctx: CompileCtx, id: L.Id) -> tuple[str, names.Name]:
    """
    Turn the lower cased name ``n`` into a valid Ada identifier (for code
    generation).
    """
    source_name = id.text
    var_name = (
        names.Name("Ignored")
        if source_name == "_" else
        names.Name("Local") + name_from_lower(ctx, "variable", id)
    )
    return source_name, var_name


class BuiltinAttribute(enum.Enum):
    as_bare_entity = enum.auto()
    as_entity = enum.auto()
    children = enum.auto()
    env_node = enum.auto()
    env_parent = enum.auto()
    is_null = enum.auto()
    parent = enum.auto()
    symbol = enum.auto()
    to_symbol = enum.auto()


class BuiltinMethod(enum.Enum):
    all = enum.auto()
    any = enum.auto()
    append_rebinding = enum.auto()
    as_array = enum.auto()
    as_big_int = enum.auto()
    as_int = enum.auto()
    concat_rebindings = enum.auto()
    contains = enum.auto()
    do = enum.auto()
    empty = enum.auto()
    env_group = enum.auto()
    env_orphan = enum.auto()
    filter = enum.auto()
    filtermap = enum.auto()
    find = enum.auto()
    get = enum.auto()
    get_first = enum.auto()
    get_value = enum.auto()
    iall = enum.auto()
    iany = enum.auto()
    ifilter = enum.auto()
    ifiltermap = enum.auto()
    ilogic_all = enum.auto()
    ilogic_any = enum.auto()
    imap = enum.auto()
    imapcat = enum.auto()
    is_visible_from = enum.auto()
    itake_while = enum.auto()
    join = enum.auto()
    length = enum.auto()
    logic_all = enum.auto()
    logic_any = enum.auto()
    map = enum.auto()
    mapcat = enum.auto()
    rebind_env = enum.auto()
    shed_rebindings = enum.auto()
    singleton = enum.auto()
    solve = enum.auto()
    solve_with_diagnostics = enum.auto()
    super = enum.auto()
    take_while = enum.auto()
    to_builder = enum.auto()
    unique = enum.auto()
    update = enum.auto()


class NullCond:
    """
    Class acting as a namespace for helpers to handle null-conditional
    expressions (``?.``/``?[]`` in Lkt).

    Handling these expressions requires a few tricks, as they behavior is not
    local: when the left operand (i.e. the prefix) evaluates to null, execution
    must jump up in the expression tree, climbing up the chain of parent
    prefixes.  To implement this behavior, we lower these expressions to
    ``Then`` expressions. For instance::

       # Tree for A?.B.C
       DotExpr(
         f_prefix=NullCondDottedName(
           f_prefix=A,
           f_suffix=B,
         ),
         f_suffix=C,
       )

       # Lowered tree
       Then(
           base=A,
           var_expr=Var(V1),
           then_expr=FieldAccess(FieldAccess(V1, B), C),
       )

    This expansion is performed as Lkt expressions are lowered to
    ``AbstractExpression`` trees. The idea is to keep track of null checks
    during the recursion on expression trees, and wrap up checks + the lowered
    expression to the corresponding ``Then`` expression whenever we are
    lowering an expression that is not a prefix.

    Checks are recorded using a stack of variable/expression couples
    (the ``CheckCouple`` type defined below), with the following semantics:

      * The expression of the bottom of the stack (``CheckStack[0].expr``) is
        to be evaluated first, and assigned to the corresponding variable
        (``CheckStack[0].var``).

      * If it evaluated to null, then all other expressions are skipped, and
        the whole expression must return a null value directly.

      * Otherwise, proceed with the next expression in the stack
        (``CheckStack[1].expr``, that uses the first variable to do its
        computation), assign it to its own variable, etc.

      * Once the last stack expression has been evaluated to a non-null value,
        the rest of the expression can be evaluated.

    Let's illustrate this with an example::

       # Checks stack for A?.B.C?.D.E:
       [0] var_1, A
       [1] var_2, var_1.B.C
       [2] var_3, var_2.D

       # Remaining expression:
       var_3.E

    In order to evaluate the whole expression, we start with the first check:
    evaluate ``A`` and assign the result to the ``var_1`` variable: if it is
    null, the whole expression returns a null value for the type of the ``E``
    field, otherwise, evaluation continues with the second expression:
    ``var_1.B.C``. Rinse and repeat... If ``var_3`` is assigned a non-null
    value, then we can then evaluate ``var_3.E``, i.e. the evalution of the
    whole expression has completed.

    Lowering first builds this stack of checks, adding one check to the stack
    whenever compiling a null-conditional operation.  It is then trivial to
    turn the stack into the right nesting of ``Then`` expression when compiling
    a chain of prefix that is itself a non-prefix operand (like ``A?.B`` in
    ``A?.B + C``): see the ``NullCond.wrap_checks`` method below.

    Building the stack of checks is easy with a simple recursion on the
    expression tree. When recursion starts on an expression, it begins with an
    empty list of checks. From there, we distinguish two cases:

    * When processing this expression's prefix, the list of checks is passed
      down to recursion: the recursive call will append checks in that list,
      collecting the chain of computations/checks necessary to compute the
      prefix. This is what allows the nested checks to propagate up in the
      expression tree.

    * When processing a non-prefix operand, we recurse with a new empty list of
      checks, and wrap them at the end of recursion.

    Again, here are some examples to clarify::

       # Parsing tree for A?.B.C?.D, with [labels] to explain recursion below
       [eD] NullCondDottedName(
       [eC]   f_prefix=DotExpr(
       [eB]     f_prefix=NullCondDottedName(
                  f_prefix=A,
                  f_suffix=B,
                ),
                f_suffix=C,
              ),
              f_suffix=D,
            )

    * [Depth 1] Lowering starts at ``eD`` with an empty list of checks:
      let's call it ``C1``. Its only subexpression is its prefix (``eC``):
      recursion goes directly to it.

    * [Depth 2] The only subexpression in ``eC`` is its prefix ``eB``:
      lowering recurses on it.

    * [Depth 3] The only subexpression in ``eC`` is its prefix ``eB``:
      lowering recurses on it.

    * [Depth 4] This just returns the expression for ``A`` (let's call it
      ``X1``), potentially adding checks to ``C1`` if ``A`` contains checked
      prefixes.

    * [Back to depth 3] That was a null-checking node: a new variable is
      created (let's call it ``V1``), and a new check couple (``V1``, ``X1``)
      is added to ``C1``. Recursion at that level returns ``V1.B``.

    * [Back to depth 2] That was not a null-checking node, so this returns the
      expression for ``V1.B.C``.

    * [Back to depth 1] That was a null-checking node: a new variable is
      created (let's call it ``V2``), and a new check couple (``V2``,
      ``V1.B.C``) is added to ``C1``. Recursion at that level returns ``V2.D``.

    Prefix expression lowering completes at this stage with the following
    stack::

       [0] V1, X1
       [1] V2, V1.B.C

    And the following returned expression::

       V2.D

    In order to get a single expression out of it, we wrap the stack and
    returned expression into into the desired final expression::

       Then(
           base=X1,
           var_expr=Var(V1),
           then_expr=Then(
               base=FieldAccess(FieldAccess(V1, B), C),
               var_expr=Var(V2),
               then_expr=FieldAccess(V2, D),
           ),
       )
    """

    @dataclasses.dataclass
    class CheckCouple:
        """
        Variable/expression couple in the expansion stack for null conditional
        expressions.
        """
        var: LocalVars.LocalVar
        """
        Variable that is checked.
        """

        expr: AbstractExpression
        """
        Initialization expression for that variable.
        """

    CheckStack = list[CheckCouple]

    @staticmethod
    def record_check(
        location: Location,
        checks: NullCond.CheckStack,
        expr: AbstractExpression,
    ) -> AbstractExpression:
        """
        Return a new variable after appending a new couple for it and ``expr``
        to ``checks``.
        """
        var = PropertyDef.get().vars.create_scopeless(
            location, codegen_name="Var_Expr"
        )
        checks.append(NullCond.CheckCouple(var, expr))
        return var.abs_var

    @staticmethod
    def wrap_checks(
        checks: NullCond.CheckStack,
        expr: AbstractExpression,
    ) -> AbstractExpression:
        """
        Turn the given checks and ``expr`` to the final expression according to
        null conditional rules.
        """

        from langkit.expressions import Then

        result = expr
        for couple in reversed(checks):
            then = Then(
                couple.expr.location, couple.expr, couple.var, [], result
            )
            then.underscore_then = True
            result = then
        return result


@dataclasses.dataclass
class BuiltinCallInfo:
    """
    Information about the call to a builtin operation that takes a lambda as
    the first argument, plus optional keyword arguments.
    """
    kwargs: dict[str, L.Expr]
    """
    Keyword arguments passed after the lambda expression.
    """

    scope: Scope
    """
    New scope to lower lambda function arguments and inner expression.
    """

    largs: list[L.LambdaArgDecl]
    """
    List of arguments for this lambda expression.
    """

    expr: L.Expr
    """
    Lambda expression "body".
    """


@dataclasses.dataclass
class CollectionLoweringResult:
    """
    Container for the result of the "lower_collection_iter" function.
    """

    inner_expr: AbstractExpression
    """
    Expression to evaluate each element of the array the collection expression
    computes.
    """

    lambda_arg_infos: list[E.LambdaArgInfo]
    """
    Information about all lambda arguments involved in this expression.
    """

    element_var: LocalVars.LocalVar
    """
    Iteration variable to hold each collection element.
    """

    index_var: LocalVars.LocalVar | None
    """
    Iteration variable to hold each collection element index, if needed.
    """


@dataclasses.dataclass
class DeclAction:
    """
    Helper for block lowering. Represents a declaration in a block expression.
    """

    location: Location
    """
    Location of this variable declaration. This may not be ``var``'s location
    for "declarations" that represent dynamic variable bindings: the dynamic
    variable is declared at the module level, whereas this "declaration" is
    located inside a property ("VarBind" Lkt node).
    """

    local_var: E.LocalVars.LocalVar
    """
    Local variable used to store the binding value.
    """

    init_expr: AbstractExpression
    """
    Initialization expression for this variable.
    """


@dataclasses.dataclass
class DynVarBindAction(DeclAction):
    dynvar: E.DynamicVariable


class ExpressionCompiler:
    """
    Translator for Lkt expressions to abstract expressions.
    """

    def __init__(self, resolver: Resolver, prop: PropertyDef | None):
        """
        :param resolver: Resolver for Lkt entities.
        :param prop: If the expression to translate is the body of a property,
            this must be the corresponding property instance. If not, the
            expression is compiled as a static one (compiled time known value).
        """
        self.resolver = resolver
        self.ctx = resolver.context
        self.prop = prop
        self.local_vars = None if prop is None else prop.vars
        self.static_required = prop is None

        self.generics = resolver.builtins.generics

        self.dynamic_lexical_env = (
            resolver.builtins.functions.dynamic_lexical_env
        )

    def lower(self, expr: L.Expr, env: Scope) -> E.AbstractExpression:
        """
        Lower the given expression to an abstract expression.

        :param expr: Expression to lower.
        :param env: Scope to use when resolving references.
        """
        checks: NullCond.CheckStack = []
        result = self._lower_expr(expr, checks, env)
        return NullCond.wrap_checks(checks, result)

    def lower_prefix_expr(
        self,
        expr: L.Expr,
        checks: NullCond.CheckStack,
        with_check: bool,
        env: Scope,
    ) -> AbstractExpression:
        """
        Lower a expression that acts as a prefix for the handling of
        null-conditional expressions.

        :param expr: Expression to lower.
        :param checks: List of check couples to handle null-conditional
            expressions (see ``NullCond``).
        :param with_check: Whether the parent expression needs a check for this
            prefix.
        :param env: Scope to use when resolving references.
        """
        result = self._lower_expr(expr, checks, env)
        return (
            NullCond.record_check(
                Location.from_lkt_node(expr), checks, result
            )
            if with_check else
            result
        )

    def lower_expr(self, expr: L.Expr, env: Scope) -> AbstractExpression:
        """
        Lower the given expression.

        :param expr: Expression to lower.
        :param env: Scope to use when resolving references.
        """
        checks: NullCond.CheckStack = []
        result = self._lower_expr(expr, checks, env)
        return NullCond.wrap_checks(checks, result)

    def _lower_expr(
        self,
        expr: L.Expr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> AbstractExpression:
        loc = Location.from_lkt_node(expr)
        result: AbstractExpression

        def abort_if_static_required(expr: L.Expr) -> None:
            """
            Abort lowering if a static expression is required for "expr".
            """
            if self.static_required:
                with lkt_context(expr):
                    error("static expression expected in this context")

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Recursion shortcut for non-prefix subexpressions.
            """
            return self.lower_expr(expr, env)

        if isinstance(expr, L.AnyOf):
            abort_if_static_required(expr)

            prefix = lower(expr.f_expr)
            return E.AnyOf(
                loc, lower(expr.f_expr), *[lower(v) for v in expr.f_values]
            )

        elif isinstance(expr, L.ArrayLiteral):
            abort_if_static_required(expr)

            elts = [lower(e) for e in expr.f_exprs]
            element_type = (
                None
                if expr.f_element_type is None else
                self.resolver.resolve_type(expr.f_element_type, env)
            )
            return E.ArrayLiteral(loc, elts, element_type=element_type)

        elif isinstance(expr, L.BigNumLit):
            abort_if_static_required(expr)

            text = expr.text
            assert text[-1] == 'b'
            return E.BigIntLiteral(loc, int(text[:-1]))

        elif isinstance(expr, L.BinOp):
            abort_if_static_required(expr)

            # Lower both operands
            left = lower(expr.f_left)
            right = lower(expr.f_right)

            # Dispatch to the appropriate abstract expression constructor
            if isinstance(expr.f_op, L.OpEq):
                return E.Eq(loc, left, right)

            elif isinstance(expr.f_op, L.OpNe):
                return E.Not(loc, E.Eq(Location.builtin, left, right))

            elif isinstance(expr.f_op, (L.OpLt, L.OpGt, L.OpLte, L.OpGte)):
                operator = {
                    L.OpLt: E.OrderingTest.LT,
                    L.OpLte: E.OrderingTest.LE,
                    L.OpGt: E.OrderingTest.GT,
                    L.OpGte: E.OrderingTest.GE,
                }[type(expr.f_op)]
                return E.OrderingTest(loc, operator, left, right)

            elif isinstance(expr.f_op, L.OpAnd):
                return E.BooleanBinaryOp(loc, E.BinaryOpKind.AND, left, right)

            elif isinstance(expr.f_op, L.OpOr):
                return E.BooleanBinaryOp(loc, E.BinaryOpKind.OR, left, right)

            elif isinstance(expr.f_op, L.OpLogicAnd):
                return E.LogicBinaryOp(loc, E.BinaryOpKind.AND, left, right)

            elif isinstance(expr.f_op, L.OpLogicOr):
                return E.LogicBinaryOp(loc, E.BinaryOpKind.OR, left, right)

            elif isinstance(expr.f_op, L.OpOrInt):
                # Create a variable to store the evaluation of the left
                # operand, then use a Then construct to conditionally evaluate
                # (and return) the right operand if the left one turns out to
                # be null.
                assert self.local_vars is not None
                left_var = self.local_vars.create_scopeless(
                    Location.builtin, "Left_Var"
                )
                return E.Then(
                    location=loc,
                    base=left,
                    local_var=left_var,
                    lambda_arg_infos=[],
                    then_expr=left_var.abs_var,
                    default_expr=right,
                )

            else:
                operator = {
                    L.OpAmp: '&',
                    L.OpPlus: '+',
                    L.OpMinus: '-',
                    L.OpMult: '*',
                    L.OpDiv: '/',
                }[type(expr.f_op)]
                return E.Arithmetic(loc, left, right, operator)

        elif isinstance(expr, L.BlockExpr):
            abort_if_static_required(expr)

            assert self.local_vars is not None
            loc = Location.from_lkt_node(expr)
            sub_env = env.create_child(
                f"scope for block at {loc.gnu_style_repr()}"
            )

            actions: list[DeclAction] = []

            for v in expr.f_val_defs:
                location = Location.from_lkt_node(v)
                scope_var: Scope.UserValue

                if isinstance(v, L.ValDecl):
                    # Create the local variable for this declaration
                    source_name = v.f_syn_name.text
                    source_name, v_name = extract_var_name(
                        self.ctx, v.f_syn_name
                    )
                    v_type = (
                        self.resolver.resolve_type(v.f_decl_type, env)
                        if v.f_decl_type else
                        None
                    )
                    local_var = self.local_vars.create_scopeless(
                        Location.from_lkt_node(v), v_name, v_type, source_name
                    )
                    scope_var = Scope.LocalVariable(
                        source_name, v, local_var.abs_var
                    )
                    actions.append(
                        DeclAction(
                            location=location,
                            local_var=local_var,
                            init_expr=self.lower_expr(v.f_expr, sub_env),
                        )
                    )

                elif isinstance(v, L.VarBind):
                    # Look for the corresponding dynamic variable, either
                    # unbound (BuiltinDynVar or DynVar, that we will bound) or
                    # already bounded (BoundDynVar, that we will rebind in this
                    # scope).
                    entity = self.resolver.resolve_entity(v.f_name, sub_env)
                    if isinstance(entity, Scope.BoundDynVar):
                        dyn_var = entity.dyn_var
                    elif isinstance(
                        entity, (Scope.BuiltinDynVar, Scope.DynVar)
                    ):
                        dyn_var = entity.variable
                    else:
                        error(
                            "dynamic variable expected, got"
                            f" {entity.diagnostic_name}",
                            location=v.f_name,
                        )

                    local_var = self.local_vars.create(
                        location, dyn_var.name, dyn_var.type
                    )
                    scope_var = Scope.BoundDynVar(
                        v.f_name.text, v, local_var.abs_var, dyn_var
                    )
                    actions.append(
                        DynVarBindAction(
                            location=location,
                            dynvar=dyn_var,
                            local_var=local_var,
                            init_expr=self.lower_expr(v.f_expr, sub_env),
                        )
                    )

                else:
                    assert False, f'Unhandled def in BlockExpr: {v}'

                # Make the declared value/dynamic variable available to the
                # remaining expressions.
                sub_env.add(scope_var)

            # Lower the block main expression and wrap it in declarative
            # blocks.
            result = self.lower_expr(expr.f_expr, sub_env)
            for action in reversed(actions):
                if isinstance(action, DynVarBindAction):
                    result = E.DynVarBind(
                        action.location,
                        action.dynvar,
                        action.local_var,
                        action.init_expr,
                        result,
                    )
                else:
                    result = E.Let(
                        action.location,
                        [(action.local_var, action.init_expr)],
                        result,
                    )
            return result

        elif isinstance(expr, L.CallExpr):
            call_expr = expr
            call_name = call_expr.f_name

            def lower_new(t: CompiledType) -> AbstractExpression:
                """
                Consider that this call creates a new struct, return the
                corresponding New expression.
                """
                # Non-struct/node types have their own constructor
                if t == T.RefCategories:
                    arg_nodes, kwarg_nodes = self.extract_call_args(call_expr)
                    if arg_nodes:
                        error(
                            "Positional arguments not allowed for"
                            " RefCategories",
                            location=call_expr,
                        )

                    default_expr = kwarg_nodes.pop("_", None)
                    enabled_categories = {
                        k: parse_static_bool(self.ctx, v)
                        for k, v in kwarg_nodes.items()
                    }
                    return E.RefCategories(
                        loc,
                        default=(
                            False
                            if default_expr is None else
                            parse_static_bool(self.ctx, default_expr)
                        ),
                        **enabled_categories,
                    )
                else:
                    abort_if_static_required(expr)

                    args, kwargs = self.lower_call_args(call_expr, lower)
                    if args:
                        error(
                            "Positional arguments not allowed for struct"
                            " constructors",
                            location=call_expr,
                        )
                    return E.New(loc, t, **kwargs)

            # Depending on its name, a call can have different meanings...

            # If it is a simple identifier...
            if isinstance(call_name, L.RefId):
                entity = self.resolver.resolve_entity(call_name, env)

                # It can be a call to a built-in function
                if entity == self.dynamic_lexical_env:
                    abort_if_static_required(expr)

                    args, _ = S.dynamic_lexical_env_signature.match(
                        self.ctx, call_expr
                    )
                    trans_parent_expr = args.get("transitive_parent")
                    return E.DynamicLexicalEnv(
                        location=loc,
                        assocs_getter=self.resolver.resolve_property(
                            args["assocs"]
                        ),
                        assoc_resolver=self.resolver.resolve_property(
                            args.get("assoc_resolver")
                        ),
                        transitive_parent=(
                            E.Literal(Location.builtin, True)
                            if trans_parent_expr is None else
                            lower(trans_parent_expr)
                        ),
                    )

                # It can be a New expression
                elif isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                    return lower_new(entity.t)

                # Everything else is illegal
                with lkt_context(call_name):
                    error("invalid call prefix")

            # If the call name is a generic instantiation, it has to be a
            # reference to a struct type, and thus the call is a New
            # expression.
            elif isinstance(call_name, L.GenericInstantiation):
                abort_if_static_required(expr)

                generic = self.resolver.resolve_generic(call_name.f_name, env)
                type_args = call_name.f_args
                if generic != self.generics.entity:
                    error(
                        f"only {self.generics.entity.name} is the only legal"
                        " generic in this context"
                    )
                with lkt_context(type_args):
                    check_source_language(
                        len(type_args) == 1,
                        f"{generic.name} expects one type argument: the node"
                        " type"
                    )

                node_arg = type_args[0]
                node_type = self.resolver.resolve_node(node_arg, env)
                return lower_new(node_type.entity)

            # Otherwise the call has to be a dot expression, for a method
            # invocation.
            elif not isinstance(call_name, L.BaseDotExpr):
                with lkt_context(call_name):
                    error("invalid call prefix")

            abort_if_static_required(expr)

            return self.lower_method_call(loc, call_expr, checks, env)

        elif isinstance(expr, L.CastExpr):
            abort_if_static_required(expr)

            subexpr = lower(expr.f_expr)
            excludes_null = expr.f_excludes_null.p_as_bool
            dest_type = self.resolver.resolve_type(expr.f_dest_type, env)
            return E.Cast(loc, subexpr, dest_type, do_raise=excludes_null)

        elif isinstance(expr, L.CharLit):
            return E.CharacterLiteral(loc, denoted_char(expr))

        elif isinstance(expr, L.BaseDotExpr):
            null_cond = isinstance(expr, L.NullCondDottedName)

            # Dotted expressions can designate an enum value (if the prefix is
            # a type name) or a member access.
            prefix_node = expr.f_prefix
            if isinstance(prefix_node, L.RefId):
                try:
                    entity = env.lookup(prefix_node.text)
                except KeyError:
                    pass
                else:
                    if isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                        check_source_language(
                            not null_cond,
                            "null-conditional dotted name notation is illegal"
                            " to designate an enum value",
                            location=expr.f_suffix,
                        )

                        # The suffix refers to the declaration of an enum
                        # value: the prefix must designate the corresponding
                        # enum type.
                        if not isinstance(entity.t, EnumType):
                            error("enum type expected", location=expr.f_prefix)
                        try:
                            return entity.t.resolve_value(expr.f_suffix.text)
                        except KeyError:
                            error("no such enum value", location=expr.f_suffix)

            # Otherwise, the prefix is a regular expression, so this dotted
            # expression is an access to a member.
            abort_if_static_required(expr)

            prefix = self.lower_prefix_expr(
                expr=expr.f_prefix,
                checks=checks,
                with_check=null_cond,
                env=env,
            )
            suffix = expr.f_suffix.text

            # Make sure this is not an attempt to access a builtin method
            try:
                BuiltinMethod[suffix]
            except KeyError:
                pass
            else:
                with lkt_context(expr.f_suffix):
                    error("this is a builtin method, it should be called")

            # Handle accesses to builtin attributes and regular field
            # access separately.
            try:
                builtin = BuiltinAttribute[suffix]
            except KeyError:
                return E.FieldAccess(
                    loc, prefix, suffix, check_call_syntax=True
                )

            if builtin == BuiltinAttribute.as_bare_entity:
                return E.as_bare_entity(loc, prefix)
            elif builtin == BuiltinAttribute.as_entity:
                return E.as_entity(loc, prefix)
            elif builtin == BuiltinAttribute.children:
                return E.children(loc, prefix)
            elif builtin == BuiltinAttribute.env_node:
                return E.env_node(loc, prefix)
            elif builtin == BuiltinAttribute.env_parent:
                return E.env_parent(loc, prefix)
            elif builtin == BuiltinAttribute.is_null:
                return E.is_null(loc, prefix)
            elif builtin == BuiltinAttribute.parent:
                return E.parent(loc, prefix)
            elif builtin == BuiltinAttribute.symbol:
                return E.node_to_symbol(loc, prefix)
            elif builtin == BuiltinAttribute.to_symbol:
                return E.string_to_symbol(loc, prefix)
            else:
                raise AssertionError(f"unhandled builtin: {builtin.name}")

        elif isinstance(expr, L.IfExpr):
            abort_if_static_required(expr)

            # We want to turn the following pattern::
            #
            #   IfExpr(C1, E1, [(C2, E2), (C3, E3), ...], E_last)
            #
            # into the following expression tree::
            #
            #   If(C1, E1,
            #      If(C2, E2,
            #         If(C3, E3,
            #            ... E_Last)))
            #
            # so first translate the "else" expression (E_last), then
            # reverse iterate on the alternatives to wrap this expression
            # with the conditional checks.
            result = lower(expr.f_else_expr)
            conditions = (
                [(expr.f_cond_expr, expr.f_then_expr)]
                + [
                    (alt.f_cond_expr, alt.f_then_expr)
                    for alt in expr.f_alternatives
                ]
            )
            for c, e in reversed(conditions):
                result = E.If(loc, lower(c), lower(e), result)
            return result

        elif isinstance(expr, L.Isa):
            abort_if_static_required(expr)

            subexpr = lower(expr.f_expr)
            nodes = [
                self.resolver.resolve_type(type_ref, env)
                for type_ref in expr.f_dest_type
            ]
            return E.is_a(loc, subexpr, nodes)

        elif isinstance(expr, L.LogicAssign):
            dest_var = lower(expr.f_dest_var)
            value_expr = lower(expr.f_value)
            return E.Bind(
                loc,
                dest_var,
                value_expr,
                kind=E.BindKind.assign,
            )

        elif isinstance(expr, L.LogicExpr):
            abort_if_static_required(expr)

            logic_expr = expr
            expr = expr.f_expr
            if isinstance(expr, L.RefId):
                if expr.text == "true":
                    return E.LogicTrue(loc)
                elif expr.text == "false":
                    return E.LogicFalse(loc)

            elif isinstance(expr, L.CallExpr):
                call_name = expr.f_name
                if not isinstance(call_name, L.RefId):
                    with lkt_context(expr):
                        error("invalid logic expression")

                if call_name.text in ("all", "any"):
                    _, vargs = S.logic_all_any_signature.match(self.ctx, expr)
                    op_kind = (
                        E.BinaryOpKind.AND
                        if call_name.text == "all" else
                        E.BinaryOpKind.OR
                    )
                    with lkt_context(logic_expr):
                        check_source_language(
                            bool(vargs), "at least one equation expected"
                        )
                    return reduce(
                        lambda lhs, rhs: E.LogicBinaryOp(
                            loc, op_kind, lhs, rhs
                        ),
                        [lower(a) for a in vargs]
                    )

                elif call_name.text == "domain":
                    args, _ = S.domain_signature.match(self.ctx, expr)
                    logic_var = lower(args["var"])
                    domain_expr = lower(args["domain"])
                    return E.domain(loc, logic_var, domain_expr)

            with lkt_context(expr):
                error("invalid logic expression")

        elif isinstance(expr, L.LogicPredicate):
            pred_prop = self.resolver.resolve_property(expr.f_name)
            arg_exprs = [lower(arg.f_value) for arg in expr.f_args]
            if len(arg_exprs) == 0:
                with lkt_context(expr.f_args):
                    error("at least one argument expected")
            node_expr = arg_exprs.pop(0)
            for arg in expr.f_args:
                if arg.f_name is not None:
                    with lkt_context(arg.f_name):
                        error(
                            "parameter names are not allowed in logic"
                            " propagates"
                        )
            return E.Predicate(
                loc, pred_prop, node_expr, *arg_exprs
            )

        elif isinstance(expr, L.LogicPropagate):
            dest_var = lower(expr.f_dest_var)
            comb_prop = self.resolver.resolve_property(expr.f_call.f_name)
            arg_vars = [lower(arg.f_value) for arg in expr.f_call.f_args]
            for arg in expr.f_call.f_args:
                if arg.f_name is not None:
                    with lkt_context(arg.f_name):
                        error(
                            "parameter names are not allowed in logic"
                            " propagates"
                        )
            return E.NPropagate(loc, dest_var, comb_prop, *arg_vars)

        elif isinstance(expr, L.LogicUnify):
            lhs_var = lower(expr.f_lhs)
            rhs_var = lower(expr.f_rhs)
            return E.Bind(loc, lhs_var, rhs_var, kind=E.BindKind.unify)

        elif isinstance(expr, L.KeepExpr):
            abort_if_static_required(expr)
            assert self.local_vars

            subexpr = lower(expr.f_expr)
            keep_type = self.resolver.resolve_type(expr.f_keep_type, env)
            iter_var = self.local_vars.create_scopeless(
                location=Location.builtin, codegen_name="Item"
            )
            return E.Map(
                location=loc,
                kind="keep",
                collection=subexpr,
                expr=E.Cast(Location.builtin, iter_var.abs_var, keep_type),
                lambda_arg_infos=[],
                element_var=iter_var,
                filter_expr=E.is_a(
                    Location.builtin, iter_var.abs_var, [keep_type]
                ),
            )

        elif isinstance(expr, L.MatchExpr):
            abort_if_static_required(expr)
            assert self.local_vars is not None

            prefix_expr = lower(expr.f_match_expr)

            # Lower each individual matcher
            matchers: list[
                tuple[
                    CompiledType | None,
                    LocalVars.LocalVar,
                    AbstractExpression,
                ]
            ] = []
            for i, m in enumerate(expr.f_branches):
                # Make sure the identifier has the expected casing
                decl_id = m.f_decl.f_syn_name
                if decl_id.text != "_":
                    with lkt_context(decl_id):
                        names.Name.check_from_lower(decl_id.text)

                # Fetch the type to match, if any
                syn_type = m.f_decl.f_decl_type
                matched_type = (
                    None
                    if syn_type is None else
                    self.resolver.resolve_type(syn_type, env)
                )

                # Create the match variable
                var_name = names.Name(f"Match_{i}")
                match_var = self.local_vars.create_scopeless(
                    location=Location.from_lkt_node(m.f_decl),
                    codegen_name=var_name,
                    spec_name=decl_id.text,
                )

                # Lower the matcher expression, making the match variable
                # available if intended.
                sub_loc = Location.from_lkt_node(m)
                sub_env = env.create_child(
                    f"scope for match branch at {sub_loc.gnu_style_repr()}"
                )
                if decl_id.text != "_":
                    sub_env.add(
                        Scope.UserValue(
                            decl_id.text, m.f_decl, match_var.abs_var
                        )
                    )
                match_expr = self.lower_expr(m.f_expr, sub_env)

                matchers.append((matched_type, match_var, match_expr))

            return E.Match(loc, prefix_expr, matchers)

        elif isinstance(expr, L.NotExpr):
            abort_if_static_required(expr)

            return E.Not(loc, lower(expr.f_expr))

        elif isinstance(expr, L.NullLit):
            result_type = self.resolver.resolve_type(expr.f_dest_type, env)
            with lkt_context(expr):
                return E.No(loc, result_type)

        elif isinstance(expr, L.NumLit):
            return E.Literal(loc, int(expr.text))

        elif isinstance(expr, L.ParenExpr):
            return E.Paren(loc, lower(expr.f_expr))

        elif isinstance(expr, L.RaiseExpr):
            abort_if_static_required(expr)

            # A raise expression can only contain a PropertyError struct
            # constructor.
            cons_expr = expr.f_except_expr
            if not isinstance(cons_expr, L.CallExpr):
                error("'raise' must be followed by a call expression")
            call_name = cons_expr.f_name
            entity = self.resolver.resolve_entity(call_name, env)
            if not isinstance(entity, Scope.Exception):
                error(f"exception expected, got {entity.diagnostic_name}")

            # Get the exception message argument
            args_nodes, kwargs_nodes = self.extract_call_args(cons_expr)
            msg_expr: L.Expr | None = None
            if args_nodes:
                msg_expr = args_nodes.pop()
            elif kwargs_nodes:
                msg_expr = kwargs_nodes.pop("exception_message")
            with lkt_context(cons_expr.f_args):
                check_source_language(
                    not args_nodes and not kwargs_nodes,
                    "at most one argument expected: the exception message",
                )

            if msg_expr is None:
                msg = "PropertyError exception"
            else:
                # TODO (S321-013): handle dynamic error message
                msg = parse_static_str(self.ctx, msg_expr)

            return entity.constructor(
                loc, self.resolver.resolve_type(expr.f_dest_type, env), msg
            )

        elif isinstance(expr, L.RefId):
            entity = self.resolver.resolve_entity(expr, env)
            if isinstance(entity, Scope.BuiltinValue):
                if not isinstance(entity.value, E.Literal):
                    abort_if_static_required(expr)

                result = E.Ref(loc, entity.value)
            elif isinstance(entity, Scope.UserValue):
                abort_if_static_required(expr)
                result = E.Ref(loc, entity.variable)
            else:
                with lkt_context(expr):
                    if isinstance(entity, Scope.DynVar):
                        error(
                            f"{entity.name} is not bound in this context:"
                            " please use the 'bind' construct to bind is"
                            " first."
                        )
                    else:
                        error(
                            f"value expected, got {entity.diagnostic_name}"
                        )
            return result

        elif isinstance(expr, L.StringLit):
            abort_if_static_required(expr)

            string_prefix = expr.p_prefix
            string_value = denoted_str(expr)
            if string_prefix == "\x00":
                return E.String(loc, string_value)
            elif string_prefix == "s":
                return E.SymbolLiteral(loc, string_value)
            else:
                error("invalid string prefix")

        elif isinstance(expr, L.SubscriptExpr):
            abort_if_static_required(expr)

            null_cond = isinstance(expr, L.NullCondSubscriptExpr)
            prefix = lower(expr.f_prefix)
            index = lower(expr.f_index)
            return E.collection_get(
                loc,
                prefix,
                index,
                or_null=isinstance(expr, L.NullCondSubscriptExpr),
            )

        elif isinstance(expr, L.TryExpr):
            abort_if_static_required(expr)

            return E.Try(
                location=loc,
                try_expr=lower(expr.f_try_expr),
                else_expr=(
                    None
                    if expr.f_or_expr is None
                    else lower(expr.f_or_expr)
                ),
            )

        elif isinstance(expr, L.UnOp):
            assert isinstance(expr.f_op, L.OpMinus)
            return E.UnaryNeg(loc, lower(expr.f_expr))

        else:
            assert False, 'Unhandled expression: {}'.format(expr)

    def extract_call_args(
        self,
        expr: L.CallExpr,
    ) -> tuple[list[L.Expr], dict[str, L.Expr]]:
        """
        Extract positional and keyword arguments from a call expression.
        """
        args = []
        kwargs = {}
        for arg in expr.f_args:
            value = arg.f_value
            if arg.f_name:
                kwargs[arg.f_name.text] = value
            elif kwargs:
                with lkt_context(arg):
                    error(
                        "positional arguments are forbidden after the first"
                        " keyword argument"
                    )
            else:
                args.append(value)
        return args, kwargs

    def lower_call_args(
        self,
        expr: L.CallExpr,
        lower: Callable[[L.Expr], AbstractExpression],
    ) -> tuple[list[AbstractExpression], dict[str, AbstractExpression]]:
        """
        Collect call positional and keyword arguments.
        """
        arg_nodes, kwarg_nodes = self.extract_call_args(expr)
        args = [lower(v) for v in arg_nodes]
        kwargs = {k: lower(v) for k, v in kwarg_nodes.items()}
        return args, kwargs

    def lower_method_call(
        self,
        location: Location,
        call_expr: L.CallExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> AbstractExpression:
        """
        Subroutine for "lower_expr": lower specifically a method call.

        :param call_expr: Method call to lower.
        :param checks: List of check couples to handle null-conditional
            expressions (see ``NullCond``).
        :param env: Scope to use when resolving references.
        """

        result: AbstractExpression

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Convenience wrapper around "self.lower_expr".
            """
            return self.lower_expr(expr, env)

        def add_lambda_arg_to_scope(
            scope: Scope,
            arg: L.LambdaArgDecl,
            var: LocalVars.LocalVar
        ) -> None:
            """
            Helper to register a lambda expression argument in a scope.
            """
            scope.add(
                Scope.LocalVariable(arg.f_syn_name.text, arg, var.abs_var)
            )

        def append_lambda_arg_info(
            infos: list[E.LambdaArgInfo],
            arg: L.LambdaArgDecl,
            var: LocalVars.LocalVar,
        ) -> None:
            if arg.f_decl_type is not None:
                infos.append(
                    E.LambdaArgInfo(
                        var,
                        self.resolver.resolve_type(arg.f_decl_type, env),
                        Location.from_lkt_node(arg.f_decl_type),
                    )
                )

        def var_for_lambda_arg(
            scope: Scope,
            arg: L.LambdaArgDecl,
            infos: list[E.LambdaArgInfo],
            prefix: str,
            type: CompiledType | None = None,
        ) -> LocalVars.LocalVar:
            """
            Create local variable to translate a lambda argument.

            This also registers this decl/variable association in ``env``.

            :param scope: Scope in which to register this variable.
            :param arg: Lambda argument to lower.
            :param infos: List of lambda argument information in which to
                append information for this argument, in case the argument
                declaration contains a type annotation.
            :param prefix: Lower-case prefix for the name of the variable in
                the generated code.
            :param type: Optional type information to associate to this
                variable.
            """
            assert self.local_vars is not None
            source_name, _ = extract_var_name(self.ctx, arg.f_syn_name)
            result = self.local_vars.create_scopeless(
                Location.from_lkt_node(arg),
                names.Name.from_lower(prefix),
                spec_name=source_name,
                type=type,
            )
            add_lambda_arg_to_scope(scope, arg, result)
            append_lambda_arg_info(infos, arg, result)
            return result

        def extract_lambda(
            expr: L.LambdaExpr,
            lambda_n_args: int,
        ) -> tuple[Scope, list[L.LambdaArgDecl], L.Expr]:
            """
            Extract arguments/expr from a lambda expression.

            :param expr: Lambda expression to analyze.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            actual_n_args = len(expr.f_params)
            with lkt_context(expr.f_params):
                check_source_language(
                    actual_n_args == lambda_n_args,
                    f"{lambda_n_args} arguments expected, got {actual_n_args}",
                )
            for larg in expr.f_params:
                with lkt_context(larg):
                    check_source_language(
                        larg.f_default_val is None,
                        "default values are not allowed here",
                    )

            loc = Location.from_lkt_node(expr)
            scope = env.create_child(
                f"scope for lambda expression at {loc.gnu_style_repr()}"
            )

            return (scope, list(expr.f_params), expr.f_body)

        def extract_lambda_and_kwargs(
            expr: L.CallExpr,
            signature: S.FunctionSignature,
            arg_for_lambda: str,
            lambda_n_args: int,
        ) -> BuiltinCallInfo:
            """
            Extract arguments from a call expression, expecting the first
            positional argument to be a lambda expression.

            :param expr: Call expression that is supposed to pass the lambda
                expression.
            :param signature: Signature for the builtin function that is
                called.
            :param arg_for_lambda: Name of the argument in ``signature`` that
                must contain the lambda.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            # Make sure the only positional argument is a lambda expression
            args, _ = signature.match(self.ctx, expr)
            lambda_expr = args[arg_for_lambda]
            if not isinstance(lambda_expr, L.LambdaExpr):
                with lkt_context(lambda_expr):
                    error("lambda expression expected")

            # Extract info from the lambda expression itself
            scope, lambda_args, lambda_body = extract_lambda(
                lambda_expr, lambda_n_args
            )

            return BuiltinCallInfo(args, scope, lambda_args, lambda_body)

        def lower_collection_iter(has_index: bool) -> CollectionLoweringResult:
            """
            Helper to lower a method call that implements a collection
            iteration.

            This assumes that that ``call_expr`` is such a method call: the
            signature for this method is ``S.collection_iter_signature``, and
            its ``expr`` argument is expected to be a lambda function to
            process one collection element. That lambda function must accept
            the collection element itself only (if ``has_index`` is false) or
            an additional element index (if ``has_index`` is true).

            Return the lowered expression for the lambda, information for
            lambda args, the variable for the iteration element, and an
            optional variable for the iteration index.
            """
            # We expect a single argument: a lambda (itself taking the
            # collection element plus optionally its index).
            lambda_info = extract_lambda_and_kwargs(
                call_expr,
                S.collection_iter_signature,
                "expr",
                2 if has_index else 1
            )
            lambda_arg_infos: list[E.LambdaArgInfo] = []
            element_arg = lambda_info.largs[0]
            if has_index:
                index_arg = lambda_info.largs[1]

            # There is always an iteration variable for the collection element
            element_var = var_for_lambda_arg(
                lambda_info.scope,
                element_arg,
                lambda_arg_infos,
                "item",
            )

            # The iteration variable for the iteration index is optional: we
            # create one only if the lambda has the corresponding element.
            index_var: LocalVars.LocalVar | None = None
            if has_index:
                index_var = var_for_lambda_arg(
                    lambda_info.scope,
                    index_arg,
                    lambda_arg_infos,
                    "index",
                    T.Int,
                )

            # Lower the body expression for that lambda
            inner_expr = self.lower_expr(lambda_info.expr, lambda_info.scope)
            return CollectionLoweringResult(
                inner_expr, lambda_arg_infos, element_var, index_var
            )

        def lower_node_builder(prefix: L.Expr) -> AbstractExpression:
            """
            Helper to lower the creation of a synthetizing node builder.

            :param prefix: Prefix for the ".builder()" method, i.e. the
                expected synthetic node type reference.
            """
            with lkt_context(prefix):
                if not isinstance(prefix, (L.DotExpr, L.TypeRef, L.RefId)):
                    error("Prefix for .builder expressions must be a node")

            node_type = self.resolver.resolve_node_type_expr(prefix, env)

            args, kwargs = self.lower_call_args(call_expr, lower)
            with lkt_context(call_expr.f_args):
                if len(args) != 0:
                    error("Positional arguments not allowed for .builder")

            return E.CreateSynthNodeBuilder(location, node_type, **kwargs)

        call_name = call_expr.f_name
        assert isinstance(call_name, L.BaseDotExpr)

        method_name = call_name.f_suffix.text

        # Handle node builder creation from node types
        if method_name == "builder":
            return lower_node_builder(call_name.f_prefix)

        # TODO (eng/libadalang/langkit#728): introduce a pre-lowering pass to
        # extract the list of types and their fields/methods so that we can
        # perform validation here.
        method_prefix = self.lower_prefix_expr(
            expr=call_name.f_prefix,
            checks=checks,
            with_check=isinstance(call_name, L.NullCondDottedName),
            env=env,
        )

        # Make sure this is not an attempt to call a builin field
        try:
            BuiltinAttribute[method_name]
        except KeyError:
            pass
        else:
            with lkt_context(call_name.f_suffix):
                error("this is a builtin attribute, it should not be called")

        # Handle calls to builtin methods and regular properties separately
        try:
            builtin = BuiltinMethod[method_name]
        except KeyError:
            call_args, call_kwargs = self.lower_call_args(call_expr, lower)
            return E.FieldAccess(
                location,
                method_prefix,
                method_name,
                E.FieldAccess.Arguments(call_args, call_kwargs),
                check_call_syntax=True,
            )

        # Past this point, we know that this is a builtin method call
        if builtin in (
            BuiltinMethod.all,
            BuiltinMethod.any,
            BuiltinMethod.iall,
            BuiltinMethod.iany,
        ):
            clr = lower_collection_iter(
                has_index=builtin in (BuiltinMethod.iall, BuiltinMethod.iany),
            )
            result = E.Quantifier(
                location,
                (
                    "all"
                    if builtin in (BuiltinMethod.all, BuiltinMethod.iall) else
                    "any"
                ),
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
            )

        elif builtin == BuiltinMethod.append_rebinding:
            args, _ = S.append_rebinding_signature.match(self.ctx, call_expr)
            result = E.append_rebinding(
                location,
                method_prefix,
                lower(args["old_env"]),
                lower(args["new_env"]),
            )

        elif builtin == BuiltinMethod.as_array:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.as_array(location, method_prefix)

        elif builtin == BuiltinMethod.as_big_int:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.BigIntLiteral(location, method_prefix)

        elif builtin == BuiltinMethod.as_int:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.as_int(location, method_prefix)

        elif builtin == BuiltinMethod.concat_rebindings:
            args, _ = S.concat_rebindings_signature.match(self.ctx, call_expr)
            result = E.concat_rebindings(
                location, method_prefix, lower(args["rebindings"])
            )

        elif builtin == BuiltinMethod.contains:
            args, _ = S.contains_signature.match(self.ctx, call_expr)
            result = E.Contains(location, method_prefix, lower(args["value"]))

        elif builtin == BuiltinMethod.do:
            lambda_info = extract_lambda_and_kwargs(
                call_expr, S.do_signature, "expr", 1
            )
            arg_node = lambda_info.largs[0]

            lambda_arg_infos: list[E.LambdaArgInfo] = []
            arg_var = var_for_lambda_arg(
                lambda_info.scope,
                arg_node,
                lambda_arg_infos,
                "var_expr",
            )
            then_expr = self.lower_expr(lambda_info.expr, lambda_info.scope)

            default_val = (
                lower(lambda_info.kwargs["default_val"])
                if "default_val" in lambda_info.kwargs else
                None
            )

            result = E.Then(
                location,
                method_prefix,
                arg_var,
                lambda_arg_infos,
                then_expr,
                default_val,
            )

        elif builtin == BuiltinMethod.empty:
            S.empty_signature.match(self.ctx, call_expr)
            length = E.length(Location.builtin, method_prefix)
            return E.Eq(location, length, E.Literal(Location.builtin, 0))

        elif builtin == BuiltinMethod.env_group:
            args, _ = S.env_group_signature.match(self.ctx, call_expr)
            with_md_expr = args.get("with_md")
            with_md = None if with_md_expr is None else lower(with_md_expr)
            result = E.env_group(location, method_prefix, with_md=with_md)

        elif builtin == BuiltinMethod.env_orphan:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.env_orphan(location, method_prefix)

        elif builtin in (BuiltinMethod.filter, BuiltinMethod.ifilter):
            clr = lower_collection_iter(
                has_index=builtin == BuiltinMethod.ifilter
            )
            result = E.Map(
                location=location,
                kind=builtin.name,
                collection=method_prefix,
                expr=clr.element_var.abs_var,
                lambda_arg_infos=clr.lambda_arg_infos,
                element_var=clr.element_var,
                index_var=clr.index_var,
                filter_expr=clr.inner_expr,
            )

        elif builtin in (BuiltinMethod.filtermap, BuiltinMethod.ifiltermap):
            has_index = builtin == BuiltinMethod.ifiltermap
            lambda_n_args = 2 if has_index else 1

            # Validate arguments for ".[i]filtermap()" itself
            args, _ = S.filtermap_signature.match(self.ctx, call_expr)
            for arg in [args["expr"], args["filter"]]:
                if not isinstance(arg, L.LambdaExpr):
                    with lkt_context(arg):
                        error("lambda expressions expceted")

            # Validate and analyze the two lambda expressions
            lambda_0 = args["expr"]
            assert isinstance(lambda_0, L.LambdaExpr)
            map_scope, map_args, map_body = extract_lambda(
                lambda_0, lambda_n_args
            )

            lambda_1 = args["filter"]
            assert isinstance(lambda_1, L.LambdaExpr)
            filter_scope, filter_args, filter_body = extract_lambda(
                lambda_1, lambda_n_args
            )

            # We need to have two different scopes for the two lambda
            # expressions, but need to create common iteration variables for
            # both.
            lambda_arg_infos = []
            element_var = var_for_lambda_arg(
                map_scope,
                map_args[0],
                lambda_arg_infos,
                "item",
            )
            name_from_lower(self.ctx, "argument", filter_args[0].f_syn_name)
            add_lambda_arg_to_scope(filter_scope, filter_args[0], element_var)
            append_lambda_arg_info(
                lambda_arg_infos, filter_args[0], element_var
            )

            index_var: LocalVars.LocalVar | None = None
            if has_index:
                index_var = var_for_lambda_arg(
                    map_scope, map_args[1], lambda_arg_infos, "index", T.Int
                )
                name_from_lower(
                    self.ctx, "argument", filter_args[1].f_syn_name
                )
                add_lambda_arg_to_scope(
                    filter_scope, filter_args[1], index_var
                )
                append_lambda_arg_info(
                    lambda_arg_infos, filter_args[1], index_var
                )

            # Lower their expressions
            map_expr = self.lower_expr(map_body, map_scope)
            filter_expr = self.lower_expr(filter_body, filter_scope)

            return E.Map(
                location=location,
                kind=builtin.name,
                collection=method_prefix,
                expr=map_expr,
                lambda_arg_infos=lambda_arg_infos,
                element_var=element_var,
                index_var=index_var,
                filter_expr=filter_expr,
            )

        elif builtin == BuiltinMethod.find:
            lambda_info = extract_lambda_and_kwargs(
                call_expr, S.collection_iter_signature, "expr", 1
            )
            elt_arg = lambda_info.largs[0]

            lambda_arg_infos = []
            elt_var = var_for_lambda_arg(
                lambda_info.scope,
                elt_arg,
                lambda_arg_infos,
                'item',
            )
            inner_expr = self.lower_expr(lambda_info.expr, lambda_info.scope)

            result = E.Find(
                location,
                method_prefix,
                inner_expr,
                lambda_arg_infos,
                elt_var,
                index_var=None,
            )
        elif builtin in (BuiltinMethod.get, BuiltinMethod.get_first):
            args, _ = S.get_signature.match(self.ctx, call_expr)
            symbol = lower(args["symbol"])

            lookup_expr = args.get("lookup")
            lookup: AbstractExpression | None = (
                None if lookup_expr is None else lower(lookup_expr)
            )

            from_node_expr = args.get("from")
            from_node: AbstractExpression | None = (
                None if from_node_expr is None else lower(from_node_expr)
            )

            categories_expr = args.get("categories")
            categories: AbstractExpression | None = (
                None if categories_expr is None else lower(categories_expr)
            )

            return E.env_get(
                location,
                only_first=method_name == "get_first",
                env=method_prefix,
                symbol=symbol,
                lookup=lookup,
                from_node=from_node,
                categories=categories,
            )

        elif builtin == BuiltinMethod.get_value:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.get_value(location, method_prefix)

        elif builtin == BuiltinMethod.is_visible_from:
            args, _ = S.is_visible_from_signature.match(self.ctx, call_expr)
            result = E.is_visible_from(
                location, method_prefix, lower(args["unit"])
            )

        elif builtin == BuiltinMethod.join:
            args, _ = S.join_signature.match(self.ctx, call_expr)
            result = E.join(location, method_prefix, lower(args["strings"]))

        elif builtin == BuiltinMethod.length:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.length(location, method_prefix)

        elif builtin in (
            BuiltinMethod.ilogic_all,
            BuiltinMethod.ilogic_any,
            BuiltinMethod.logic_all,
            BuiltinMethod.logic_any,
        ):
            import langkit.expressions.logic as LE

            has_index = builtin in (
                BuiltinMethod.ilogic_all, BuiltinMethod.ilogic_any
            )
            is_all = builtin in (
                BuiltinMethod.ilogic_all, BuiltinMethod.logic_all
            )

            clr = lower_collection_iter(has_index=has_index)
            map_expr = E.Map(
                location,
                builtin.name,
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
            )
            result = (
                LE.All(location, map_expr)
                if is_all else
                LE.Any(location, map_expr)
            )

        elif builtin in (
            BuiltinMethod.imap,
            BuiltinMethod.imapcat,
            BuiltinMethod.map,
            BuiltinMethod.mapcat,
        ):
            clr = lower_collection_iter(
                has_index=builtin in (
                    BuiltinMethod.imap, BuiltinMethod.imapcat
                )
            )
            result = E.Map(
                location,
                builtin.name,
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                do_concat=builtin in (
                    BuiltinMethod.mapcat, BuiltinMethod.imapcat
                ),
            )

        elif builtin == BuiltinMethod.rebind_env:
            args, _ = S.rebind_env_signature.match(self.ctx, call_expr)
            result = E.rebind_env(location, method_prefix, lower(args["env"]))

        elif builtin == BuiltinMethod.singleton:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.singleton(location, method_prefix)

        elif builtin == BuiltinMethod.shed_rebindings:
            args, _ = S.shed_rebindings_signature.match(self.ctx, call_expr)
            result = E.shed_rebindings(
                location, method_prefix, lower(args["entity_info"])
            )

        elif builtin in (
            BuiltinMethod.solve, BuiltinMethod.solve_with_diagnostics
        ):
            S.empty_signature.match(self.ctx, call_expr)
            result = E.solve(
                location,
                equation=method_prefix,
                with_diagnostics=builtin.name == "solve_with_diagnostics"
            )

        elif builtin == BuiltinMethod.super:
            call_args, call_kwargs = self.lower_call_args(call_expr, lower)
            result = E.Super(
                location, method_prefix, *call_args, **call_kwargs
            )

        elif builtin in (
            BuiltinMethod.itake_while, BuiltinMethod.take_while
        ):
            clr = lower_collection_iter(
                has_index=builtin == BuiltinMethod.itake_while
            )
            result = E.Map(
                location,
                builtin.name,
                method_prefix,
                clr.element_var.abs_var,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                take_while_expr=clr.inner_expr,
            )

        elif builtin == BuiltinMethod.to_builder:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.CreateCopyNodeBuilder(location, method_prefix)

        elif builtin == BuiltinMethod.unique:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.unique(location, method_prefix)

        elif builtin == BuiltinMethod.update:
            arg_nodes, kwarg_nodes = self.extract_call_args(call_expr)
            if arg_nodes:
                error(
                    ".update() accepts keyword arguments only",
                    location=arg_nodes[0],
                )
            field_exprs = {k: lower(v) for k, v in kwarg_nodes.items()}
            result = E.StructUpdate(location, method_prefix, **field_exprs)

        else:
            assert False, f"unhandled builitn call: {call_name.f_suffix}"

        return result
