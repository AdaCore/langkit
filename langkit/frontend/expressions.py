from __future__ import annotations

import dataclasses
import enum

import funcy

import liblktlang as L

from langkit.common import ascii_repr, text_repr
from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    ArrayType,
    BaseStructType,
    CompiledType,
    EntityType,
    EnumType,
    StructType,
    T,
)
from langkit.diagnostics import (
    Location,
    Severity,
    WarningSet,
    check_source_language,
    emit_error,
    error,
)
import langkit.expressions as E
from langkit.expressions import LocalVars, PropertyDef
import langkit.frontend.func_signatures as S
from langkit.frontend.resolver import Resolver
from langkit.frontend.scopes import Scope
from langkit.frontend.static import (
    denoted_char,
    denoted_str,
    parse_static_bool,
    parse_static_str,
)
from langkit.frontend.utils import name_from_lower
import langkit.names as names
from langkit.utils import TypeSet


def extract_var_name(ctx: CompileCtx, id: L.Id) -> tuple[str, names.Name]:
    """
    Turn the lower cased name ``n`` into a valid Ada identifier (for code
    generation).
    """
    source_name = id.text
    var_name = (
        names.Name("Ignored")
        if source_name == "_"
        else names.Name("Local") + name_from_lower(ctx, "variable", id)
    )
    return source_name, var_name


def expr_type_matches(
    location: L.LktNode,
    expr: E.Expr,
    t: CompiledType,
) -> None:
    """
    Emit an error located at ``location`` if the type of ``expr`` does not
    match ``t``.
    """
    check_source_language(
        expr.type.matches(t),
        f"Expected type {t.dsl_name}, got {expr.type.dsl_name}",
        location=location,
    )


def reject_param_names(args: L.ArgumentList, context: str) -> None:
    """
    If the ``args`` argument list contain named associations, emit an error.

    :param args: Argument list to validate.
    :param context: String that designates the context if which this argument
        list appears (used to format the error message).
    """
    for a in args:
        if a.f_name is not None:
            error(
                f"parameter names are not allowed in {context}",
                location=a.f_name,
            )


def call_parens_loc(call_expr: L.BaseCallExpr) -> Location:
    """
    Return the sloc range for the parens in the given call expresion.
    """
    args = call_expr.f_args
    assert args.token_start is not None
    assert args.token_start.previous is not None
    assert args.token_end is not None
    return Location.from_lkt_tokens(
        call_expr, args.token_start.previous, args.token_end
    )


def detect_variadic_logic_properties(
    expr: L.Expr, args: list[E.Expr]
) -> tuple[bool, list[E.Expr], list[E.Expr]]:
    """
    Separate logic variable expressions from extra argument expressions.

    This helper detects the leading arguments in ``args`` that.

    :param expr: Expression that is being lowered when analyzing the arguments.
        Used to give context to the error diagnostics that may be emitted.
    :param args: List of arguments to analyze.
    """
    logic_var_args: list[E.Expr]
    captured_args: list[E.Expr]
    is_variadic = False
    if args[0].type.matches(T.LogicVar.array):
        # Make sure this predicate works on clean logic variables
        logic_var_args = [E.ResetAllLogicVars(args[0])]
        captured_args = args[1:]
        is_variadic = True
    else:
        logic_var_args, captured_args = funcy.lsplit_by(
            lambda e: e.type == T.LogicVar, args
        )
        # Make sure this predicate works on clean logic variables
        logic_var_args = [E.ResetLogicVar(e) for e in logic_var_args]

    check_source_language(
        all(e.type != T.LogicVar for e in captured_args),
        "Logic variable expressions should be grouped at the beginning,"
        " and should not appear after non logic variable expressions",
        location=expr,
    )
    check_source_language(
        all(e.type != T.LogicVar.array for e in captured_args),
        "Unexpected logic variable array",
        location=expr,
    )

    return is_variadic, logic_var_args, captured_args


def debug_info(expr: L.Expr, label: str) -> E.ExprDebugInfo:
    """
    Shortcut to create the debug information for an expression.
    """
    return E.ExprDebugInfo(label, Location.from_lkt_node(expr))


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
    at = enum.auto()
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
         f_prefix=DottedName(
           f_prefix=A,
           f_null_cond=NullCondQualifierPresent,
           f_suffix=B,
         ),
         f_null_cond=NullCondQualifierAbsent,
         f_suffix=C,
       )

       # Lowered tree
       Then(
           base=A,
           var_expr=Var(V1),
           then_expr=EvalMemberExpr(EvalMemberExpr(V1, B), C),
       )

    This expansion is performed as Lkt expressions are lowered to ``Expr``
    trees. The idea is to keep track of null checks during the recursion on
    expression trees, and wrap up checks + the lowered expression to the
    corresponding ``Then`` expression whenever we are lowering an expression
    that is not a prefix.

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
       [eD] DottedName(
       [eC]   f_prefix=DotExpr(
       [eB]     f_prefix=DottedName(
                  f_prefix=A,
                  f_null_cond=NullCondQualifierPresent,
                  f_suffix=B,
                ),
                f_null_cond=NullCondQualifierAbsent,
                f_suffix=C,
              ),
              f_null_cond=NullCondQualifierPresent,
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
               base=EvalMemberExpr(EvalMemberExpr(V1, B), C),
               var_expr=Var(V2),
               then_expr=EvalMemberExpr(V2, D),
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

        expr: E.Expr
        """
        Initialization expression for that variable.
        """

    CheckStack = list[CheckCouple]

    @staticmethod
    def record_check(
        location: Location,
        checks: NullCond.CheckStack,
        expr: E.Expr,
    ) -> E.Expr:
        """
        Return a new variable after appending a new couple for it and ``expr``
        to ``checks``.
        """
        var = PropertyDef.get().vars.create(
            location, codegen_name="Var_Expr", type=expr.type
        )
        checks.append(NullCond.CheckCouple(var, expr))
        return var.ref_expr

    @staticmethod
    def wrap_checks(checks: NullCond.CheckStack, expr: E.Expr) -> E.Expr:
        """
        Turn the given checks and ``expr`` to the final expression according to
        null conditional rules.
        """
        result = expr
        for couple in reversed(checks):
            result = E.ThenExpr(
                debug_info=None,
                expr=couple.expr,
                var_expr=couple.var.ref_expr,
                then_expr=result,
                default_expr=E.NullExpr(None, result.type),
            )
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

    largs: list[L.LambdaParamDecl]
    """
    List of arguments for this lambda expression.
    """

    expr: L.Expr
    """
    Lambda expression "body".
    """


@dataclasses.dataclass(frozen=True)
class CollectionAnalysisResult:
    """
    Holder for the result of the ``analyze_collection_expr`` method: commonly
    needed attributes for the collection.
    """

    expr: E.Expr
    """
    Expression for the collection. Note that it may evaluate to an entity,
    which is not a collection itself in the generated code: in that case, only
    the bare node it contains (a list node) is a collection.
    """

    collection_type: CompiledType
    """
    Type for the actual collection (see above).
    """

    codegen_element_type: CompiledType
    """
    Type for the element type as stored in the collection (see above).
    """

    user_element_type: CompiledType
    """
    Type for the element type as seen by the user.
    """

    with_entity: bool
    """
    Whether the collection is actually an entity. Collection items are stored
    as bare nodes, so iterations must wrap them with the collection's own
    entity info.
    """


@dataclasses.dataclass
class CollectionLambdaIterationLoweringResult:
    """
    Holder for the result of the ``lower_collection_lambda_iter`` function:
    aggregate of items from the lowering of a collection iteration done through
    a lambda function (like: ``c.map((e, i) => something``).
    """

    syn_inner_expr: L.Expr
    """
    Node for the lambda body expression.
    """

    inner_expr: E.Expr
    """
    Expression to evaluate each element of the array the collection expression
    computes.
    """

    inner_scope: E.LocalVars.Scope
    """
    Scope in which ``inner_expr`` is evaluated.
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

    init_expr: E.Expr
    """
    Initialization expression for this variable.
    """

    init_expr_node: L.Expr
    """
    Node for the initialization expression for this variable.
    """


@dataclasses.dataclass
class DynVarBindAction(DeclAction):
    dynvar: E.DynamicVariable
    """
    Dynamic variable to bind.
    """

    binding_token: E.DynamicVariable.BindingToken
    """
    Binding token, used to disable the binding once the lowering of the scope
    that contains the binding is done.
    """


class ExpressionCompiler:
    """
    Translator for Lkt expressions to the expression IR.
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

    def lower_expr(self, expr: L.Expr, env: Scope) -> E.Expr:
        """
        Lower the given Lkt expression to the expression IR.

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
    ) -> E.Expr:
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
            NullCond.record_check(Location.from_lkt_node(expr), checks, result)
            if with_check
            else result
        )

    def abort_if_static_required(self, expr: L.Expr) -> None:
        """
        Abort lowering if a static expression is required for "expr".
        """
        if self.static_required:
            error("static expression expected in this context", location=expr)

    def _lower_expr(
        self,
        expr: L.Expr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        """
        Lower a expression from an Lkt tree to the expression IR.

        :param expr: Expression to lower.
        :param checks: List of check couples to handle null-conditional
            expressions (see ``NullCond``).
        :param env: Scope to use when resolving references.
        """
        result: E.Expr

        def lower(expr: L.Expr) -> E.Expr:
            """
            Recursion shortcut for non-prefix subexpressions.
            """
            return self.lower_expr(expr, env)

        if isinstance(expr, L.AnyOf):
            return self.lower_any_of(expr, env)

        elif isinstance(expr, L.ArrayLiteral):
            return self.lower_array_literal(expr, env)

        elif isinstance(expr, L.BigNumLit):
            return self.lower_big_num_lit(expr, env)

        elif isinstance(expr, L.BinOp):
            return self.lower_bin_op(expr, env)

        elif isinstance(expr, L.BlockExpr):
            return self.lower_block_expr(expr, env)

        elif isinstance(expr, L.CallExpr):
            return self.lower_call_expr(expr, checks, env)

        elif isinstance(expr, L.CastExpr):
            return self.lower_cast_expr(expr, checks, env)

        elif isinstance(expr, L.CharLit):
            return E.CharacterLiteralExpr(
                debug_info(expr, f"CharacterLiteral {expr.text}"),
                denoted_char(expr),
            )

        elif isinstance(expr, L.DotExpr):
            return self.lower_dot_expr(expr, checks, env)

        elif isinstance(expr, L.IfExpr):
            return self.lower_if_expr(expr, env)

        elif isinstance(expr, L.Isa):
            return self.lower_is_a(expr, env)

        elif isinstance(expr, L.LogicAssign):
            return self.lower_logic_assign(expr, env)

        elif isinstance(expr, L.LogicExpr):
            return self.lower_logic_expr(expr, env)

        elif isinstance(expr, L.LogicPredicate):
            return self.lower_logic_predicate(expr, env)

        elif isinstance(expr, L.LogicPropagate):
            return self.lower_logic_propagate(expr, env)

        elif isinstance(expr, L.LogicUnify):
            return self.lower_logic_unify(expr, env)

        elif isinstance(expr, L.KeepExpr):
            return self.lower_keep(expr, checks, env)

        elif isinstance(expr, L.MatchExpr):
            return self.lower_match(expr, env)

        elif isinstance(expr, L.NotExpr):
            self.abort_if_static_required(expr)
            subexpr = self.lower_expr(expr.f_expr, env)
            expr_type_matches(expr.f_expr, subexpr, T.Bool)
            return E.make_not_expr(debug_info(expr, "Not"), subexpr)

        elif isinstance(expr, L.NullLit):
            result_type = self.resolver.resolve_type(expr.f_dest_type, env)
            check_source_language(
                result_type.null_allowed,
                f"Invalid type for Null expression: {result_type.dsl_name}",
                location=expr.f_dest_type,
            )
            return E.NullExpr(debug_info(expr, "Null"), result_type)

        elif isinstance(expr, L.NumLit):
            return E.IntegerLiteralExpr(
                debug_info(expr, f"IntLiteral {expr.text}"), int(expr.text)
            )

        elif isinstance(expr, L.ParenExpr):
            return self.lower_expr(expr.f_expr, env)

        elif isinstance(expr, L.RaiseExpr):
            return self.lower_raise_expr(expr, env)

        elif isinstance(expr, L.RefId):
            entity = self.resolver.resolve_entity(expr, env)
            if isinstance(entity, Scope.BuiltinValue):
                if not isinstance(entity.value, E.BindableLiteralExpr):
                    self.abort_if_static_required(expr)

                # If this is a reference to the "self" variable, then the
                # current property uses entity info, which must be tracked.
                if isinstance(entity, Scope.SelfVariable):
                    assert self.prop is not None
                    self.prop.set_uses_entity_info()

                result = entity.value
            elif isinstance(entity, Scope.UserValue):
                self.abort_if_static_required(expr)
                result = entity.variable
            elif isinstance(entity, Scope.DynVar):
                error(
                    f"{entity.name} is not bound in this context:"
                    " please use the 'bind' construct to bind is"
                    " first.",
                    location=expr,
                )
            else:
                error(
                    f"value expected, got {entity.diagnostic_name}",
                    location=expr,
                )
            return result

        elif isinstance(expr, L.StringLit):
            return self.lower_string_lit(expr, env)

        elif isinstance(expr, L.SubscriptExpr):
            return self.lower_subscript_expr(expr, checks, env)

        elif isinstance(expr, L.TryExpr):
            return self.lower_try_expr(expr, env)

        elif isinstance(expr, L.UnOp):
            assert isinstance(expr.f_op, L.OpMinus)
            subexpr = self.lower_expr(expr.f_expr, env)
            if not subexpr.type.matches(T.Int) and not subexpr.type.matches(
                T.BigInt
            ):
                error(
                    f"{T.Int.dsl_name} or {T.BigInt.dsl_name} expected, got"
                    f" {subexpr.type.dsl_name}",
                    location=expr.f_expr,
                )
            return E.UnaryNegExpr(debug_info(expr, "UnaryNeg"), subexpr)

        else:
            raise AssertionError(f"Unhandled expression: {expr!r}")

    def extract_call_args(
        self,
        expr: L.CallExpr,
    ) -> tuple[
        list[tuple[L.Argument, L.Expr]], dict[str, tuple[L.Argument, L.Expr]]
    ]:
        """
        Extract positional and keyword arguments from a call expression.
        """
        args = []
        kwargs = {}
        for arg in expr.f_args:
            value = arg.f_value
            if arg.f_name:
                kwargs[arg.f_name.text] = (arg, value)
            elif kwargs:
                error(
                    "positional arguments are forbidden after the first"
                    " keyword argument",
                    location=arg,
                )
            else:
                args.append((arg, value))
        return args, kwargs

    def lower_call_args(
        self,
        expr: L.CallExpr,
        env: Scope,
    ) -> tuple[
        list[tuple[L.Argument, E.Expr]],
        dict[str, tuple[L.Argument, E.Expr]],
    ]:
        """
        Collect call positional and keyword arguments and lower them to the
        expression IR.
        """
        arg_nodes, kwarg_nodes = self.extract_call_args(expr)
        args = [(p, self.lower_expr(v, env)) for p, v in arg_nodes]
        kwargs = {
            k: (p, self.lower_expr(v, env))
            for k, (p, v) in kwarg_nodes.items()
        }
        return args, kwargs

    def lower_method_call(
        self,
        call_expr: L.CallExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        """
        Subroutine for "lower_expr": lower specifically a method call.

        :param call_expr: Method call to lower.
        :param checks: List of check couples to handle null-conditional
            expressions (see ``NullCond``).
        :param env: Scope to use when resolving references.
        """
        assert self.prop is not None
        assert self.local_vars is not None

        result: E.Expr

        call_name = call_expr.f_name
        assert isinstance(call_name, L.DotExpr)

        method_name = call_name.f_suffix.text
        method_loc = Location.from_lkt_node(call_name.f_suffix)
        null_cond = call_name.f_null_cond.p_as_bool

        def add_lambda_arg_to_scope(
            scope: Scope, arg: L.LambdaParamDecl, var: LocalVars.LocalVar
        ) -> None:
            """
            Helper to register a lambda expression argument in a scope.
            """
            scope.add(
                Scope.LocalVariable(arg.f_syn_name.text, arg, var.ref_expr)
            )

        def check_lambda_arg_type(
            arg: L.LambdaParamDecl,
            var: LocalVars.LocalVar,
        ) -> None:
            if arg.f_decl_type is not None:
                decl_type = self.resolver.resolve_type(arg.f_decl_type, env)
                check_source_language(
                    decl_type == var.type,
                    f"{var.type.dsl_name} expected",
                    location=arg.f_decl_type,
                )

        def var_for_lambda_arg(
            env: Scope,
            arg: L.LambdaParamDecl,
            prefix: str,
            type: CompiledType,
        ) -> LocalVars.LocalVar:
            """
            Create local variable to translate a lambda argument.

            This also registers this decl/variable association in ``env``.

            :param env: Lkt scope in which to register this variable.
            :param arg: Lambda argument to lower.
            :param prefix: Lower-case prefix for the name of the variable in
                the generated code.
            :param type: Type for this variable.
            """
            assert self.local_vars is not None
            source_name, _ = extract_var_name(self.ctx, arg.f_syn_name)
            result = self.local_vars.create(
                Location.from_lkt_node(arg),
                names.Name.from_lower(prefix),
                spec_name=source_name,
                type=type,
            )
            add_lambda_arg_to_scope(env, arg, result)
            check_lambda_arg_type(arg, result)
            return result

        def extract_lambda(
            expr: L.LambdaExpr,
            lambda_n_args: int,
        ) -> tuple[Scope, list[L.LambdaParamDecl], L.Expr]:
            """
            Extract arguments/expr from a lambda expression.

            :param expr: Lambda expression to analyze.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            actual_n_args = len(expr.f_params)
            check_source_language(
                actual_n_args == lambda_n_args,
                f"{lambda_n_args} arguments expected, got {actual_n_args}",
                location=expr.f_params,
            )
            for larg in expr.f_params:
                check_source_language(
                    larg.f_default_val is None,
                    "default values are not allowed here",
                    location=larg,
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
                error("lambda expression expected", location=lambda_expr)

            # Extract info from the lambda expression itself
            scope, lambda_args, lambda_body = extract_lambda(
                lambda_expr, lambda_n_args
            )

            return BuiltinCallInfo(args, scope, lambda_args, lambda_body)

        def lower_collection_lambda_iter(
            element_type: CompiledType, has_index: bool
        ) -> CollectionLambdaIterationLoweringResult:
            """
            Helper to lower a method call that implements a collection
            iteration using lambda functions.

            This assumes that that ``call_expr`` is such a method call: the
            signature for this method is ``S.collection_iter_signature``, and
            its ``expr`` argument is expected to be a lambda function to
            process one collection element (whose type is ``element_type``).
            That lambda function must accept the collection element itself only
            (if ``has_index`` is false) or an additional element index (if
            ``has_index`` is true).

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
                2 if has_index else 1,
            )
            element_arg = lambda_info.largs[0]
            if has_index:
                index_arg = lambda_info.largs[1]

            # Create a dedicated scope in which the lambda bodies will run
            assert self.local_vars is not None
            with self.local_vars.current_scope.new_child() as inner_scope:

                # There is always an iteration variable for the collection
                # element.
                element_var = var_for_lambda_arg(
                    lambda_info.scope, element_arg, "item", element_type
                )

                # The iteration variable for the iteration index is optional:
                # we create one only if the lambda has the corresponding
                # element.
                index_var: LocalVars.LocalVar | None = None
                if has_index:
                    index_var = var_for_lambda_arg(
                        lambda_info.scope, index_arg, "index", T.Int
                    )

                # Lower the body expression for that lambda
                inner_expr = self.lower_expr(
                    lambda_info.expr, lambda_info.scope
                )

            return CollectionLambdaIterationLoweringResult(
                lambda_info.expr,
                inner_expr,
                inner_scope,
                element_var,
                index_var,
            )

        def lower_node_builder(prefix: L.Expr) -> E.Expr:
            """
            Helper to lower the creation of a synthetizing node builder.

            :param prefix: Prefix for the ".builder()" method, i.e. the
                expected synthetic node type reference.
            """
            if not isinstance(prefix, (L.DotExpr, L.TypeRef, L.RefId)):
                error(
                    "Prefix for .builder expressions must be a node",
                    location=prefix,
                )

            node_type = self.resolver.resolve_node_type_expr(prefix, env)
            if (
                not isinstance(node_type, ASTNodeType)
                or not node_type.synthetic
            ):
                error(
                    "node builders can yield synthetic nodes only",
                    location=method_loc,
                )

            args, kwargs = self.lower_call_args(call_expr, env)
            list_element_builders: E.Expr | None = None
            if node_type.is_list_type:
                check_source_language(
                    len(args) == 1,
                    "One positional argument expected: the array of node"
                    " builders for each element for the list node to"
                    " synthetize",
                    location=method_loc,
                )
                syn_arg, list_element_builders = args[0]
                expr_type_matches(
                    syn_arg,
                    list_element_builders,
                    node_type.element_type.builder_type.array,
                )
            elif len(args) != 0:
                param, _ = args[0]
                error(
                    "Positional arguments not allowed for .builder",
                    location=param,
                )

            return E.make_synth_node_builder(
                debug_info(call_expr, ".builder"),
                node_type,
                E.New.construct_fields(
                    call_expr,
                    node_type,
                    {n: e for n, (_, e) in kwargs.items()},
                    for_node_builder=True,
                ),
                list_element_builders=list_element_builders,
            )

        # Handle node builder creation from node types
        if method_name == "builder":
            return lower_node_builder(call_name.f_prefix)

        syn_prefix = call_name.f_prefix
        method_prefix = self.lower_prefix_expr(
            expr=syn_prefix,
            checks=checks,
            with_check=null_cond,
            env=env,
        )

        # Make sure this is not an attempt to call a builin field
        try:
            BuiltinAttribute[method_name]
        except KeyError:
            pass
        else:
            error(
                "this is a builtin attribute, it should not be called",
                location=call_name.f_suffix,
            )

        # Handle calls to builtin methods and regular properties separately
        try:
            builtin = BuiltinMethod[method_name]
        except KeyError:
            return self.lower_field_access(
                call_expr,
                method_prefix,
                null_cond,
                method_name,
                self.lower_call_args(call_expr, env),
                env,
                is_super=False,
            )

        dbg_info = debug_info(call_expr, f".{method_name}")

        # Past this point, we know that this is a builtin method call
        if builtin in (
            BuiltinMethod.all,
            BuiltinMethod.any,
            BuiltinMethod.iall,
            BuiltinMethod.iany,
        ):
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            clr = lower_collection_lambda_iter(
                coll_info.user_element_type,
                has_index=builtin in (BuiltinMethod.iall, BuiltinMethod.iany),
            )
            expr_type_matches(clr.syn_inner_expr, clr.inner_expr, T.Bool)
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=clr.inner_scope,
                inner_expr=clr.inner_expr,
                element_var=clr.element_var,
                index_var=clr.index_var,
            )
            result = E.QuantifierExpr(
                dbg_info,
                (
                    "all"
                    if builtin in (BuiltinMethod.all, BuiltinMethod.iall)
                    else "any"
                ),
                r,
            )

        elif builtin == BuiltinMethod.append_rebinding:
            args, _ = S.append_rebinding_signature.match(self.ctx, call_expr)
            result = E.make_append_rebinding(
                dbg_info,
                method_prefix,
                self.lower_expr(args["old_env"], env),
                self.lower_expr(args["new_env"], env),
            )

        elif builtin == BuiltinMethod.as_array:
            S.empty_signature.match(self.ctx, call_expr)
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            with self.local_vars.current_scope.new_child() as inner_scope:
                element_var = self.local_vars.create(
                    Location.builtin, "Item", coll_info.user_element_type
                )
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=inner_scope,
                inner_expr=element_var.ref_expr,
                element_var=element_var,
            )
            result = E.MapExpr(dbg_info, r)

        elif builtin == BuiltinMethod.as_big_int:
            S.empty_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.Int)
            result = E.BigIntLiteralExpr(dbg_info, method_prefix)

        elif builtin == BuiltinMethod.as_int:
            S.empty_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.BigInt)
            result = E.make_as_int(dbg_info, method_prefix, self.prop)

        elif builtin == BuiltinMethod.at:
            args, _ = S.at_signature.match(self.ctx, call_expr)
            return self.lower_collection_subscript(
                dbg_info,
                syn_prefix,
                method_prefix,
                args["index"],
                bounds_resilient=True,
                env=env,
            )

        elif builtin == BuiltinMethod.concat_rebindings:
            result = self.lower_concat_rebindings(
                call_expr, syn_prefix, method_prefix, env
            )

        elif builtin == BuiltinMethod.contains:
            args, _ = S.contains_signature.match(self.ctx, call_expr)
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)

            value_expr = self.lower_expr(args["value"], env)
            expr_type_matches(
                args["value"], value_expr, coll_info.user_element_type
            )

            with self.local_vars.current_scope.new_child() as inner_scope:
                element_var = self.local_vars.create(
                    Location.builtin, "Item", coll_info.user_element_type
                )

            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=inner_scope,
                inner_expr=self.lower_eq(
                    None, method_loc, element_var.ref_expr, value_expr
                ),
                element_var=element_var,
            )
            result = E.QuantifierExpr(dbg_info, E.QuantifierExpr.ANY, r)

        elif builtin == BuiltinMethod.do:
            # The prefix type must have a null value
            if not method_prefix.type.null_allowed:
                error(
                    "Invalid prefix type for .do:"
                    f" {method_prefix.type.dsl_name}",
                    location=method_loc,
                )

            with self.local_vars.current_scope.new_child() as inner_scope:
                lambda_info = extract_lambda_and_kwargs(
                    call_expr, S.do_signature, "expr", 1
                )
                arg_node = lambda_info.largs[0]

                # Create a local variable to materialize the lambda argument
                arg_var = var_for_lambda_arg(
                    lambda_info.scope, arg_node, "var_expr", method_prefix.type
                )

                # Lower the two sub-expressions: the "then" one must have
                # access to "arg_var", so lower it in "inner_scope". However
                # the "else" one must not have access to it, so lower it
                # outside of that scope.
                then_expr = self.lower_expr(
                    lambda_info.expr, lambda_info.scope
                )
            then_expr, default_expr = E.expr_or_null(
                expr=E.BindingScope(None, then_expr, [], inner_scope),
                default_expr=(
                    self.lower_expr(lambda_info.kwargs["default_val"], env)
                    if "default_val" in lambda_info.kwargs
                    else None
                ),
                error_location=method_loc,
                context_name=".do expression",
                use_case_name="result type",
            )

            result = E.ThenExpr(
                dbg_info,
                method_prefix,
                arg_var.ref_expr,
                then_expr,
                default_expr,
            )

        elif builtin == BuiltinMethod.empty:
            S.empty_signature.match(self.ctx, call_expr)
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            length_expr = E.make_length(None, method_prefix)
            return self.lower_eq(
                dbg_info,
                method_loc,
                length_expr,
                E.IntegerLiteralExpr(None, 0),
            )

        elif builtin == BuiltinMethod.env_group:
            args, _ = S.env_group_signature.match(self.ctx, call_expr)

            expr_type_matches(syn_prefix, method_prefix, T.LexicalEnv.array)

            with_md_expr = args.get("with_md")
            with_md: E.Expr
            if with_md_expr is None:
                with_md = E.NullExpr(None, T.env_md)
            else:
                with_md = self.lower_expr(with_md_expr, env)
                expr_type_matches(with_md_expr, with_md, T.env_md)

            result = E.make_env_group(dbg_info, method_prefix, with_md)

        elif builtin == BuiltinMethod.env_orphan:
            S.empty_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.LexicalEnv)
            result = E.make_env_orphan(dbg_info, method_prefix)

        elif builtin in (BuiltinMethod.filter, BuiltinMethod.ifilter):
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            clr = lower_collection_lambda_iter(
                coll_info.user_element_type,
                has_index=builtin == BuiltinMethod.ifilter,
            )
            expr_type_matches(clr.syn_inner_expr, clr.inner_expr, T.Bool)
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=clr.inner_scope,
                inner_expr=clr.element_var.ref_expr,
                element_var=clr.element_var,
                index_var=clr.index_var,
            )
            result = E.MapExpr(dbg_info, r, filter=clr.inner_expr)

        elif builtin in (BuiltinMethod.filtermap, BuiltinMethod.ifiltermap):
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            has_index = builtin == BuiltinMethod.ifiltermap
            lambda_n_args = 2 if has_index else 1

            # Validate arguments for ".[i]filtermap()" itself
            args, _ = S.filtermap_signature.match(self.ctx, call_expr)
            for arg in [args["expr"], args["filter"]]:
                if not isinstance(arg, L.LambdaExpr):
                    error("lambda expressions expceted", location=arg)

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

            # We need to have two different Lkt lexical scopes for the two
            # lambda expressions (map_scope and filter_scope above), but need
            # to create common iteration variables for both. However, create a
            # single expression scope for the lowered expressions.
            with self.local_vars.current_scope.new_child() as inner_scope:
                element_var = var_for_lambda_arg(
                    map_scope, map_args[0], "item", coll_info.user_element_type
                )
                name_from_lower(
                    self.ctx, "argument", filter_args[0].f_syn_name
                )
                add_lambda_arg_to_scope(
                    filter_scope, filter_args[0], element_var
                )
                check_lambda_arg_type(filter_args[0], element_var)

                index_var: LocalVars.LocalVar | None = None
                if has_index:
                    index_var = var_for_lambda_arg(
                        map_scope, map_args[1], "index", T.Int
                    )
                    name_from_lower(
                        self.ctx, "argument", filter_args[1].f_syn_name
                    )
                    add_lambda_arg_to_scope(
                        filter_scope, filter_args[1], index_var
                    )
                    check_lambda_arg_type(filter_args[1], index_var)

                # Lower their expressions
                map_expr = self.lower_expr(map_body, map_scope)
                filter_expr = self.lower_expr(filter_body, filter_scope)

            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=inner_scope,
                inner_expr=map_expr,
                element_var=element_var,
                index_var=index_var,
            )
            return E.MapExpr(dbg_info, r, filter_expr)

        elif builtin == BuiltinMethod.find:
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            clr = lower_collection_lambda_iter(
                coll_info.user_element_type, has_index=False
            )
            expr_type_matches(clr.syn_inner_expr, clr.inner_expr, T.Bool)
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=clr.inner_scope,
                inner_expr=clr.inner_expr,
                element_var=clr.element_var,
            )
            result = E.FindExpr(dbg_info, r)

        elif builtin in (BuiltinMethod.get, BuiltinMethod.get_first):
            args, _ = S.get_signature.match(self.ctx, call_expr)

            expr_type_matches(syn_prefix, method_prefix, T.LexicalEnv)

            symbol = self.lower_expr(args["symbol"], env)
            expr_type_matches(args["symbol"], symbol, T.Symbol)

            lookup_expr = args.get("lookup")
            if lookup_expr is None:
                lookup_kind_type = T.LookupKind
                assert isinstance(lookup_kind_type, EnumType)
                lookup = lookup_kind_type.resolve_value(None, "recursive")
            else:
                lookup = self.lower_expr(lookup_expr, env)
                expr_type_matches(lookup_expr, lookup, T.LookupKind)

            from_node_expr = args.get("from")
            from_node: E.Expr | None
            if from_node_expr is None:
                from_node = None
            else:
                from_node = self.lower_expr(from_node_expr, env)
                expr_type_matches(from_node_expr, from_node, T.root_node)

            # If no category is provided, consider they are all requested
            categories_expr = args.get("categories")
            categories: E.Expr
            if categories_expr is None:
                categories = E.RefCategoriesExpr(None, self.ctx.ref_cats)
            else:
                categories = self.lower_expr(categories_expr, env)
                expr_type_matches(categories_expr, categories, T.RefCategories)

            result = E.EnvGetExpr(
                dbg_info,
                env_expr=method_prefix,
                key_expr=symbol,
                lookup_kind_expr=lookup,
                categories_expr=categories,
                sequential_from_expr=from_node,
                only_first=method_name == "get_first",
            )

        elif builtin == BuiltinMethod.get_value:
            S.empty_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.LogicVar)
            result = E.make_get_value(dbg_info, method_prefix)

        elif builtin == BuiltinMethod.is_visible_from:
            args, _ = S.is_visible_from_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.LexicalEnv)
            base_env = self.lower_expr(args["unit"], env)
            expr_type_matches(args["unit"], base_env, T.LexicalEnv)
            result = E.make_is_visible_from(dbg_info, method_prefix, base_env)

        elif builtin == BuiltinMethod.join:
            args, _ = S.join_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.String)
            strings = self.lower_expr(args["strings"], env)
            expr_type_matches(args["strings"], strings, T.String.array)
            result = E.make_join(dbg_info, method_prefix, strings)

        elif builtin == BuiltinMethod.length:
            S.empty_signature.match(self.ctx, call_expr)
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            result = E.make_length(dbg_info, method_prefix)

        elif builtin in (
            BuiltinMethod.ilogic_all,
            BuiltinMethod.ilogic_any,
            BuiltinMethod.logic_all,
            BuiltinMethod.logic_any,
        ):
            has_index = builtin in (
                BuiltinMethod.ilogic_all,
                BuiltinMethod.ilogic_any,
            )
            is_all = builtin in (
                BuiltinMethod.ilogic_all,
                BuiltinMethod.logic_all,
            )

            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            clr = lower_collection_lambda_iter(
                coll_info.user_element_type, has_index=has_index
            )
            expr_type_matches(clr.syn_inner_expr, clr.inner_expr, T.Equation)
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=clr.inner_scope,
                inner_expr=clr.inner_expr,
                element_var=clr.element_var,
                index_var=clr.index_var,
            )
            map_expr = E.MapExpr(None, r)

            # The equation constructor takes an Ada array as a parameter, not
            # our access to record: unwrap it.
            relation_array = E.untyped_literal_expr(
                "Relation_Array ({}.Items)", [map_expr]
            )
            result = E.CallExpr(
                dbg_info,
                "Logic_Boolean_Op",
                f"Solver.Create_{'All' if is_all else 'Any'}",
                T.Equation,
                [relation_array, E.sloc_info_arg(method_loc)],
            )

        elif builtin in (
            BuiltinMethod.imap,
            BuiltinMethod.imapcat,
            BuiltinMethod.map,
            BuiltinMethod.mapcat,
        ):
            has_index = builtin in (BuiltinMethod.imap, BuiltinMethod.imapcat)
            concat = builtin in (BuiltinMethod.imapcat, BuiltinMethod.mapcat)

            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            clr = lower_collection_lambda_iter(
                coll_info.user_element_type, has_index=has_index
            )
            if concat and not clr.inner_expr.type.is_collection:
                error(
                    "Cannot mapcat with expressions returning"
                    f" {clr.inner_expr.type.dsl_name} values (collections"
                    " expected instead)",
                    location=method_loc,
                )
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                index_var=clr.index_var,
                inner_expr=clr.inner_expr,
                element_var=clr.element_var,
                inner_scope=clr.inner_scope,
            )
            result = E.MapExpr(dbg_info, r, do_concat=concat)

        elif builtin == BuiltinMethod.rebind_env:
            args, _ = S.rebind_env_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.LexicalEnv)
            rebindings = self.lower_expr(args["env"], env)
            expr_type_matches(args["env"], rebindings, T.EnvRebindings)
            result = E.make_rebind_env(dbg_info, method_prefix, rebindings)

        elif builtin == BuiltinMethod.singleton:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.SingletonExpr(dbg_info, method_prefix)

        elif builtin == BuiltinMethod.shed_rebindings:
            args, _ = S.shed_rebindings_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.LexicalEnv)
            entity_info = self.lower_expr(args["entity_info"], env)
            expr_type_matches(args["entity_info"], entity_info, T.EntityInfo)
            result = E.make_shed_rebindings(
                dbg_info, method_prefix, entity_info
            )

        elif builtin in (
            BuiltinMethod.solve,
            BuiltinMethod.solve_with_diagnostics,
        ):
            S.empty_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.Equation)
            result = E.make_solve(
                dbg_info,
                equation=method_prefix,
                with_diagnostics=builtin.name == "solve_with_diagnostics",
            )

        elif builtin == BuiltinMethod.super:
            result = self.lower_field_access(
                expr=call_expr,
                prefix=method_prefix,
                null_cond=False,
                suffix=method_name,
                args=self.lower_call_args(call_expr, env),
                env=env,
                is_super=True,
            )

        elif builtin in (BuiltinMethod.itake_while, BuiltinMethod.take_while):
            coll_info = self.analyze_collection_expr(method_prefix, method_loc)
            clr = lower_collection_lambda_iter(
                coll_info.user_element_type,
                has_index=builtin == BuiltinMethod.itake_while,
            )
            r = self.lower_collection_iter(
                location=method_loc,
                collection_info=coll_info,
                inner_scope=clr.inner_scope,
                inner_expr=clr.element_var.ref_expr,
                element_var=clr.element_var,
                index_var=clr.index_var,
            )
            result = E.MapExpr(dbg_info, r, take_while=clr.inner_expr)

        elif builtin == BuiltinMethod.to_builder:
            S.empty_signature.match(self.ctx, call_expr)
            expr_type_matches(syn_prefix, method_prefix, T.root_node)
            result = E.CreateCopyNodeBuilder.common_construct(
                dbg_info, method_prefix
            )

        elif builtin == BuiltinMethod.unique:
            S.empty_signature.match(self.ctx, call_expr)

            array_type = method_prefix.type
            if not isinstance(array_type, ArrayType):
                error(
                    f"Array expected but got {array_type.dsl_name} instead",
                    location=method_loc,
                )

            element_type = array_type.element_type
            if not element_type.hashable:
                error(
                    f"Element type (here {element_type.dsl_name}) must be"
                    " hashable",
                    location=method_loc,
                )

            result = E.make_unique(dbg_info, method_prefix)

        elif builtin == BuiltinMethod.update:
            # Make sure the prefix is a struct
            prefix_type = method_prefix.type
            if not isinstance(prefix_type, StructType):
                error(
                    "Struct expected as the prefix, got"
                    f" {prefix_type.dsl_name}",
                    location=method_loc,
                )

            # Extract the fields to update
            arg_nodes, kwarg_nodes = self.extract_call_args(call_expr)
            if arg_nodes:
                param, _ = arg_nodes[0]
                error(
                    ".update() accepts keyword arguments only", location=param
                )

            # Check that all the given fields are valid structure fields with
            # the correct type.
            fields = prefix_type.required_fields_in_exprs
            assocs = {}
            for name, (param, syn_field_expr) in sorted(kwarg_nodes.items()):
                check_source_language(
                    name in fields,
                    f"Invalid {prefix_type.dsl_name} field: {name}",
                    location=param,
                )
                field = fields[name]
                field_expr = self.lower_expr(syn_field_expr, env)
                assocs[field] = field_expr
                expr_type_matches(syn_field_expr, field_expr, field.type)

            result = E.StructUpdateExpr(dbg_info, method_prefix, assocs)

        else:
            assert False, f"unhandled builitn call: {call_name.f_suffix}"

        return result

    def lower_any_of(self, expr: L.AnyOf, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        prefix = self.lower_expr(expr.f_expr, env)

        # If the prefix is a node/entity, then we only require that other
        # operands are nodes/entities themselves.  Otherwise, operand types
        # must match the prefix's.
        values: list[E.Expr] = []
        expected_type: CompiledType
        if prefix.type.is_ast_node:
            expected_type = T.root_node
        elif prefix.type.is_entity_type:
            expected_type = T.entity
        else:
            expected_type = prefix.type
        for v in expr.f_values:
            ve = self.lower_expr(v, env)
            expr_type_matches(v, ve, expected_type)
            values.append(ve)
        assert len(values) >= 1

        # Make sure the prefix has a result variable so that equality tests
        # do not re-evaluate it over and over.
        expr_var = prefix.create_result_var("Any_Of_Prefix")
        result = E.make_eq_expr(None, expr_var, values.pop())
        while values:
            result = E.IfExpr(
                None,
                E.make_eq_expr(None, expr_var, values.pop()),
                E.LiteralExpr(None, "True", T.Bool),
                result,
            )

        return E.SequenceExpr(debug_info(expr, "AnyOf"), prefix, result)

    def lower_array_literal(
        self,
        expr: L.ArrayLiteral,
        env: Scope,
    ) -> E.Expr:
        self.abort_if_static_required(expr)

        def check_element_type(
            location: L.LktNode,
            expected: CompiledType,
            actual: CompiledType,
        ) -> None:
            """
            Helper to emit an error message if ``expected`` and ``actual`` are
            not the same type.
            """
            check_source_language(
                expected == actual,
                f"Element of type {expected.dsl_name} expected, got"
                f" {actual.dsl_name}",
                location=location,
            )

        explicit_element_type = (
            None
            if expr.f_element_type is None
            else self.resolver.resolve_type(expr.f_element_type, env)
        )
        element_type: CompiledType
        elements: list[E.Expr] = []
        for i, el in enumerate(expr.f_exprs):
            el_expr = self.lower_expr(el, env)
            if i == 0:
                element_type = el_expr.type
                if explicit_element_type is not None:
                    check_element_type(el, explicit_element_type, element_type)
            else:
                check_element_type(el, element_type, el_expr.type)
            elements.append(el_expr)
        if not elements:
            if explicit_element_type is None:
                error(
                    "Missing element type for empty array literal",
                    location=expr,
                )
            else:
                element_type = explicit_element_type

        array_type = element_type.array
        return E.ArrayLiteral.construct_static(
            debug_info(expr, "ArrayLiteral"), elements, array_type
        )

    def lower_big_num_lit(
        self,
        expr: L.BigNumLit,
        env: Scope,
    ) -> E.Expr:
        self.abort_if_static_required(expr)

        text = expr.text
        assert text[-1] == "b"
        return E.BigIntLiteralExpr(
            debug_info(expr, f"BigIntLiteral {text}"), ascii_repr(text[:-1])
        )

    def lower_bin_op(self, expr: L.BinOp, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        # Lower both operands
        left = self.lower_expr(expr.f_left, env)
        right = self.lower_expr(expr.f_right, env)

        # Exact lowering depends on the binary operator
        if isinstance(expr.f_op, L.OpEq):
            return self.lower_eq(
                debug_info(expr, "Eq"), expr.f_op, left, right
            )

        elif isinstance(expr.f_op, L.OpNe):
            return E.make_not_expr(
                debug_info(expr, "NotEqual"),
                self.lower_eq(None, expr.f_op, left, right),
            )

        elif isinstance(expr.f_op, (L.OpLt, L.OpGt, L.OpLte, L.OpGte)):
            match expr.f_op:
                case L.OpLt():
                    operator = E.OrderingTestKind.less_than
                case L.OpLte():
                    operator = E.OrderingTestKind.less_or_equal
                case L.OpGt():
                    operator = E.OrderingTestKind.greater_than
                case L.OpGte():
                    operator = E.OrderingTestKind.greater_or_equal
                case _:
                    raise AssertionError(f"unreachable code: {expr.f_op}")
            dbg_info = debug_info(expr, f"OrderingTest {operator!r}")

            check_source_language(
                left.type.is_long_type
                or left.type.is_big_integer_type
                or left.type.is_ast_node,
                f"Comparisons only work on {T.Int.dsl_name},"
                f" {T.BigInt.dsl_name} or nodes not {left.type.dsl_name}",
                location=expr.f_left,
            )

            # If we are comparing two nodes, just use the dedicated helper
            if left.type.is_ast_node:
                assert self.prop is not None
                check_source_language(
                    right.type.is_ast_node,
                    "A node can only be compared to another node (got"
                    f" {left.type.dsl_name} and {right.type.dsl_name})",
                    location=expr.f_op,
                )
                return E.OrderingTestExpr.make_compare_nodes(
                    dbg_info, self.prop, operator, left, right
                )

            # Otherwise, expect strict equality for both operands and use the
            # native comparison operator for code generation.
            check_source_language(
                left.type == right.type,
                "Comparisons require the same type for both operands"
                f" (got {left.type.dsl_name} and {right.type.dsl_name})",
                location=expr.f_op,
            )
            return E.OrderingTestExpr(dbg_info, operator, left, right)

        elif isinstance(expr.f_op, L.OpAnd):
            dbg_info = debug_info(expr, "BooleanAnd")
            expr_type_matches(expr.f_left, left, T.Bool)
            expr_type_matches(expr.f_right, right, T.Bool)
            return E.IfExpr(
                dbg_info, left, right, E.LiteralExpr(None, "False", T.Bool)
            )

        elif isinstance(expr.f_op, L.OpOr):
            dbg_info = debug_info(expr, "BooleanOr")
            expr_type_matches(expr.f_left, left, T.Bool)
            expr_type_matches(expr.f_right, right, T.Bool)
            return E.IfExpr(
                dbg_info, left, E.LiteralExpr(None, "True", T.Bool), right
            )

        elif isinstance(expr.f_op, L.OpLogicAnd):
            dbg_info = debug_info(expr, "LogicAnd")
            expr_type_matches(expr.f_left, left, T.Equation)
            expr_type_matches(expr.f_right, right, T.Equation)
            return E.CallExpr(
                dbg_info,
                "And_Pred",
                "Create_And",
                T.Equation,
                [left, right, E.sloc_info_arg(dbg_info.location)],
            )

        elif isinstance(expr.f_op, L.OpLogicOr):
            dbg_info = debug_info(expr, "LogicOr")
            expr_type_matches(expr.f_left, left, T.Equation)
            expr_type_matches(expr.f_right, right, T.Equation)
            return E.CallExpr(
                dbg_info,
                "Or_Pred",
                "Create_Or",
                T.Equation,
                [left, right, E.sloc_info_arg(dbg_info.location)],
            )

        elif isinstance(expr.f_op, L.OpOrInt):
            # Create a variable to store the evaluation of the left operand
            assert self.local_vars is not None
            left_var = self.local_vars.create(
                Location.builtin, "Left_Var", type=left.type
            )

            then_expr, default_expr = left_var.ref_expr.unify(
                right, expr.f_op, "or? expression"
            )

            # Use a Then construct to conditionally evaluate (and return) the
            # right operand if the left one turns out to be null.
            return E.ThenExpr(
                debug_info=debug_info(expr, "or?"),
                expr=left,
                var_expr=left_var.ref_expr,
                then_expr=then_expr,
                default_expr=default_expr,
            )

        elif isinstance(expr.f_op, L.OpAmp):
            if left.type == T.Symbol and right.type == T.Symbol:
                return E.BasicExpr(
                    debug_info(expr, "SymbolConcat"),
                    "Sym_Concat",
                    "Find (Self.Unit.TDH.Symbols, ({}.all & {}.all))",
                    T.Symbol,
                    [left, right],
                )

            elif left.type == T.String and right.type == T.String:
                return E.CallExpr(
                    debug_info(expr, "StringConcat"),
                    "Concat_Result",
                    "Concat_String",
                    T.String,
                    [left, right],
                )

            elif left.type.is_array_type and right.type == left.type:
                return E.CallExpr(
                    debug_info(expr, "ArrayConcat"),
                    "Concat_Result",
                    "Concat",
                    left.type,
                    [left, right],
                )
            else:
                error(
                    f"invalid concatenation operands: {left.type.dsl_name} and"
                    f" {right.type.dsl_name}",
                    location=expr.f_op,
                )

        else:
            ada_operator = {
                L.OpPlus: "+",
                L.OpMinus: "-",
                L.OpMult: "*",
                L.OpDiv: "/",
            }[type(expr.f_op)]
            dbg_info = debug_info(expr, f"Arithmetic {ada_operator!r}")

            check_source_language(
                left.type == right.type,
                f"Incompatible types for {ada_operator}: {left.type.dsl_name}"
                f" and {right.type.dsl_name}",
                location=expr.f_op,
            )
            check_source_language(
                left.type in (T.Int, T.BigInt),
                f"Invalid type for {ada_operator}: {left.type.dsl_name}",
                location=expr.f_op,
            )

            return E.BasicExpr(
                dbg_info,
                "Arith_Result",
                f"({{}} {ada_operator} {{}})",
                left.type,
                [left, right],
                requires_incref=False,
            )

    def lower_block_expr(self, expr: L.BlockExpr, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        assert self.local_vars is not None
        loc = Location.from_lkt_node(expr)
        sub_env = env.create_child(
            f"scope for block at {loc.gnu_style_repr()}"
        )
        with self.local_vars.current_scope.new_child() as inner_scope:
            # Go through all declarations/bindings in the source order
            actions: list[DeclAction] = []
            for v in expr.f_val_defs:
                v_loc = Location.from_lkt_node(v)
                scope_var: Scope.UserValue

                if isinstance(v, L.ValDecl):
                    # Create the local variable for this declaration
                    source_name = v.f_syn_name.text
                    source_name, v_name = extract_var_name(
                        self.ctx, v.f_syn_name
                    )
                    init_expr = self.lower_expr(v.f_expr, sub_env)
                    v_type = (
                        self.resolver.resolve_type(v.f_decl_type, env)
                        if v.f_decl_type
                        else init_expr.type
                    )
                    local_var = self.local_vars.create(
                        v_loc, v_name, v_type, source_name
                    )
                    scope_var = Scope.LocalVariable(
                        source_name, v, local_var.ref_expr
                    )
                    actions.append(
                        DeclAction(
                            location=v_loc,
                            local_var=local_var,
                            init_expr=init_expr,
                            init_expr_node=v.f_expr,
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

                    # Create a local variable in the generated code to hold the
                    # binding value.
                    local_var = self.local_vars.create(
                        v_loc, dyn_var.name, dyn_var.type
                    )
                    scope_var = Scope.BoundDynVar(
                        v.f_name.text, v, local_var.ref_expr, dyn_var
                    )
                    actions.append(
                        DynVarBindAction(
                            location=v_loc,
                            dynvar=dyn_var,
                            local_var=local_var,
                            init_expr=self.lower_expr(v.f_expr, sub_env),
                            init_expr_node=v.f_expr,
                            binding_token=dyn_var.push_binding(local_var),
                        )
                    )

                else:
                    assert False, f"Unhandled def in BlockExpr: {v}"

                # Make the declared value/dynamic variable available to the
                # remaining expressions.
                sub_env.add(scope_var)

            # Lower the block main expression and wrap it in declarative blocks
            result = self.lower_expr(expr.f_expr, sub_env)
            for action in reversed(actions):
                if isinstance(action, DynVarBindAction):
                    # Disable the binding of this dynamic variable for the
                    # compilation of the rest of the expression tree.
                    action.dynvar.pop_binding(action.binding_token)

                    # Emit a warning if this binding is useless because the
                    # rest of the block does not use the dynamic variable
                    # binding.
                    local_binding = action.local_var.ref_expr

                    def is_expr_using_self(expr: object) -> bool:
                        """
                        Return True iff ``expr`` is a reference to this
                        binding.
                        """
                        return expr == local_binding

                    def traverse_expr(expr: E.Expr) -> bool:
                        if len(expr.flat_subexprs(is_expr_using_self)) > 0:
                            return True

                        for subexpr in expr.flat_actual_subexprs():
                            if traverse_expr(subexpr):
                                return True

                        return False

                    WarningSet.unused_bindings.warn_if(
                        not is_expr_using_self(result)
                        and not traverse_expr(result),
                        "Useless bind of dynamic var"
                        f" '{action.dynvar.dsl_name}'",
                        location=action.location,
                    )

                    result = E.DynamicVariableBindExpr(
                        E.ExprDebugInfo("bind", action.location),
                        action.dynvar,
                        action.local_var,
                        E.maybe_cast(
                            action.init_expr_node,
                            action.init_expr,
                            action.dynvar.type,
                        ),
                        result,
                    )
                else:
                    result = E.LetExpr(
                        E.ExprDebugInfo("ValDecl", action.location),
                        [(action.local_var.ref_expr, action.init_expr)],
                        result,
                    )

        # Wrap the Let expression in a binding scope expression so that
        # local variables created in inner_scope are finalized once execution
        # leaves the Let expression.
        return E.BindingScope(None, result, [], inner_scope)

    def lower_call_expr(
        self,
        expr: L.CallExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        call_name = expr.f_name

        # Depending on its name, a call can have different meanings...

        # If it is a simple identifier...
        if isinstance(call_name, L.RefId):
            entity = self.resolver.resolve_entity(call_name, env)

            # It can be a call to a built-in function
            if entity == self.dynamic_lexical_env:
                self.abort_if_static_required(expr)
                assert self.prop

                args, _ = S.dynamic_lexical_env_signature.match(self.ctx, expr)

                # Make sure this expression is allowed in the current
                # expression context.
                check_source_language(
                    self.prop.lazy_field,
                    "Dynamic lexical environment creation can only happen"
                    " inside a lazy field initializer",
                    location=expr,
                )

                # Sanitize assocs_getter: make sure we have a property
                # reference, then make sure it has the expected signature.

                assocs_getter_ref = args["assocs"]
                assocs_getter = self.resolver.resolve_property(
                    assocs_getter_ref
                ).root
                assocs_getter.require_untyped_wrapper()

                expected_rtype = T.InnerEnvAssoc.array
                check_source_language(
                    assocs_getter.type.matches(expected_rtype),
                    '"assocs_getter" must return an array of '
                    f"{expected_rtype.element_type.dsl_name} (got"
                    f" {assocs_getter.type.dsl_name})",
                    location=assocs_getter_ref,
                )
                check_source_language(
                    not assocs_getter.arguments,
                    '"assocs_getter" cannot accept arguments',
                    location=assocs_getter_ref,
                )

                # Likewise for assoc_resolver, is present
                assoc_resolver_ref = args.get("assoc_resolver")
                assoc_resolver: PropertyDef | None = None
                if assoc_resolver_ref:
                    assoc_resolver = self.resolver.resolve_property(
                        assoc_resolver_ref
                    ).root
                    assoc_resolver.require_untyped_wrapper()

                    check_source_language(
                        assoc_resolver.type.matches(T.entity),
                        f'"assoc_resolver" must return a {T.entity.dsl_name}'
                        f" (got {assoc_resolver.type.dsl_name})",
                        location=assoc_resolver_ref,
                    )
                    check_source_language(
                        not assoc_resolver.arguments,
                        '"assoc_resolver" cannot accept arguments',
                        location=assoc_resolver_ref,
                    )

                # Should this environment has a transitive parent?
                trans_parent_expr = args.get("transitive_parent")
                if trans_parent_expr:
                    transitive_parent = self.lower_expr(trans_parent_expr, env)
                    expr_type_matches(
                        trans_parent_expr, transitive_parent, T.Bool
                    )
                else:
                    transitive_parent = E.BooleanLiteralExpr(None, True)

                return E.DynamicLexicalEnvExpr(
                    debug_info(expr, "DynamicLexicalEnv"),
                    assocs_getter,
                    assoc_resolver,
                    transitive_parent,
                )

            # It can be a New expression
            elif isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                return self.lower_new(entity.t, expr, env)

            # Everything else is illegal
            error("invalid call prefix", location=call_name)

        # If the call name is a generic instantiation, it has to be a reference
        # to a struct type, and thus the call is a New expression.
        elif isinstance(call_name, L.GenericInstantiation):
            self.abort_if_static_required(expr)

            generic = self.resolver.resolve_generic(call_name.f_name, env)
            type_args = call_name.f_args
            if generic != self.generics.entity:
                error(
                    f"only {self.generics.entity.name} is the only legal"
                    " generic in this context",
                    location=call_name,
                )
            check_source_language(
                len(type_args) == 1,
                f"{generic.name} expects one type argument: the node type",
                location=type_args,
            )

            node_arg = type_args[0]
            node_type = self.resolver.resolve_node(node_arg, env)
            return self.lower_new(node_type.entity, expr, env)

        # Otherwise the call has to be a dot expression, for a method
        # invocation.
        elif not isinstance(call_name, L.DotExpr):
            error("invalid call prefix", location=call_name)

        self.abort_if_static_required(expr)
        return self.lower_method_call(expr, checks, env)

    def lower_cast_expr(
        self,
        expr: L.CastExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        self.abort_if_static_required(expr)

        if expr.f_null_cond.p_as_bool:
            error(
                "The null-conditional operator is not allowed on .as",
                location=expr.f_null_cond,
            )

        subexpr = self.lower_expr(expr.f_expr, env)
        excludes_null = expr.f_excludes_null.p_as_bool
        dest_type = self.resolve_cast_type(
            subexpr.type,
            expr.f_dest_type,
            env,
            upcast_allowed=True,
        )
        return E.CastExpr(
            debug_info(expr, "Cast"),
            subexpr,
            dest_type,
            do_raise=excludes_null,
        )

    def analyze_collection_expr(
        self,
        collection_expr: E.Expr,
        location: Location,
    ) -> CollectionAnalysisResult:
        """
        Analyze an expression that is supposed to evaluate to a collection.

        This emits an error if the expression actually does not evaluate to a
        colection. Otherwise, it computes various commonly needed attributes
        for the collection: see the ``CollectionAnalysisResult`` class.

        :param collection_expr: Expression to analyze.
        :param location: Location for the errors that may be emitted.
        """
        # Determine the type for the actual collection (i.e. strip the optional
        # entity layer).
        coll_type = collection_expr.type
        with_entity = collection_expr.type.is_entity_type
        if isinstance(coll_type, EntityType):
            with_entity = True
            coll_type = coll_type.element_type
        else:
            with_entity = False

        # Check that we indeed have a collection and determine the element
        # types.
        is_collection = True
        if isinstance(coll_type, ArrayType):
            codegen_elt_type = user_elt_type = coll_type.element_type
        elif isinstance(coll_type, (ASTNodeType, EntityType)):
            is_collection = coll_type.is_list_type
            if is_collection:
                user_elt_type = coll_type.element_type
            codegen_elt_type = T.root_node
        else:
            is_collection = False

        if not is_collection:
            error(
                f"Collection expected, got {coll_type.dsl_name}",
                location=location,
            )

        if with_entity:
            assert isinstance(user_elt_type, ASTNodeType)
            user_elt_type = user_elt_type.entity

        return CollectionAnalysisResult(
            collection_expr,
            coll_type,
            codegen_elt_type,
            user_elt_type,
            with_entity,
        )

    def lower_collection_iter(
        self,
        location: Location,
        collection_info: CollectionAnalysisResult,
        inner_scope: E.LocalVars.Scope,
        inner_expr: E.Expr,
        element_var: LocalVars.LocalVar,
        index_var: E.LocalVars.LocalVar | None = None,
    ) -> E.BaseCollectionExpr.ConstructCommonResult:
        iter_vars: list[E.InitializedVar] = []
        assert self.local_vars

        # Because of the discrepancy between the storage type in list nodes
        # (always root nodes) and the element type that user code deals with
        # (non-root list elements and/or entities), we may need to introduce
        # variables and initializing expressions. This is what the code below
        # does.
        collection_expr = collection_info.expr

        # If the collection is actually an entity, unwrap the bare list node
        # and save the entity info for later.
        if collection_info.with_entity:
            saved_entity_coll_expr, collection_expr, entity_info = (
                collection_expr.destructure_entity()
            )
            collection_expr = E.SequenceExpr(
                None, saved_entity_coll_expr, collection_expr
            )

        # Now that potential entity types are unwrapped, we can look for the
        # collection element type.
        user_element_var = element_var
        user_element_var.consolidate_type(
            collection_info.user_element_type,
            "unexpected type for the iteration variable",
            location,
        )
        iter_vars.append(E.InitializedVar(user_element_var))

        # Node lists contain bare nodes: if the user code deals with entities,
        # create a variable to hold a bare node and initialize the user
        # variable using it.
        if collection_info.with_entity:
            entity_var = iter_vars[-1]
            node_var = self.local_vars.create(
                location=Location.builtin,
                codegen_name=(names.Name("Bare") + element_var.codegen_name),
                type=collection_info.user_element_type.element_type,
                scope=inner_scope,
            )
            entity_var.init_expr = E.make_as_entity(
                None, node_var.ref_expr, entity_info=entity_info
            )
            iter_vars.append(E.InitializedVar(node_var))

        # Node lists contain root nodes: if the user code deals with non-root
        # nodes, create a variable to hold the root bare node and initialize
        # the non-root node using it.
        if (
            collection_info.collection_type.is_list_type
            and not collection_info.collection_type.is_root_node
        ):
            typed_elt_var = iter_vars[-1]
            untyped_elt_var = self.local_vars.create(
                location=Location.builtin,
                codegen_name=(
                    names.Name("Untyped") + element_var.codegen_name
                ),
                type=T.root_node,
                scope=inner_scope,
            )
            typed_elt_var.init_expr = E.UncheckedCastExpr(
                untyped_elt_var.ref_expr, typed_elt_var.var.type
            )
            iter_vars.append(E.InitializedVar(untyped_elt_var))

        # Unlike all other iteration variable, the ultimate "codegen" element
        # variable is the only one that will be defined by the "for" loop in
        # Ada (the other ones must be declared as regular local variables).
        codegen_element_var = iter_vars[-1].var
        codegen_element_var.manual_decl = True

        # If requested, create the index variable
        if index_var:
            iter_vars.append(E.InitializedVar(index_var))

        return E.BaseCollectionExpr.ConstructCommonResult(
            collection_expr=collection_expr,
            codegen_element_var=codegen_element_var.ref_expr,
            user_element_var=user_element_var.ref_expr,
            index_var=None if index_var is None else index_var.ref_expr,
            iter_vars=iter_vars,
            inner_expr=inner_expr,
            inner_scope=inner_scope,
        )

    def lower_collection_subscript(
        self,
        debug_info: E.ExprDebugInfo | None,
        syn_coll_expr: L.LktNode,
        coll_expr: E.Expr,
        syn_index_expr: L.Expr,
        bounds_resilient: bool,
        env: Scope,
    ) -> E.Expr:
        assert self.prop is not None

        # Index yields a 0-based index and all the Get primitives expect
        # 0-based indexes, so there is no need to fiddle indexes here.
        index_expr = self.lower_expr(syn_index_expr, env)
        expr_type_matches(syn_index_expr, index_expr, T.Int)

        if isinstance(coll_expr.type, EntityType):
            is_entity = True
            saved_coll_expr, coll_expr, entity_info = (
                coll_expr.destructure_entity()
            )
        else:
            is_entity = False

        check_source_language(
            coll_expr.type.is_collection,
            f"Collection expected, got {coll_expr.type.dsl_name} instead",
            location=syn_coll_expr,
        )

        # If the collection is a list node, ensure it is not null
        if isinstance(coll_expr.type, ASTNodeType):
            coll_expr = E.NullCheckExpr(coll_expr)
        elif isinstance(coll_expr.type, EntityType):
            coll_expr = E.NullCheckExpr(coll_expr, implicit_deref=True)

        or_null_expr = E.BooleanLiteralExpr(None, bounds_resilient)
        result: E.Expr = E.CallExpr(
            None,
            "Get_Result",
            "Get",
            coll_expr.type.element_type,
            [self.prop.node_var.ref_expr, coll_expr, index_expr, or_null_expr],
        )

        if is_entity:
            result = E.SequenceExpr(
                None,
                saved_coll_expr,
                E.make_as_entity(None, result, entity_info),
            )

        result.debug_info = debug_info
        return result

    def lower_concat_rebindings(
        self,
        expr: L.CallExpr,
        syn_prefix: L.Expr,
        prefix: E.Expr,
        env: Scope,
    ) -> E.Expr:
        args, _ = S.concat_rebindings_signature.match(self.ctx, expr)
        other = self.lower_expr(args["rebindings"], env)

        expr_type_matches(syn_prefix, prefix, T.EnvRebindings)
        expr_type_matches(args["rebindings"], other, T.EnvRebindings)
        return E.make_concat_rebindings(
            debug_info(expr, ".concat_rebindings"), prefix, other
        )

    def lower_dot_expr(
        self,
        expr: L.DotExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        null_cond = expr.f_null_cond.p_as_bool

        # Dotted expressions can designate an enum value (if the prefix is a
        # type name) or a member access.
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
                        return entity.t.resolve_value(
                            debug_info(expr, "enum value"), expr.f_suffix.text
                        )
                    except KeyError:
                        error("no such enum value", location=expr.f_suffix)

        # Otherwise, the prefix is a regular expression, so this dotted
        # expression is an access to a member.
        self.abort_if_static_required(expr)
        assert self.prop

        syn_prefix = expr.f_prefix
        prefix = self.lower_prefix_expr(
            expr=syn_prefix,
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
            error(
                "this is a builtin method, it should be called",
                location=expr.f_suffix,
            )

        # Handle accesses to builtin attributes and regular field
        # access separately.
        try:
            builtin = BuiltinAttribute[suffix]
        except KeyError:
            return self.lower_field_access(
                expr,
                prefix,
                null_cond,
                suffix,
                args=None,
                env=env,
                is_super=False,
            )

        dbg_info = debug_info(expr, f".{suffix}")

        if builtin == BuiltinAttribute.as_bare_entity:
            result = E.make_as_entity(
                dbg_info, prefix, entity_info=E.NullExpr(None, T.EntityInfo)
            )
            result.create_result_var("Ent")
            return result

        elif builtin == BuiltinAttribute.as_entity:
            check_source_language(
                self.prop._uses_entity_info is not False,
                "This property has been explicitly tagged as not using entity"
                " info, so .as_entity is invalid here",
                location=expr,
            )
            expr_type_matches(syn_prefix, prefix, T.root_node)

            result = E.make_as_entity(dbg_info, prefix)
            result.create_result_var("Ent")
            return result

        elif builtin == BuiltinAttribute.children:
            check_source_language(
                prefix.type.is_ast_node or prefix.type.is_entity_type,
                f'Invalid prefix for "children": got {prefix.type.dsl_name}'
                " but AST node or entity expected",
                location=expr.f_suffix,
            )

            return E.build_field_access(
                dbg_info,
                prefix,
                "children",
                [],
                lambda: E.CallExpr(
                    dbg_info,
                    "Node_Children",
                    "Children",
                    T.root_node.array,
                    [E.NullCheckExpr(prefix)],
                ),
            )

        elif builtin == BuiltinAttribute.env_node:
            expr_type_matches(syn_prefix, prefix, T.LexicalEnv)
            return E.CallExpr(
                dbg_info,
                "Env_Node",
                "AST_Envs.Env_Node",
                T.root_node,
                [prefix],
            )

        elif builtin == BuiltinAttribute.env_parent:
            expr_type_matches(syn_prefix, prefix, T.LexicalEnv)
            return E.CallExpr(
                dbg_info,
                "Env_Parent",
                "AST_Envs.Parent",
                T.LexicalEnv,
                [prefix],
            )

        elif builtin == BuiltinAttribute.is_null:
            check_source_language(
                prefix.type.null_allowed,
                f"Prefix must have a nullable type, {prefix.type.dsl_name} is"
                " not",
                location=expr.f_suffix,
            )
            return E.make_is_null(dbg_info, prefix)

        elif builtin == BuiltinAttribute.parent:
            check_source_language(
                prefix.type.is_ast_node or prefix.type.is_entity_type,
                f'Invalid prefix for "parent": got {prefix.type.dsl_name} but'
                " AST node or entity expected",
                location=expr.f_suffix,
            )

            return E.build_field_access(
                dbg_info,
                prefix,
                "parent",
                [],
                lambda: E.FieldAccessExpr(
                    dbg_info,
                    prefix,
                    "Parent",
                    T.root_node,
                    do_explicit_incref=False,
                ),
            )

        elif builtin == BuiltinAttribute.symbol:
            if isinstance(prefix.type, ASTNodeType):
                check_source_language(
                    prefix.type.is_token_node,
                    "Token node expected, but the input"
                    f" {prefix.type.dsl_name} node is not a token node",
                    location=expr.f_suffix,
                )
            elif isinstance(prefix.type, EntityType):
                prefix = E.FieldAccessExpr(
                    None,
                    prefix,
                    "Node",
                    prefix.type.element_type,
                    do_explicit_incref=False,
                )
            else:
                error(
                    "Token node expected, but got instead"
                    f" {prefix.type.dsl_name}",
                    location=expr.f_suffix,
                )

            return E.make_node_to_symbol(dbg_info, prefix)

        elif builtin == BuiltinAttribute.to_symbol:
            expr_type_matches(syn_prefix, prefix, T.String)
            return E.CallExpr(
                dbg_info,
                "Sym",
                "String_To_Symbol",
                T.Symbol,
                [self.prop.node_var.ref_expr, "Self.Unit.Context", prefix],
            )

        else:
            raise AssertionError(f"unhandled builtin: {builtin.name}")

    def lower_eq(
        self,
        debug_info: E.ExprDebugInfo | None,
        error_location: Location | L.LktNode,
        lhs: E.Expr,
        rhs: E.Expr,
    ) -> E.Expr:

        def check_type_compatibility(is_valid: bool) -> None:
            check_source_language(
                is_valid,
                f"Incompatible types for equality: {lhs.type.dsl_name} and"
                f" {rhs.type.dsl_name}",
                location=error_location,
            )

        def check_never_equal(can_be_equal: bool) -> None:
            check_source_language(
                can_be_equal,
                f"{lhs.type.dsl_name} and {rhs.type.dsl_name} values are never"
                " equal",
                location=error_location,
            )

        # Don't use CompiledType.matches since in the generated code, we need
        # both operands to be *exactly* the same types, so handle specifically
        # each case.
        if isinstance(lhs.type, ASTNodeType):
            check_type_compatibility(rhs.type.is_ast_node)
            assert isinstance(rhs.type, ASTNodeType)

            # Handle checks between two subclasses without explicit casts. In
            # order to help users to detect dubious checks, forbid operands
            # that can never be equal because they have no subclass in common.
            if lhs.type.matches(rhs.type):
                lhs = E.CastExpr(None, lhs, rhs.type)
            elif rhs.type.matches(lhs.type):
                assert isinstance(lhs.type, ASTNodeType)
                rhs = E.CastExpr(None, rhs, lhs.type)
            else:
                check_never_equal(False)

        # Likewise for entities. Moreover, we need to use a special comparison
        # predicate for them.
        elif lhs.type.is_entity_type:
            check_type_compatibility(rhs.type.is_entity_type)
            check_never_equal(
                lhs.type.element_type.matches(rhs.type.element_type)
                or rhs.type.element_type.matches(lhs.type.element_type)
            )

        else:
            check_type_compatibility(lhs.type == rhs.type)

        return E.make_eq_expr(debug_info, lhs, rhs)

    def lower_field_access(
        self,
        expr: L.DotExpr | L.CallExpr,
        prefix: E.Expr,
        null_cond: bool,
        suffix: str,
        args: (
            tuple[
                list[tuple[L.Argument, E.Expr]],
                dict[str, tuple[L.Argument, E.Expr]],
            ]
            | None
        ),
        env: Scope,
        is_super: bool,
    ) -> E.Expr:
        assert self.prop

        if isinstance(expr, L.DotExpr):
            syn_suffix = expr.f_suffix
        else:
            assert isinstance(expr.f_name, L.DotExpr)
            syn_suffix = expr.f_name.f_suffix
            call_parens = call_parens_loc(expr)

        # Look up the accessed field: by name for regular field accesses, or
        # the base property if this is a "super" call.
        implicit_deref = False
        node_data: AbstractNodeData
        if is_super:
            # Super calls are necessarily done on "node" or "self". If the
            # entity is a prefix, we necessarily have an implicit dereference.
            valid_prefix = False
            if isinstance(prefix, E.VariableExpr):
                if prefix.local_var is self.prop.node_var:
                    valid_prefix = True
                    implicit_deref = False
                elif (
                    self.prop.has_self_var
                    and prefix.local_var is self.prop.self_var
                ):
                    valid_prefix = True
                    implicit_deref = True
            if not valid_prefix:
                error(
                    ".super() is allowed on Self or Entity only",
                    location=syn_suffix,
                )

            prop = self.prop.base
            if prop is None:
                error(
                    "There is no overridden property to call",
                    location=syn_suffix,
                )
            if prop.abstract:
                error(
                    "Cannot call abstract overridden property",
                    location=syn_suffix,
                )
            prop.called_by_super = True
            node_data = prop
        else:
            # Try the most common case: accessing a member that belongs to the
            # prefix type.
            member = prefix.type.get_abstract_node_data_dict().get(
                suffix, None
            )

            # If not found, maybe the receiver is an entity, in which case we
            # want to do implicit dereference.
            if member is None and isinstance(prefix.type, EntityType):
                implicit_deref = True
                member = (
                    prefix.type.element_type.get_abstract_node_data_dict().get(
                        suffix, None
                    )
                )

            # If still not found, we have a problem
            if member is None:
                error(
                    f"Type {prefix.type.dsl_name} has no '{suffix}' field or"
                    " property",
                    location=syn_suffix,
                )
            node_data = member

        check_source_language(
            not node_data.is_internal,
            "{node_data.qualname} is for internal use only",
            location=syn_suffix,
        )

        actual_node_data = node_data

        # If this is a property call, and this is not a "super" call, actually
        # call the root property, as it will be turned into a dispatcher.
        if isinstance(actual_node_data, PropertyDef):
            if not is_super:
                actual_node_data = actual_node_data.root

            # Reject the call syntax for 1) lazy fields and 2) properties with
            # the "property" annotation, and mandate it for all the other
            # properties.
            if actual_node_data.lazy_field:
                if args is not None:
                    error("cannot call a lazy field", location=call_parens)

            elif actual_node_data.has_property_syntax:
                if args is not None:
                    error(
                        "argument list forbidden with @property",
                        location=call_parens,
                    )

            elif args is None:
                error(
                    "call syntax is mandatory for properties",
                    location=syn_suffix,
                )
        elif args is not None:
            # Reject the call syntax for anything that is not a property
            error("cannot call a field", location=call_parens)

        # Try to associate passed arguments with each natural argument in the
        # `node_data` property. If invalid count or invalid argument names
        # are detected, raise the appropriate user diagnostic.
        #
        # On success, return a list with all actuals and arg keyword/position
        # to pass in the same order as natural arguments in the spec. None
        # values are left for arguments that must be passed default values.
        resolved_args: list[E.Expr | None] = []
        if args:
            pos_args, kw_args = args
            for arg_spec in node_data.natural_arguments:
                actual_expr: E.Expr | None

                # Look for a keyword argument corresponding to `arg_spec`
                arg_name = arg_spec.name.lower
                try:
                    param, actual_expr = kw_args.pop(arg_name)
                except KeyError:
                    # There is no keyword argument passed for this argument, so
                    # pick the first remaining one from positional arguments
                    # or, if there is no positional argument left, fallback to
                    # the default argument.
                    if pos_args:
                        param, actual_expr = pos_args.pop(0)
                    else:
                        check_source_language(
                            arg_spec.default_value is not None,
                            f"Missing actual for argument {arg_name}",
                            location=expr,
                        )
                        # Don't pass the argument explicitly: let Ada pass the
                        # default one instead.
                        actual_expr = None

                if actual_expr is None:
                    resolved_args.append(None)
                else:
                    assert param is not None
                    resolved_args.append(
                        E.maybe_cast(
                            param.f_value,
                            actual_expr,
                            arg_spec.type,
                        )
                    )

            # At this point, we managed to find an actual for all arguments, so
            # all remaining passed arguments are unexpected.
            if pos_args:
                param, _ = pos_args[0]
                error("Unexpected argument", location=param)
            for param, _ in kw_args.values():
                error("Unexpected keyword argument", location=param)

        dbg_info = debug_info(expr, node_data.qualname)

        # If this field overrides expression construction, delegate it to the
        # corresponding callback.
        if node_data.access_constructor:
            # This hooks is useful for builtin members only, and builtin
            # members cannot be overriden, so "is_super" should neven be true
            # here.
            assert not is_super

            return node_data.access_constructor(
                dbg_info, prefix, actual_node_data, resolved_args
            )
        else:
            # Check that the callee's dynamic variables are bound here
            if isinstance(node_data, PropertyDef):
                E.DynamicVariable.check_call_bindings(
                    syn_suffix, node_data, "In call to {prop}"
                )

            return E.EvalMemberExpr(
                dbg_info,
                receiver_expr=prefix,
                node_data=node_data,
                arguments=resolved_args,
                actual_node_data=actual_node_data,
                implicit_deref=implicit_deref,
                is_super=is_super,
            )

    def lower_if_expr(self, expr: L.IfExpr, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

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
        result = self.lower_expr(expr.f_else_expr, env)
        conditions = [(expr.f_cond_expr, expr.f_then_expr)] + [
            (alt.f_cond_expr, alt.f_then_expr) for alt in expr.f_alternatives
        ]
        for cond_expr, then_expr in reversed(conditions):
            cond = self.lower_expr(cond_expr, env)
            expr_type_matches(cond_expr, cond, T.Bool)

            # Do not forget to unify types for the then/else expressions, so
            # that the IF expression returns a single type.
            unified_then, unified_else = self.lower_expr(then_expr, env).unify(
                result, expr, "if expression"
            )

            result = E.IfExpr(
                debug_info(expr, "If"), cond, unified_then, unified_else
            )
        return result

    def lower_is_a(self, expr: L.Isa, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        subexpr = self.lower_expr(expr.f_expr, env)
        node_types: list[ASTNodeType] = []
        for type_ref in expr.f_dest_type:
            node_types.append(
                self.resolve_cast_type(
                    subexpr.type,
                    type_ref,
                    env,
                    upcast_allowed=False,
                )
            )
        return E.IsAExpr(debug_info(expr, "IsA"), subexpr, node_types)

    def lower_keep(
        self,
        expr: L.KeepExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        self.abort_if_static_required(expr)
        assert self.local_vars

        loc = Location.from_lkt_node(expr)
        with_check = expr.f_null_cond.p_as_bool
        coll_expr = self.lower_prefix_expr(
            expr.f_expr, checks, with_check, env
        )
        coll_info = self.analyze_collection_expr(coll_expr, loc)
        keep_type = self.resolve_cast_type(
            coll_info.user_element_type,
            expr.f_keep_type,
            env,
            upcast_allowed=False,
        )
        with self.local_vars.current_scope.new_child() as inner_scope:
            element_var = self.local_vars.create(
                Location.builtin, "Item", coll_info.user_element_type
            )
        r = self.lower_collection_iter(
            location=loc,
            collection_info=coll_info,
            inner_expr=E.CastExpr(None, element_var.ref_expr, keep_type),
            element_var=element_var,
            inner_scope=inner_scope,
        )
        return E.MapExpr(
            debug_info(expr, "Keep"),
            r,
            E.IsAExpr(None, r.user_element_var, [keep_type]),
        )

    def lower_logic_assign(self, expr: L.LogicAssign, env: Scope) -> E.Expr:
        dest_var_expr = self.lower_logic_var_ref(expr.f_dest_var, env)
        value_expr = self.lower_expr(expr.f_value, env)
        if value_expr.type.matches(T.LogicVar):
            error(
                "Assigning from a logic variable is forbidden: use unify"
                " instead",
                location=expr.f_value,
            )
        expr_type_matches(expr.f_value, value_expr, T.root_node.entity)

        # Because of Ada OOP typing rules, for code generation to work
        # properly, make sure the value to assign to the logic variable is a
        # root node entity.
        if value_expr.type is not T.root_node.entity:
            value_expr = E.CastExpr(None, value_expr, T.root_node.entity)

        return E.AssignExpr(
            debug_info(expr, "LogicAssign"),
            expr,
            dest_var_expr,
            value_expr,
            E.construct_logic_ctx(),
        )

    def lower_logic_expr(self, expr: L.LogicExpr, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)
        inner_expr = expr.f_expr
        loc_arg = E.sloc_info_arg(Location.from_lkt_node(expr))

        if isinstance(inner_expr, L.RefId):
            if inner_expr.text in ("false", "true"):
                val_name = inner_expr.text.capitalize()
                return E.CallExpr(
                    debug_info(expr, f"Logic{val_name}"),
                    f"{val_name}_Rel",
                    f"Solver.Create_{val_name}",
                    T.Equation,
                    [loc_arg],
                )

        elif isinstance(inner_expr, L.CallExpr):
            call_name = inner_expr.f_name
            if not isinstance(call_name, L.RefId):
                error("invalid logic expression", location=expr)

            if call_name.text in ("all", "any"):
                _, subeq_exprs = S.logic_all_any_signature.match(
                    self.ctx, inner_expr
                )
                op_kind = "And" if call_name.text == "all" else "Or"
                check_source_language(
                    bool(subeq_exprs),
                    "at least one equation expected",
                    location=expr,
                )
                subeqs = []
                for a in inner_expr.f_args:
                    e = self.lower_expr(a.f_value, env)
                    expr_type_matches(a, e, T.Equation)
                    subeqs.append(e)

                # Turn all sub-equations into a degenerate tree of And/Or
                # aggregate predicates.
                result = subeqs.pop()
                for item in reversed(subeqs):
                    result = E.CallExpr(
                        None,
                        f"{op_kind}_Pred",
                        f"Create_{op_kind}",
                        T.Equation,
                        [item, result, loc_arg],
                    )

                # Assign debug info only for the root "and'ed"/"or'ed"
                # predicate so that the debugger steps only once inside.
                if result.debug_info is None:
                    result.debug_info = debug_info(expr, f"Logic{op_kind}")
                return result

            elif call_name.text == "domain":
                args, _ = S.domain_signature.match(self.ctx, inner_expr)
                logic_var = self.lower_logic_var_ref(args["var"], env)
                domain_expr = self.lower_expr(args["domain"], env)
                if not domain_expr.type.is_collection or not isinstance(
                    domain_expr.type.element_type, (ASTNodeType, EntityType)
                ):
                    error(
                        "Entity or bare node collection expected, got"
                        f" {domain_expr.type.dsl_name}",
                        location=args["domain"],
                    )
                return E.DomainExpr(
                    debug_info(expr, "LogicDomain"), domain_expr, logic_var
                )

        error("invalid logic expression", location=expr)

    def lower_logic_predicate(
        self,
        expr: L.LogicPredicate,
        env: Scope,
    ) -> E.Expr:
        reject_param_names(expr.f_args, "logic predicates")
        args = [self.lower_expr(arg.f_value, env) for arg in expr.f_args]
        if len(args) == 0:
            error("at least one argument expected", location=expr.f_args)

        is_variadic, logic_var_args, captured_args = (
            detect_variadic_logic_properties(expr, args)
        )
        check_source_language(
            len(logic_var_args) > 0,
            "Predicate instantiation should have at least one logic variable"
            " expression",
            location=expr,
        )

        # Check the property return type
        prop = self.resolver.resolve_property(expr.f_name)
        check_source_language(
            prop.type.matches(T.Bool),
            "Predicate property must return a boolean, got"
            f" {prop.type.dsl_name}",
            location=expr.f_name,
        )

        pred_id, captured_args = E.create_property_closure(
            expr,
            prop,
            is_variadic,
            logic_var_args,
            captured_args,
            E.LogicClosureKind.Predicate,
        )

        if prop.predicate_error is not None:
            error_loc_expr = E.construct_builtin_dynvar(
                self.resolver.builtins.dyn_vars.error_location.variable
            )
            if error_loc_expr is None:
                error(
                    "The error_location dynamic variable must be bound in"
                    f" order to create a predicate for {prop.qualname}",
                    location=expr.f_name,
                )
            assert error_loc_expr.type.matches(T.root_node)
            captured_args.append(error_loc_expr)

        saved_exprs: list[E.Expr] = []
        arity_expr: E.Expr | None = None

        if len(logic_var_args) > 1:
            arity_expr = E.IntegerLiteralExpr(None, len(logic_var_args))
        elif is_variadic:
            var_array_expr = E.SavedExpr(None, "Logic_Vars", logic_var_args[0])
            saved_exprs.append(var_array_expr)
            var_array = E.LiteralExpr(
                None,
                "Entity_Vars.Logic_Var_Array ({}.Items)",
                T.LogicVar.array,
                [var_array_expr.result_var_expr],
            )
            logic_var_args = [var_array]
            arity_expr = E.LiteralExpr(
                None, "{}.N", None, [var_array_expr.result_var_expr]
            )

        predicate_expr = E.logic_closure_instantiation_expr(
            f"{pred_id}_Predicate", captured_args, arity_expr
        )

        result: E.Expr = E.PredicateExpr(
            debug_info(expr, "LogicPropagate"),
            prop,
            pred_id,
            logic_var_args,
            predicate_expr,
        )

        for e in reversed(saved_exprs):
            result = E.SequenceExpr(None, e, result)

        return result

    def lower_logic_propagate(
        self,
        expr: L.LogicPropagate,
        env: Scope,
    ) -> E.Expr:
        dest_var_expr = self.lower_logic_var_ref(expr.f_dest_var, env)
        comb_prop = self.resolver.resolve_property(expr.f_call.f_name)
        logic_ctx = E.construct_logic_ctx()

        # Construct all property arguments to determine what kind of equation
        # this really is.
        reject_param_names(expr.f_call.f_args, "logic propagates")
        args = [
            self.lower_expr(arg.f_value, env) for arg in expr.f_call.f_args
        ]
        if not args:
            error(
                "At least one argument logic variable (or array thereof)"
                " expected",
                location=call_parens_loc(expr.f_call),
            )

        is_variadic, logic_var_args, captured_args = (
            detect_variadic_logic_properties(expr, args)
        )

        # If the first argument is not a logic var nor an array of logic vars,
        # this is actually a Bind: transform it now.
        if len(logic_var_args) == 0:
            comb_prop_arg = args[0]

            # Because of Ada OOP typing rules, for code generation to work
            # properly, make sure the type of the property argument is a root
            # node entity.
            if comb_prop_arg.type is not T.root_node.entity:
                comb_prop_arg = E.CastExpr(
                    None, comb_prop_arg, T.root_node.entity
                )

            return E.AssignExpr(
                debug_info(expr, "LogicAssign"),
                expr,
                dest_var_expr,
                comb_prop_arg,
                logic_ctx,
                conv_prop=comb_prop,
            )
        else:
            return E.PropagateExpr.construct_propagate(
                debug_info(expr, "LogicPropagate"),
                expr,
                dest_var=dest_var_expr,
                is_variadic=is_variadic,
                logic_var_args=logic_var_args,
                captured_args=captured_args,
                prop=comb_prop,
                logic_ctx=logic_ctx,
            )

    def lower_logic_unify(self, expr: L.LogicUnify, env: Scope) -> E.Expr:
        lhs_expr = self.lower_logic_var_ref(expr.f_lhs, env)
        expr_type_matches(expr.f_lhs, lhs_expr, T.LogicVar)

        rhs_expr = self.lower_expr(expr.f_rhs, env)
        expr_type_matches(expr.f_rhs, rhs_expr, T.LogicVar)

        return E.UnifyExpr(
            debug_info(expr, "LogicUnify"),
            lhs_expr,
            rhs_expr,
            E.construct_logic_ctx(),
        )

    def lower_logic_var_ref(self, expr: L.Expr, env: Scope) -> E.Expr:
        var_ref = self.lower_expr(expr, env)
        expr_type_matches(expr, var_ref, T.LogicVar)
        return E.ResetLogicVar(var_ref)

    def lower_match(self, expr: L.MatchExpr, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)
        assert self.prop is not None
        assert self.local_vars is not None

        outer_scope = self.prop.get_scope()

        # Determine the bare node type that is matched: the input expression
        # may be an entity.
        matched = self.lower_expr(expr.f_match_expr, env)
        if isinstance(matched.type, ASTNodeType):
            is_entity = False
            input_node = matched.type
        elif isinstance(matched.type, EntityType):
            is_entity = True
            input_node = matched.type.element_type
        else:
            error(
                "Match expressions can only work on AST nodes or entities: got"
                f" {matched.type.dsl_name} instead",
                location=expr.f_match_expr,
            )

        # Lower all match branches
        matched_types: list[tuple[L.MatchBranch, ASTNodeType]] = []
        matchers: list[E.MatchExpr.Matcher] = []
        for i, branch in enumerate(expr.f_branches):
            # Make sure the identifier has the expected casing
            spec_name, codegen_name = extract_var_name(
                self.ctx, branch.f_decl.f_syn_name
            )

            # Fetch the type to match, if any, and include it to
            # ``matched_types``.
            syn_type = branch.f_decl.f_decl_type
            if syn_type is None:
                matched_type = input_node
            else:
                # If present, the type must be either a bare node (always
                # legal) or an entity type (only if the matched expression is
                # an entity).
                #
                # The designated node must also be a subtype of the matched
                # expression.
                t = self.resolver.resolve_type(syn_type, env)
                if isinstance(t, EntityType):
                    if not is_entity:
                        error("bare node expected", location=syn_type)
                    node_type = t.element_type
                elif isinstance(t, ASTNodeType):
                    node_type = t
                else:
                    error(
                        f"Cannot match {t.dsl_name} (input type is"
                        f" {matched.type.dsl_name})",
                        location=syn_type,
                    )

                t = node_type.entity if is_entity else node_type
                check_source_language(
                    t.matches(matched.type),
                    "Cannot match {} (input type is {})".format(
                        t.dsl_name, matched.type.dsl_name
                    ),
                    location=syn_type,
                )
                matched_type = node_type

            matched_types.append((branch, matched_type))

            # Create a codegen scope so that the branch variable is contained
            # in this branch as is not exposed outside in the debug info.
            with outer_scope.new_child() as inner_scope:
                match_var = self.local_vars.create(
                    location=Location.from_lkt_node(branch.f_decl),
                    codegen_name=codegen_name,
                    spec_name=spec_name,
                    type=matched_type.entity if is_entity else matched_type,
                )

                # Create a child Lkt scope to host the match variable (if there
                # is one in the Lkt source code).
                sub_loc = Location.from_lkt_node(branch)
                sub_env = env.create_child(
                    f"scope for match branch at {sub_loc.gnu_style_repr()}"
                )
                if spec_name != "_":
                    sub_env.add(
                        Scope.UserValue(
                            spec_name, branch.f_decl, match_var.ref_expr
                        )
                    )

                # Finally, lower the expression for this branch
                branch_expr = self.lower_expr(branch.f_expr, sub_env)

                matchers.append(
                    E.MatchExpr.Matcher(match_var, branch_expr, inner_scope)
                )

        # All possible input types must have at least one matcher. Also warn if
        # some matchers are unreachable.
        type_set: TypeSet[ASTNodeType] = TypeSet()
        for branch, t in matched_types:
            if type_set.include(t):
                emit_error(
                    "This branch is unreachable as previous branches cover all"
                    " the nodes it can match",
                    location=Location.from_lkt_node_range(
                        branch, branch.f_decl
                    ),
                    severity=Severity.warning,
                )

        mm = sorted(
            type_set.unmatched_types(input_node),
            key=lambda cls: cls.hierarchical_name,
        )
        check_source_language(
            not mm,
            "The following AST nodes have no handler: {} (all {} subclasses"
            " require one)".format(
                ", ".join(t.dsl_name for t in mm), input_node.dsl_name
            ),
            location=expr,
        )

        return E.MatchExpr(debug_info(expr, "Match"), expr, matched, matchers)

    def lower_new(
        self,
        t: CompiledType,
        expr: L.CallExpr,
        env: Scope,
    ) -> E.Expr:
        # Non-struct/node types have their own constructor
        if t == T.RefCategories:
            # Compute the list of requested categories
            arg_nodes, kwarg_nodes = self.extract_call_args(expr)
            if arg_nodes:
                param, _ = arg_nodes[0]
                error(
                    "Positional arguments not allowed for RefCategories",
                    location=param,
                )

            _, default_expr = kwarg_nodes.pop("_", (None, None))
            default = (
                False
                if default_expr is None
                else parse_static_bool(self.ctx, default_expr)
            )

            all_cats = self.ctx.ref_cats
            cats = set(all_cats) if default else set()
            for key, (param, value) in kwarg_nodes.items():
                name = names.Name.from_lower(key)
                check_source_language(
                    name in all_cats,
                    f"Invalid category: {key}",
                    location=param,
                )
                if parse_static_bool(self.ctx, value):
                    cats.add(name)
                else:
                    cats.discard(name)

            return E.RefCategoriesExpr(debug_info(expr, "RefCategories"), cats)
        else:
            # This is a constructor for a struct or a node
            self.abort_if_static_required(expr)
            assert self.prop

            # Ensure calling this constructor is legal in this context
            if isinstance(t, ASTNodeType):
                if not t.synthetic:
                    error(
                        "Cannot synthetize a node that is not annotated as"
                        f" synthetic ({t.dsl_name})",
                        location=expr,
                    )

                if t.is_list_type:
                    error(
                        "List node synthetization is not supported for now",
                        location=expr,
                    )

                if not self.prop.memoized and not self.prop.lazy_field:
                    error(
                        "Node synthetization can only happen inside"
                        " memoized properties or lazy fields",
                        location=expr,
                    )

            elif not isinstance(t, BaseStructType):
                error(
                    "Invalid type, expected struct type or AST node, got"
                    f" {t.dsl_name}",
                    location=expr,
                )

            # Lower sub expressions and associate them to the fields to
            # initialize.
            args, kwargs = self.lower_call_args(expr, env)
            if args:
                param, _ = args[0]
                error(
                    "Positional arguments not allowed for struct"
                    " constructors",
                    location=param,
                )
            field_values = E.New.construct_fields(
                expr, t, {n: e for n, (_, e) in kwargs.items()}
            )

            dbg_info = debug_info(expr, f"New[{t.dsl_name}]")
            if isinstance(t, ASTNodeType):
                return E.New.NodeExpr(dbg_info, t, field_values)
            else:
                return E.New.StructExpr(dbg_info, t, field_values)

    def lower_raise_expr(self, expr: L.RaiseExpr, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        # Return types are mandatory
        if expr.f_dest_type is None:
            raise_kw = expr.token_start
            error(
                "Return type required (`raise[T]`)",
                location=Location.from_lkt_tokens(expr, raise_kw, raise_kw),
            )
        dest_type = self.resolver.resolve_type(expr.f_dest_type, env)

        # A raise expression can only contain a PropertyError struct
        # constructor.
        cons_expr = expr.f_except_expr
        if not isinstance(cons_expr, L.CallExpr):
            error(
                "'raise' must be followed by a call expression",
                location=cons_expr,
            )
        call_name = cons_expr.f_name
        entity = self.resolver.resolve_entity(call_name, env)
        if not isinstance(entity, Scope.Exception):
            error(
                f"exception expected, got {entity.diagnostic_name}",
                location=call_name,
            )

        # Get the exception message argument. TODO (S321-013): handle dynamic
        # error message.
        args, _ = S.exception_signature.match(self.ctx, cons_expr)
        msg_node = args.get("exception_message")
        msg = (
            "PropertyError exception"
            if msg_node is None
            else parse_static_str(self.ctx, msg_node)
        )

        return E.ErrorExpr(
            debug_info(expr, "RaiseException"),
            dest_type,
            names.Name.from_camel(entity.name),
            msg,
        )

    def lower_string_lit(self, expr: L.StringLit, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        prefix = expr.p_prefix
        value = denoted_str(expr)
        if prefix == "\x00":
            return E.CallExpr(
                debug_info(expr, "StringLiteral"),
                "Str",
                "Create_String",
                T.String,
                [text_repr(value)],
            )
        elif prefix == "s":
            return E.SymbolLiteralExpr(
                debug_info(expr, "SymbolLiteral"), value
            )
        else:
            error("invalid string prefix", location=expr)

    def lower_subscript_expr(
        self,
        expr: L.SubscriptExpr,
        checks: NullCond.CheckStack,
        env: Scope,
    ) -> E.Expr:
        self.abort_if_static_required(expr)

        coll_expr = self.lower_prefix_expr(
            expr.f_prefix,
            checks,
            expr.f_null_cond.p_as_bool,
            env,
        )
        return self.lower_collection_subscript(
            debug_info(expr, "Subscript"),
            expr.f_prefix,
            coll_expr,
            expr.f_index,
            bounds_resilient=False,
            env=env,
        )

    def lower_try_expr(self, expr: L.TryExpr, env: Scope) -> E.Expr:
        self.abort_if_static_required(expr)

        try_expr = self.lower_expr(expr.f_try_expr, env)
        or_expr = (
            None
            if expr.f_or_expr is None
            else self.lower_expr(expr.f_or_expr, env)
        )

        try_expr, or_expr = E.expr_or_null(
            try_expr, or_expr, expr, "Try expression", "fallback expression"
        )

        return E.TryExpr(debug_info(expr, "Try"), try_expr, or_expr)

    def resolve_cast_type(
        self,
        input_type: CompiledType,
        type_ref: L.TypeRef,
        env: Scope,
        upcast_allowed: bool,
    ) -> ASTNodeType:
        resolved_type = self.resolver.resolve_type(type_ref, env)
        input_is_entity = isinstance(input_type, EntityType)
        input_node = input_type.element_type if input_is_entity else input_type
        bad_type_msg = (
            "Bare node or entity type expected"
            if input_is_entity
            else "Bare node type expected"
        )

        # Determine the bare node type for the cast
        node_type: ASTNodeType
        match resolved_type:
            case ASTNodeType():
                node_type = resolved_type
            case EntityType():
                if not input_is_entity:
                    error(bad_type_msg, location=type_ref)
                node_type = resolved_type.element_type
            case _:
                error(bad_type_msg, location=type_ref)

        if node_type == input_node:
            emit_error(
                "Given type is the same as the input type",
                location=type_ref,
                severity=Severity.warning,
            )
        if not node_type.matches(input_node):
            if not upcast_allowed:
                error(
                    f"{input_node.dsl_name} subtype expected",
                    location=type_ref,
                )
            elif not input_node.matches(node_type):
                error(
                    f"{input_node.dsl_name} parent type or subtype expected",
                    location=type_ref,
                )

        return node_type
