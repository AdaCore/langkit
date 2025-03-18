from __future__ import annotations

import enum
from itertools import zip_longest
from typing import TYPE_CHECKING

from langkit import names
from langkit.compile_context import get_context
from langkit.compiled_types import ASTNodeType, Argument, EntityType, T
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions.base import (
    CallExpr,
    ComputingExpr,
    DynamicVariable,
    Expr,
    ExprDebugInfo,
    IntegerLiteralExpr,
    LiteralExpr,
    NullExpr,
    PropertyClosure,
    PropertyDef,
    SavedExpr,
    SequenceExpr,
    aggregate_expr,
    render,
    sloc_info_arg,
)


if TYPE_CHECKING:
    import liblktlang as L


class LogicClosureKind(enum.Enum):
    """
    The purpose of a property closure, i.e. whether it will be used in a
    predicate atom or in a propagate atom. This is mainly used to decide
    where a property closure should be registered during the
    ``create_property_closure`` routine.
    """
    Predicate = enum.auto()
    Propagate = enum.auto()


def untyped_literal_expr(
    expr_str: str,
    operands: list[Expr] = [],
) -> LiteralExpr:
    """
    Create an untyped LiteralExpr instance for "expr_str" and return it.

    This is a helper for code that generates expressions which have no
    corresponding CompiledType in Langkit. Materializing such values in
    Expr trees can be useful anayway to leverage CallExpr code generation
    capabilities, in particular temporary creation for the result.  We can do
    this because CallExpr does not need its operands' types to be valid.

    :param str expr_str: Template code for this literal expression.
    :param list[Expr] operands: Operand for this literal expression.
    :rtype: LiteralExpr
    """
    return LiteralExpr(None, expr_str, T.NoCompiledType, operands)


def construct_builtin_dynvar(dynvar: DynamicVariable) -> Expr | None:
    """
    Common logic to get a reference to a builtin dynamic variable, if bound.
    None is returned if it is unbound.
    """
    # Do not pass a logic context if the logic context builtin variable was not
    # bound.
    return dynvar.current_binding.ref_expr if dynvar.is_bound else None


def construct_logic_ctx() -> Expr | None:
    """
    Common logic to construct the logic context expression for a logic atom
    builder.
    """
    # Do not pass a logic context if the logic context builtin variable was not
    # bound.
    return construct_builtin_dynvar(
        get_context().lkt_resolver.builtins.dyn_vars.logic_context.variable
    )


def logic_closure_instantiation_expr(
    closure_name: str,
    closure_args: list[Expr],
    arity: Expr | None = None
) -> LiteralExpr:
    """
    Given the name of a property closure to be used as a logic predicate,
    converter or combiner, return an expression that instantiates this closure
    with the given partial arguments. For combiners or predicates that allow
    multiple arguments, the arity parameter must be a non-null expression that
    evaluates to the number of said arguments.
    """
    assocs: list[Expr] = []

    if arity is not None:
        assocs.append(arity)

    assocs.extend(closure_args)

    args = " ({})".format(
        ', '.join(["{}" for _ in assocs])
    ) if assocs else ""

    return untyped_literal_expr(
        f"Create_{closure_name}{args}", operands=assocs
    )


def create_property_closure(
    error_location: Location | L.LktNode,
    prop: PropertyDef,
    is_variadic: bool,
    closure_args: list[Expr],
    captured_args: list[Expr],
    kind: LogicClosureKind
) -> tuple[str, list[Expr]]:
    """
    Create and register a PropertyClosure object for the given property,
    considering the given partial arguments.

    This performs checks on all given arguments to make sure they match the
    signature of the property.

    Return the unique id representing the closure, as well as the complete
    list of partial arguments: the given ones extended with all dynamic var
    arguments, if any.

    :param error_location: Location to use when emitting errors in legality
        checks.
    :param prop: The property for which to generate a closure.
    :param is_variadic: Whether the closure accepts a single array argument.
    :param closure_args: The list of non-partial arguments.
    :param captured_args: The list of partial arguments.
    :param kind: Whether this should be registered as a predicate closure or
        as a functor closure.
    """
    from langkit.expressions import CastExpr

    prop = prop.root
    name = prop.qualname

    if not isinstance(prop.owner, ASTNodeType):
        error(f"{name} must belong to a subtype of {T.root_node.dsl_name}")

    entity_expr_count = len(closure_args)

    if is_variadic:
        # We are creating a predicate/propagate with an array of logic vars
        # so we expect a property that takes an array of entities as its first
        # parameter.
        entity_arg = prop.natural_arguments[0]
        extra_args = prop.natural_arguments[1:]
        check_source_language(
            entity_arg.type.element_type.is_entity_type,
            f"{name} property's first argument must be an array of entities,"
            f" not {entity_arg.type.dsl_name})",
            location=error_location,
        )
    else:
        # Otherwise, check that it takes the expected number of arguments.
        # "Self" counts as an implicit argument, so we expect at least
        # ``arity - 1`` natural arguments.
        n_args = entity_expr_count - 1
        entity_args = prop.natural_arguments[:n_args]
        extra_args = prop.natural_arguments[n_args:]
        check_source_language(
            len(entity_args) == n_args
            and all(arg.type.is_entity_type for arg in entity_args),
            f"{name} property must accept {n_args} entity arguments (only"
            f" {len(entity_args)} found)",
            location=error_location,
        )

    # Compute the list of arguments to pass to the property (Self
    # included).
    args = (
        [
            Argument(Location.builtin, names.Name('Self'), prop.owner.entity)
        ] + prop.natural_arguments
    )
    expr_count = entity_expr_count + len(captured_args)

    # Then check that 1) all extra passed actuals match what the property
    # arguments expect and that 2) arguments left without an actual have a
    # default value.
    default_passed_args = 0
    partial_args: list[PropertyClosure.PartialArgument] = []
    for i, (expr, arg) in enumerate(zip_longest(captured_args, extra_args)):
        arg_index = entity_expr_count + i
        if expr is None:
            check_source_language(
                arg.default_value is not None,
                'Missing an actual for argument #{} ({})'.format(
                    arg_index, arg.name.lower
                ),
                location=error_location,
            )
            default_passed_args += 1
            continue

        check_source_language(
            arg is not None,
            'Too many actuals: at most {} expected, got {}'.format(
                len(args), expr_count
            ),
            location=error_location,
        )
        check_source_language(
            expr.type.matches(arg.type), "Argument #{} of {} "
            "has type {}, should be {}".format(
                arg_index, name, expr.type.dsl_name, arg.type.dsl_name
            ),
            location=error_location,
        )
        partial_args.append(
            PropertyClosure.PartialArgument(
                len(partial_args), arg.name, arg.type
            )
        )

    DynamicVariable.check_call_bindings(error_location, prop)

    # Since we allow instantiating a predicate with partial arguments that
    # are subtypes of their corresponding property parameter, we may need
    # to generate an intermediate cast.
    cast_captured_args: list[Expr] = []
    for expr, arg in zip(captured_args, partial_args):
        if expr.type != arg.type:
            assert isinstance(arg.type, (ASTNodeType, EntityType))
            cast_captured_args.append(CastExpr(None, expr, arg.type))
        else:
            cast_captured_args.append(expr)

    # Append dynamic variables to embed their values in the closure
    for dv_arg in prop.dynamic_var_args:
        dynvar = dv_arg.dynvar
        cast_captured_args.append(dynvar.current_binding.ref_expr)
        partial_args.append(
            PropertyClosure.PartialArgument(
                len(partial_args), dynvar.name, dynvar.type
            )
        )

    # Register the closure as a predicate or as a functor, depending on the
    # the initial need. Note that the `do_generate_*` methods are memoized so
    # they will only generate a new closure if needed.
    closure_id: str
    if kind == LogicClosureKind.Predicate:
        closure_id = prop.do_generate_logic_predicate(
            tuple(partial_args), default_passed_args
        )
    else:
        closure_id = prop.do_generate_logic_functor(
            tuple(partial_args), default_passed_args
        )

    return closure_id, cast_captured_args


class BindExpr(CallExpr):
    """
    Base class for expressions that create Assign/Propagate/Unify equations.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        constructor_name: str,
        constructor_args: list[str | Expr],
        logic_ctx: Expr | None,
    ):
        """
        :param constructor_name: Name of the function to create the equation.
        :param constructor_args: Its arguments, exclusing the "Debug_String"
            one, which we automatically add.
        :param logic_ctx: The logic context to associate to this equation.
        """
        self.logic_ctx: Expr | None = logic_ctx

        args: list[str | Expr] = list(constructor_args)

        if logic_ctx:
            args.append(CallExpr(
                None,
                "Logic_Ctx",
                "Allocate_Logic_Context",
                T.InternalLogicContextAccess,
                [logic_ctx]
            ))

        if debug_info:
            args.append(
                f"Debug_String => {sloc_info_arg(debug_info.location)}"
            )

        super().__init__(
            debug_info, "Bind_Result", constructor_name, T.Equation, args
        )

    @staticmethod
    def create_functor(
        error_location: Location | L.LktNode,
        prop: PropertyDef,
        is_variadic: bool,
        closure_args: list[Expr],
        captured_args: list[Expr]
    ) -> tuple[str, list[Expr]]:
        """
        Shortcut to create a property closure for a propagate atom. In
        particular, this allows factoring further checks that need to be done
        on the signature of the property.
        """
        # Check that the property returns an entity type
        check_source_language(
            prop.type.matches(T.root_node.entity),
            f"Converter property must return a subtype of {T.entity.dsl_name}",
            location=error_location,
        )

        return create_property_closure(
            error_location,
            prop,
            is_variadic,
            closure_args,
            captured_args,
            LogicClosureKind.Propagate,
        )


class AssignExpr(BindExpr):
    """
    Expression that creates Unify equations.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        error_location: Location | L.LktNode,
        logic_var: Expr,
        value: Expr,
        logic_ctx: Expr | None,
        conv_prop: PropertyDef | None = None,
    ):
        self.logic_var = logic_var
        self.value = value
        self.conv_prop = conv_prop

        conv_expr: str | Expr
        if conv_prop:
            functor_id, closure_args = self.create_functor(
                error_location, conv_prop, False, [value], []
            )
            conv_expr = logic_closure_instantiation_expr(
                f"{functor_id}_Functor", closure_args
            )
        else:
            conv_expr = "Solver_Ifc.No_Converter"

        constructor_args: list[str | Expr] = [logic_var, value, conv_expr]

        super().__init__(
            debug_info, "Solver.Create_Assign", constructor_args, logic_ctx
        )

    @property
    def subexprs(self) -> dict:
        return {
            'logic_var': self.logic_var,
            'value': self.value,
            'conv_prop': self.conv_prop,
            'logic_ctx': self.logic_ctx
        }

    def __repr__(self) -> str:
        return '<AssignExpr>'


class PropagateExpr(BindExpr):
    """
    Expression that creates Propagate/N_Propagate equations.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        dest_var: Expr,
        exprs: list[Expr],
        prop: PropertyDef,
        constructor_name: str,
        constructor_args: list[str | Expr],
        logic_ctx: Expr | None,
    ):
        self.dest_var = dest_var
        self.exprs = exprs
        self.prop = prop
        super().__init__(
            debug_info, constructor_name, constructor_args, logic_ctx
        )

    @classmethod
    def construct_propagate(
        cls,
        debug_info: ExprDebugInfo | None,
        error_location: Location | L.LktNode,
        dest_var: Expr,
        is_variadic: bool,
        logic_var_args: list[Expr],
        captured_args: list[Expr],
        prop: PropertyDef,
        logic_ctx: Expr | None,
    ) -> Expr:

        constructor_name: str
        constructor_args: list[str | Expr]
        saved_exprs: list[SavedExpr] = []

        functor_id, closure_args = cls.create_functor(
            error_location, prop, is_variadic, logic_var_args, captured_args
        )
        functor_name = f"{functor_id}_Functor"
        exprs = logic_var_args + closure_args

        if is_variadic:
            # For combiners that work on an array of logic vars, the solver's
            # ``Create_N_Propagate`` constructor already expects an array of
            # logic variables. Although the types are different (one comes from
            # the support library and the other is generated), they have the
            # same structure, so we cast the given one to the expected type.
            assert len(logic_var_args) == 1
            constructor_name = "Solver.Create_N_Propagate"
            var_array_expr = SavedExpr(None, "Logic_Vars", logic_var_args[0])
            saved_exprs.append(var_array_expr)
            var_array = LiteralExpr(
                None,
                "Entity_Vars.Logic_Var_Array ({}.Items)",
                None,
                [var_array_expr.result_var_expr],
            )
            var_length = LiteralExpr(
                None, "{}.N", None, [var_array_expr.result_var_expr]
            )
            constructor_name = "Solver.Create_N_Propagate"
            constructor_args = [
                # "To" argument
                dest_var,
                logic_closure_instantiation_expr(
                    functor_name, closure_args, var_length
                ),
                var_array,
            ]
        elif len(logic_var_args) > 1:
            constructor_name = "Solver.Create_N_Propagate"
            constructor_args = [
                # "To" argument
                dest_var,
                logic_closure_instantiation_expr(
                    functor_name, closure_args,
                    IntegerLiteralExpr(None, len(logic_var_args))
                ),
                aggregate_expr(
                    type=None,
                    assocs=[(str(i), v)
                            for i, v in enumerate(logic_var_args, 1)]
                ),
            ]
        else:
            constructor_name = "Solver.Create_Propagate"
            constructor_args = [
                logic_var_args[0],
                dest_var,
                logic_closure_instantiation_expr(functor_name, closure_args)
            ]

        result: Expr = PropagateExpr(
            None,
            dest_var,
            exprs,
            prop,
            constructor_name,
            constructor_args,
            logic_ctx,
        )
        for e in reversed(saved_exprs):
            result = SequenceExpr(None, e, result)
        result.debug_info = debug_info
        return result

    @property
    def subexprs(self) -> dict:
        return {
            'dest_var': self.dest_var,
            'exprs': self.exprs,
            'prop': self.prop,
            'logic_ctx': self.logic_ctx
        }

    def __repr__(self) -> str:
        return '<PropagateExpr>'


class UnifyExpr(BindExpr):
    """
    Expression that creates Unify equations.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        left_var: Expr,
        right_var: Expr,
        logic_ctx: Expr | None,
    ):
        self.left_var = left_var
        self.right_var = right_var

        super().__init__(
            debug_info,
            "Solver.Create_Unify",
            [self.left_var, self.right_var],
            logic_ctx,
        )

    @property
    def subexprs(self) -> dict:
        return {
            'left_var': self.left_var,
            'right_var': self.right_var,
            'logic_ctx': self.logic_ctx
        }

    def __repr__(self) -> str:
        return '<UnifyExpr>'


class BindKind(enum.Enum):
    assign = "assign"
    propagate = "propagate"
    unify = "unify"
    unknown = "unknown"


class DomainExpr(ComputingExpr):
    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        domain: Expr,
        logic_var_expr: Expr,
    ):
        self.domain = domain
        self.logic_var_expr = logic_var_expr
        self.static_type = T.Equation
        super().__init__(debug_info, "Domain_Equation")

    def _render_pre(self) -> str:
        assert self.debug_info is not None
        return render('properties/domain_ada',
                      expr=self,
                      sloc_info_arg=sloc_info_arg(self.debug_info.location))

    @property
    def subexprs(self) -> dict:
        return {'domain': self.domain, 'logic_var_expr': self.logic_var_expr}


class PredicateExpr(CallExpr):
    """
    Return an equation that ensures that the `predicate` property is maintained
    on one or several logical variables in all possible solutions, so that the
    only solutions in the equations are the equations where the property is
    true.

    Expressions that are passed as `exprs` that are not logical variables will
    be passed as extra arguments to `predicate`, so their types need to match::

        class BaseNode(ASTNode):
            a = UserField(LogicVarType)
            b = UserField(LogicVarType)

            @langkit_property(return_type=BoolType)
            def test_property(other_node=BaseNode, int_arg=IntegerType):
                ...

            # This is a valid Predicate instantiation for the above property
            equation = Property(
                Predicate(FooNode.test_property, Self.a, Self.b, 12)
            )

    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        pred_property: PropertyDef,
        pred_id: str,
        logic_var_args: list[Expr],
        predicate_expr: Expr,
    ):
        self.pred_property = pred_property
        self.pred_id = pred_id
        self.logic_var_args = logic_var_args
        self.predicate_expr = predicate_expr

        if logic_var_args[0].type.matches(T.LogicVar.array):
            assert len(logic_var_args) == 1
            super().__init__(
                debug_info,
                "Pred",
                "Solver.Create_N_Predicate",
                T.Equation,
                [logic_var_args[0], predicate_expr],
            )
        elif len(logic_var_args) > 1:
            strn = "({})".format(", ".join(["{}"] * len(logic_var_args)))
            vars_array = untyped_literal_expr(
                strn, operands=logic_var_args
            )
            super().__init__(
                debug_info,
                "Pred",
                "Solver.Create_N_Predicate",
                T.Equation,
                [vars_array, predicate_expr],
            )
        else:
            super().__init__(
                debug_info,
                "Pred",
                "Solver.Create_Predicate",
                T.Equation,
                [logic_var_args[0], predicate_expr],
            )

    @property
    def subexprs(self) -> dict:
        return {'pred': self.pred_property,
                'pred_id': self.pred_id,
                'logic_var_args': self.logic_var_args,
                'predicate_expr': self.predicate_expr}

    def __repr__(self) -> str:
        return '<PredicateExpr {}>'.format(self.pred_id)


def make_get_value(
    debug_info: ExprDebugInfo | None,
    logic_var_expr: Expr,
) -> Expr:
    """
    Return an expression to extract the value out of a logic variable. The
    expression type is always the root entity type. If the variable is not
    defined, this evaluates to the null entity.
    """
    from langkit.expressions import IfExpr

    PropertyDef.get()._gets_logic_var_value = True
    logic_var_ref = logic_var_expr.create_result_var("Logic_Var_Value")
    return IfExpr(
        debug_info,
        cond=CallExpr(
            None,
            "Is_Logic_Var_Defined",
            "Entity_Vars.Is_Defined",
            T.Bool,
            [logic_var_expr],
        ),
        then=CallExpr(
            None,
            "Eq_Solution",
            "Entity_Vars.Get_Value",
            T.root_node.entity,
            [logic_var_ref],
        ),
        else_then=NullExpr(None, T.root_node.entity),
    )


def make_solve(
    debug_info: ExprDebugInfo | None,
    equation: Expr,
    with_diagnostics: bool,
) -> Expr:
    """
    Return an expression to call ``solve`` on the given ``equation``.

    If ``with_diagnostics`` evaluates to false, this evaluates to whether any
    solution was found or not. The solutions are not returned, instead, logic
    variables are bound to their values in the current solution.

    If ``with_diagnostics`` evaluates to true, this evalutaes to a
    ``SolverResult`` struct instead, which ``success`` field indicates whether
    resolution was successful or not.  If not, its ``diagnostics`` field
    contains an array of ``SolverDiagnostic``.

    .. todo::

        For the moment, since properties returning equations will reconstruct
        them everytime, there is no way to get the second solution if there is
        one. Also you cannot do that manually either since a property exposing
        equations cannot be public at the moment.
    """

    p = PropertyDef.get()
    p._solves_equation = True
    return CallExpr(
        debug_info,
        "Solve_Success",
        "Solve_With_Diagnostics" if with_diagnostics else "Solve_Wrapper",
        T.SolverResult if with_diagnostics else T.Bool,
        [equation, p.node_var.ref_expr],
    )


class ResetLogicVar(Expr):
    """
    Expression wrapper to reset a logic variable.

    We use this wrapper during logic equation construction so that they can
    work on logic variables that don't hold stale results.
    """

    def __init__(self, logic_var_expr: Expr):
        assert logic_var_expr.type == T.LogicVar
        self.logic_var_expr = logic_var_expr
        self.static_type = T.LogicVar
        super().__init__(None)

    def _render_pre(self) -> str:
        return '\n'.join([
            '{pre}',
            '{var}.Value := No_Entity;',
            'Entity_Vars.Reset ({var});',
        ]).format(pre=self.logic_var_expr.render_pre(),
                  var=self.logic_var_expr.render_expr())

    def _render_expr(self) -> str:
        return self.logic_var_expr.render_expr()

    @property
    def subexprs(self) -> dict:
        return {'logic_var': self.logic_var_expr}

    def __repr__(self) -> str:
        return '<ResetLogicVar>'


class ResetAllLogicVars(Expr):
    """
    Expression wrapper to reset an array of logic variables.

    We use this wrapper during logic equation construction (specifically for
    equations that take arrays of logic variables, such as the variadic version
    of NPropagate) so that they can work on logic variables that don't hold
    stale results.
    """

    def __init__(self, logic_vars_expr: Expr):
        assert logic_vars_expr.type == T.LogicVar.array
        self.logic_vars_expr = logic_vars_expr
        self.static_type = T.LogicVar.array
        super().__init__(None, skippable_refcount=True)

    def _render_pre(self) -> str:
        return '\n'.join([
            '{pre}',
            'for Var of {var}.Items loop',
            '   Var.Value := No_Entity;',
            '   Entity_Vars.Reset (Var);',
            'end loop;',
        ]).format(pre=self.logic_vars_expr.render_pre(),
                  var=self.logic_vars_expr.render_expr())

    def _render_expr(self) -> str:
        return self.logic_vars_expr.render_expr()

    @property
    def subexprs(self) -> dict:
        return {'logic_vars': self.logic_vars_expr}

    def __repr__(self) -> str:
        return '<ResetAllLogicVars>'
