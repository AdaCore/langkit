from __future__ import annotations

import enum
from itertools import zip_longest

import funcy

from langkit import names
from langkit.compile_context import get_context
from langkit.compiled_types import (
    ASTNodeType,
    Argument,
    CompiledType,
    T,
)
from langkit.diagnostics import check_source_language, error
from langkit.expressions.base import (
    AbstractExpression,
    CallExpr,
    ComputingExpr,
    DynamicVariable,
    IntegerLiteralExpr,
    LiteralExpr,
    NullExpr,
    PropertyClosure,
    PropertyDef,
    ResolvedExpression,
    SavedExpr,
    Self,
    SequenceExpr,
    abstract_expression_from_construct,
    aggregate_expr,
    construct,
    dsl_document,
    render,
    resolve_property,
    sloc_info_arg,
)


class LogicClosureKind(enum.Enum):
    """
    The purpose of a property closure, i.e. whether it will be used in a
    predicate atom or in a propagate atom. This is mainly used to decide
    where a property closure should be registered during the
    ``create_property_closure`` routine.
    """
    Predicate = enum.auto()
    Propagate = enum.auto()


def untyped_literal_expr(expr_str, operands=[]):
    """
    Create an untyped LiteralExpr instance for "expr_str" and return it.

    This is a helper for code that generates expressions which have no
    corresponding CompiledType in Langkit. Materializing such values in
    ResolvedExpression trees can be useful anayway to leverage CallExpr code
    generation capabilities, in particular temporary creation for the result.
    We can do this because CallExpr does not need its operands' types to be
    valid.

    :param str expr_str: Template code for this literal expression.
    :param list[ResolvedExpression] operands: Operand for this literal
        expression.
    :rtype: LiteralExpr
    """
    return LiteralExpr(expr_str, T.NoCompiledType, operands)


def construct_logic_ctx(
    expr: AbstractExpression | None
) -> ResolvedExpression | None:
    """
    Common logic to construct a logic context expression for a logic atom
    builder.
    """
    if expr is None:
        return None

    # Do not pass a logic context if the logic context builtin variable was not
    # bound.
    types_loader = get_context().lkt_types_loader
    assert types_loader is not None
    if expr is types_loader.logic_context_builtin.variable:
        assert isinstance(expr, DynamicVariable)
        if not expr.is_bound:
            return None

    return construct(expr, T.LogicContext)


def logic_closure_instantiation_expr(
    closure_name: str,
    closure_args: list[ResolvedExpression],
    arity: ResolvedExpression | None = None
) -> LiteralExpr:
    """
    Given the name of a property closure to be used as a logic predicate,
    converter or combiner, return an expression that instantiates this closure
    with the given partial arguments. For combiners or predicates that allow
    multiple arguments, the arity parameter must be a non-null expression that
    evaluates to the number of said arguments.
    """
    assocs: list[ResolvedExpression] = []

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
    prop: PropertyDef,
    is_variadic: bool,
    closure_args: list[ResolvedExpression],
    captured_args: list[ResolvedExpression],
    kind: LogicClosureKind
) -> tuple[str, list[ResolvedExpression]]:
    """
    Create and register a PropertyClosure object for the given property,
    considering the given partial arguments.

    This performs checks on all given arguments to make sure they match the
    signature of the property.

    Return the unique id representing the closure, as well as the complete
    list of partial arguments: the given ones extended with all dynamic var
    arguments, if any.

    :param prop: The property for which to generate a closure.
    :param is_variadic: Whether the closure accepts a single array argument.
    :param closure_args: The list of non-partial arguments.
    :param captured_args: The list of partial arguments.
    :param kind: Whether this should be registered as a predicate closure or
        as a functor closure.
    """
    from langkit.expressions import Cast

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
        )

    # Compute the list of arguments to pass to the property (Self
    # included).
    args = ([Argument(names.Name('Self'), prop.owner.entity)]
            + prop.natural_arguments)
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
                )
            )
            default_passed_args += 1
            continue

        check_source_language(
            arg is not None,
            'Too many actuals: at most {} expected, got {}'.format(
                len(args), expr_count
            )
        )
        check_source_language(
            expr.type.matches(arg.type), "Argument #{} of {} "
            "has type {}, should be {}".format(
                arg_index, name, expr.type.dsl_name, arg.type.dsl_name
            )
        )
        partial_args.append(
            PropertyClosure.PartialArgument(
                len(partial_args), arg.name, arg.type
            )
        )

    DynamicVariable.check_call_bindings(prop)

    # Since we allow instantiating a predicate with partial arguments that
    # are subtypes of their corresponding property parameter, we may need
    # to generate an intermediate cast.
    captured_args = [
        Cast.Expr(expr, arg.type) if expr.type != arg.type else expr
        for expr, arg in zip(captured_args, partial_args)
    ]

    # Append dynamic variables to embed their values in the closure
    for dynvar in prop.dynamic_vars:
        captured_args.append(construct(dynvar))
        partial_args.append(
            PropertyClosure.PartialArgument(
                len(partial_args), dynvar.argument_name, dynvar.type
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

    return closure_id, captured_args


class BindExpr(CallExpr):
    """
    Base class for resolved expressions that create Assign/Propagate/Unify
    equations.
    """

    def __init__(self,
                 constructor_name: str,
                 constructor_args: list[str | ResolvedExpression],
                 logic_ctx: ResolvedExpression | None,
                 abstract_expr: AbstractExpression | None = None):
        """
        :param constructor_name: Name of the function to create the equation.
        :param constructor_args: Its arguments, exclusing the "Debug_String"
            one, which we automatically add.
        :param logic_ctx: The logic context to associate to this equation.
        :param abstract_expr: Reference to the corresponding abstract
            expression, if any.
        """
        self.logic_ctx: ResolvedExpression | None = logic_ctx

        args: list[str | ResolvedExpression] = list(constructor_args)

        if logic_ctx:
            args.append(CallExpr(
                "Logic_Ctx",
                "Allocate_Logic_Context",
                T.InternalLogicContextAccess,
                [logic_ctx]
            ))

        if abstract_expr:
            args.append(
                f"Debug_String => {sloc_info_arg(abstract_expr.location)}"
            )

        super().__init__(
            "Bind_Result",
            constructor_name,
            T.Equation,
            args,
            abstract_expr=abstract_expr,
        )

    @staticmethod
    def create_functor(
        prop: PropertyDef,
        is_variadic: bool,
        closure_args: list[ResolvedExpression],
        captured_args: list[ResolvedExpression]
    ) -> tuple[str, list[ResolvedExpression]]:
        """
        Shortcut to create a property closure for a propagate atom. In
        particular, this allows factoring further checks that need to be done
        on the signature of the property.
        """
        # Check that the property returns an entity type
        check_source_language(
            prop.type.matches(T.root_node.entity),
            f"Converter property must return a subtype of {T.entity.dsl_name}"
        )

        return create_property_closure(
            prop, is_variadic, closure_args, captured_args,
            LogicClosureKind.Propagate
        )


class AssignExpr(BindExpr):
    """
    Resolved expression that creates Unify equations.
    """

    def __init__(self,
                 logic_var: ResolvedExpression,
                 value: ResolvedExpression,
                 conv_prop: PropertyDef | None,
                 logic_ctx: ResolvedExpression | None,
                 abstract_expr: AbstractExpression | None = None):
        self.logic_var = logic_var
        self.value = value
        self.conv_prop = conv_prop

        conv_expr: str | ResolvedExpression
        if conv_prop:
            functor_id, closure_args = self.create_functor(
                conv_prop, False, [value], []
            )
            conv_expr = logic_closure_instantiation_expr(
                f"{functor_id}_Functor", closure_args
            )
        else:
            conv_expr = "Solver_Ifc.No_Converter"

        constructor_args: list[str | ResolvedExpression] = [
            logic_var,
            value,
            conv_expr
        ]

        super().__init__(
            "Solver.Create_Assign",
            constructor_args,
            logic_ctx,
            abstract_expr=abstract_expr
        )

    @property
    def subexprs(self):
        return {
            'logic_var': self.logic_var,
            'value': self.value,
            'conv_prop': self.conv_prop,
            'logic_ctx': self.logic_ctx
        }

    def __repr__(self):
        return '<AssignExpr>'


class PropagateExpr(BindExpr):
    """
    Resolved expression that creates Propagate/N_Propagate equations.
    """

    def __init__(self,
                 dest_var: ResolvedExpression,
                 exprs: list[ResolvedExpression],
                 prop: PropertyDef,
                 constructor_name: str,
                 constructor_args: list[str | ResolvedExpression],
                 logic_ctx: ResolvedExpression | None,
                 abstract_expr: AbstractExpression | None = None):
        self.dest_var = dest_var
        self.exprs = exprs
        self.prop = prop
        super().__init__(
            constructor_name,
            constructor_args,
            logic_ctx,
            abstract_expr=abstract_expr
        )

    @classmethod
    def construct_propagate(
        cls,
        dest_var: ResolvedExpression,
        is_variadic: bool,
        logic_var_args: list[ResolvedExpression],
        captured_args: list[ResolvedExpression],
        prop: PropertyDef,
        logic_ctx: ResolvedExpression | None,
        abstract_expr: AbstractExpression | None = None
    ) -> ResolvedExpression:

        constructor_name: str
        constructor_args: list[str | ResolvedExpression]
        saved_exprs: list[SavedExpr] = []

        functor_id, closure_args = cls.create_functor(
            prop, is_variadic, logic_var_args, captured_args
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
            var_array_expr = SavedExpr("Logic_Vars", logic_var_args[0])
            saved_exprs.append(var_array_expr)
            var_array = LiteralExpr(
                "Entity_Vars.Logic_Var_Array ({}.Items)", None,
                [var_array_expr.result_var_expr]
            )
            var_length = LiteralExpr(
                "{}.N", None, [var_array_expr.result_var_expr]
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
                    IntegerLiteralExpr(len(logic_var_args))
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

        result: ResolvedExpression = PropagateExpr(
            dest_var, exprs, prop, constructor_name, constructor_args,
            logic_ctx, abstract_expr
        )
        for e in reversed(saved_exprs):
            result = SequenceExpr(e, result)
        return result

    @property
    def subexprs(self):
        return {
            'dest_var': self.dest_var,
            'exprs': self.exprs,
            'prop': self.prop,
            'logic_ctx': self.logic_ctx
        }

    def __repr__(self):
        return '<PropagateExpr>'


class UnifyExpr(BindExpr):
    """
    Resolved expression that creates Unify equations.
    """

    def __init__(self,
                 left_var: ResolvedExpression,
                 right_var: ResolvedExpression,
                 logic_ctx: ResolvedExpression | None,
                 abstract_expr: AbstractExpression | None = None):
        self.left_var = left_var
        self.right_var = right_var

        super().__init__(
            "Solver.Create_Unify",
            [self.left_var, self.right_var],
            logic_ctx,
            abstract_expr=abstract_expr
        )

    @property
    def subexprs(self):
        return {
            'left_var': self.left_var,
            'right_var': self.right_var,
            'logic_ctx': self.logic_ctx
        }

    def __repr__(self):
        return '<UnifyExpr>'


class BindKind(enum.Enum):
    assign = "assign"
    propagate = "propagate"
    unify = "unify"
    unknown = "unknown"


@dsl_document
class Bind(AbstractExpression):
    """
    Bind the two logic variables `from_expr` and `to_expr`, or one logic
    variable `from_expr` and an entity `to_expr`, through a property call.

    If provided, `conv_prop` must be a property that takes no argument and that
    return any ``ASTNode`` subclass. It is used to convert `from_expr` into a
    value to which `to_expr` is compared.

    For instance, in order to express the following logical equation::

        B = A.some_property()

    Write in the DSL::

        Bind(A, B, conv_prop=T.TypeOfA.some_property)
    """

    def __init__(
        self,
        to_expr: AbstractExpression,
        from_expr: AbstractExpression,
        conv_prop: PropertyDef | None = None,
        logic_ctx: AbstractExpression | None = None,
        kind: BindKind = BindKind.unknown,
    ):
        """
        :param to_expr: An expression resolving to a logical variable that is
            the destination of the bind.
        :param from_expr: An expression resolving to a logical variable that is
            the source of the bind.
        :param conv_prop: The property to apply on the value of from_expr that
            will yield the value to give to to_expr.  For convenience, it can
            be a property on any subclass of the root AST node class, and can
            return any subclass of the root AST node class.
        :param logic_ctx: An expression resolving to a LogicContext.
        :param kind: Kind of Bind expression, if known.
        """
        super().__init__()
        self.to_expr = to_expr
        self.from_expr = from_expr
        self.conv_prop = conv_prop
        self.logic_ctx = logic_ctx
        self.to_expr_is_logic_var = False
        self.kind = kind

        if kind == BindKind.unify:
            assert conv_prop is None
        elif kind == BindKind.propagate:
            assert conv_prop is not None

    @staticmethod
    def _construct_logic_var(var_expr) -> ResolvedExpression:
        """
        Construct a logic variable expression, making sure it is reset.
        """
        return ResetLogicVar(construct(var_expr, T.LogicVar))

    @staticmethod
    def common_construct(
        dest_var: ResolvedExpression,
        conv_prop: PropertyDef | None,
        src_expr: ResolvedExpression,
        logic_ctx: ResolvedExpression | None,
        abstract_expr: AbstractExpression | None,
    ) -> ResolvedExpression:
        """
        Construct the resolves expression that corresponds to a Bind (either a
        Propagate or an Assign equation).

        :param dest_var: Destination variable for the Bind. It must return a
            logic variable.
        :param conv_prop: Optional conversion property for the Bind, to convert
            what ``src_expr`` designates before assigning it to ``dest_var``.
        :param src_expr: Source expression for the Bind. This method checks
            that it computes a logic variable (for a Propagate) or an entity
            (for an Assign).
        :param logic_ctx: Logic context for the equation this creates.
        :param abstract_expr: Reference to the corresponding abstract
            expression, if any.
        """
        assert dest_var.type.matches(T.LogicVar)
        assert logic_ctx is None or logic_ctx.type.matches(T.LogicContext)

        if src_expr.type.matches(T.LogicVar):
            # The second operand is a logic variable: this is a Propagate or a
            # Unify equation depending on whether we have a conversion
            # property.
            if isinstance(abstract_expr, Bind):
                abstract_expr.to_expr_is_logic_var = True

            # For this operand too, make sure it will work on a clean logic
            # variable.
            src_expr = ResetLogicVar(src_expr)

            return (
                PropagateExpr.construct_propagate(
                    dest_var=dest_var,
                    is_variadic=False,
                    logic_var_args=[src_expr],
                    captured_args=[],
                    prop=conv_prop,
                    logic_ctx=logic_ctx,
                    abstract_expr=abstract_expr,
                )
                if conv_prop else
                UnifyExpr(
                    dest_var, src_expr, logic_ctx, abstract_expr=abstract_expr
                )
            )

        else:
            # The second operand is a value: this is an Assign equation

            check_source_language(
                src_expr.type.matches(T.root_node.entity),
                "Right operand must be an entity, got "
                f"{src_expr.type.dsl_name}"
            )

            # Because of Ada OOP typing rules, for code generation to work
            # properly, make sure the type of `src_expr` is the root node
            # entity.
            if src_expr.type is not T.root_node.entity:
                from langkit.expressions import Cast
                src_expr = Cast.Expr(src_expr, T.root_node.entity)

            return AssignExpr(
                dest_var,
                src_expr,
                conv_prop,
                logic_ctx,
                abstract_expr=abstract_expr,
            )

    def construct(self) -> ResolvedExpression:
        # Resolve the converter property, make sure it has an acceptable
        # signature and generate a functor for it.
        self.conv_prop = resolve_property(self.conv_prop)

        # The "to" operand must be a logic variable. Make sure the resulting
        # equation will work on a clean logic variable.
        to_expr = self._construct_logic_var(self.to_expr)

        # Second one can be either:
        # 1) A logic variable for a Propagate (with a conversion property) or a
        #    Unify (no conversion property).
        # 2) An entity for an Assign.
        from_expr_type: CompiledType | None = None
        if self.kind in (BindKind.unify, BindKind.propagate):
            from_expr_type = T.LogicVar
        from_expr = construct(self.from_expr, from_expr_type)
        if self.kind == BindKind.assign:
            check_source_language(
                not from_expr.type.matches(T.LogicVar),
                "Assigning from a logic variable is forbidden: use unify"
                " instead",
            )

        logic_ctx = construct_logic_ctx(self.logic_ctx)

        return self.common_construct(
            to_expr, self.conv_prop, from_expr, logic_ctx, self
        )


@dsl_document
class NPropagate(AbstractExpression):
    """
    Equation to assign a logic variable to the result of a property when called
    with the value of other logic variables.

    For instance::

        NPropagate(V1, T.SomeNode.some_property, V2, V3, V4)

    will create the following equation::

        %V1 <- SomeNode.some_property(%V2, %V3, %V4)
    """

    def __init__(self, dest_var, comb_prop, *exprs, **kwargs):
        """
        :param dest_var: Logic variable that is assigned the result of the
            combiner property.
        :param comb_prop: Combiner property used during the assignment. This
            property must take N entity arguments (``N = len(exprs)``) and
            return an entity.
        :param exprs: Every argument to pass to the combiner property,
            logical variables first, and extra arguments last.
        """
        super().__init__()
        self.dest_var = dest_var
        self.comb_prop = comb_prop
        self.exprs = list(exprs)
        self.logic_ctx = kwargs.pop('logic_ctx', None)

    def construct(self) -> ResolvedExpression:
        check_source_language(
            len(self.exprs) >= 1,
            "At least one argument logic variable (or array thereof) expected"
        )

        # Resolve the combiner property, make sure it matches the argument
        # logic variables and generate a functor for it.
        self.comb_prop = resolve_property(self.comb_prop)
        assert self.comb_prop is not None

        logic_ctx = construct_logic_ctx(self.logic_ctx)

        # Construct all property arguments to determine what kind of equation
        # this really is.
        dest_var = Bind._construct_logic_var(self.dest_var)
        is_variadic = False
        logic_var_args: list[ResolvedExpression]
        captured_args: list[ResolvedExpression]
        exprs = [construct(e) for e in self.exprs]
        if exprs[0].type == T.LogicVar.array:
            logic_var_args = [ResetAllLogicVars(exprs[0])]
            captured_args = exprs[1:]
            is_variadic = True
        else:
            logic_var_args, captured_args = funcy.lsplit_by(
                lambda e: e.type == T.LogicVar, exprs
            )
            # Make sure this predicate will work on clean logic variables
            logic_var_args = [ResetLogicVar(expr) for expr in logic_var_args]

        check_source_language(
            all(e.type != T.LogicVar for e in captured_args),
            'Logic variable expressions should be grouped at the beginning,'
            ' and should not appear after non logic variable expressions'
        )
        check_source_language(
            all(e.type != T.LogicVar.array for e in captured_args),
            'Unexpected logic variable array'
        )

        # If the first argument is not a logic var nor an array of logic vars,
        # this is actually a Bind: transform it now.
        if len(logic_var_args) == 0:
            return Bind.common_construct(
                dest_var=dest_var,
                conv_prop=self.comb_prop,
                src_expr=exprs[0],
                logic_ctx=logic_ctx,
                abstract_expr=self,
            )
        else:
            return PropagateExpr.construct_propagate(
                dest_var=dest_var,
                is_variadic=is_variadic,
                logic_var_args=logic_var_args,
                captured_args=captured_args,
                prop=self.comb_prop,
                logic_ctx=logic_ctx,
                abstract_expr=self
            )


class DomainExpr(ComputingExpr):
    def __init__(self, domain, logic_var_expr, abstract_expr=None):
        self.domain = domain
        ":type: ResolvedExpression"

        self.logic_var_expr = logic_var_expr
        ":type: ResolvedExpression"

        self.static_type = T.Equation

        super().__init__('Domain_Equation', abstract_expr=abstract_expr)

    def _render_pre(self):
        return render('properties/domain_ada',
                      expr=self,
                      sloc_info_arg=sloc_info_arg(self.abstract_expr.location))

    @property
    def subexprs(self):
        return {'domain': self.domain, 'logic_var_expr': self.logic_var_expr}


@abstract_expression_from_construct
def domain(
    self: AbstractExpression,
    logic_var_expr: AbstractExpression,
    domain: AbstractExpression,
) -> ResolvedExpression:
    """
    Define the domain of a logical variable. Several important properties about
    this expression:

    This is the entry point into the logic DSL. A ``LogicVarType`` variable
    *must* have a domain defined in the context of an equation. If it doesn't,
    its solution set is empty, and thus the only possible value for it is
    undefined.

    If an equation is defined in which the only constraint on variables is
    their domains, then, for a set A, B, .., N of logical variables, with
    domains DA, DB, .., DN, the set of solutions will be of the product of the
    set of every domains.

    So for example, in the equation::

        A.domain([1, 2]) and B.domain([1, 2])

    The set of solutions is::

        [(1, 1), (1, 2), (2, 1), (2, 2)]

    The ``or`` operator acts like concatenation on domains of logic variable,
    so for example::

        A.domain([1, 2]) or A.Domain([3, 4])

    is equivalent to (but slower than) ``A.domain([1, 2, 3, 4])``.

    You can define an equation that is invalid, in that not every equation has
    a domain, and, due to runtime dispatch, we cannot statically predict if
    that's going to happen. Thus, trying to solve such an equation will result
    in an error.

    Please note that for the moment equations can exist only on AST nodes, so
    the above examples are invalid, and just meant to illustrate the semantics.

    :param AbstractExpression logic_var_expr: An expression
        that must resolve to an instance of a logic variable.
    :param AbstractExpression domain: An expression that must resolve to
        the domain, which needs to be a collection of a type that
        derives from the root grammar class, or the root grammar class
        itself.
    """
    return DomainExpr(
        construct(domain, lambda d: d.is_collection, "Type given "
                  "to LogicVar must be collection type, got {expr_type}"),
        ResetLogicVar(construct(logic_var_expr, T.LogicVar)),
        abstract_expr=self,
    )


@dsl_document
class Predicate(AbstractExpression):
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

    class Expr(CallExpr):
        def __init__(self, pred_property, pred_id, logic_var_args,
                     predicate_expr, abstract_expr=None):
            self.pred_property = pred_property
            self.pred_id = pred_id
            self.logic_var_args = logic_var_args
            self.predicate_expr = predicate_expr

            if logic_var_args[0].type.matches(T.LogicVar.array):
                assert len(logic_var_args) == 1
                super().__init__(
                    'Pred', 'Solver.Create_N_Predicate',
                    T.Equation, [logic_var_args[0], predicate_expr],
                    abstract_expr=abstract_expr
                )
            elif len(logic_var_args) > 1:
                strn = "({})".format(", ".join(["{}"] * len(logic_var_args)))
                vars_array = untyped_literal_expr(
                    strn, operands=logic_var_args
                )
                super().__init__(
                    'Pred', 'Solver.Create_N_Predicate',
                    T.Equation, [vars_array, predicate_expr],
                    abstract_expr=abstract_expr
                )
            else:
                super().__init__(
                    'Pred', 'Solver.Create_Predicate',
                    T.Equation, [logic_var_args[0], predicate_expr],
                    abstract_expr=abstract_expr
                )

        @property
        def subexprs(self):
            return {'pred': self.pred_property,
                    'pred_id': self.pred_id,
                    'logic_var_args': self.logic_var_args,
                    'predicate_expr': self.predicate_expr}

        def __repr__(self):
            return '<Predicate.Expr {}>'.format(self.pred_id)

    def __init__(
        self,
        predicate: PropertyDef,
        error_location: AbstractExpression,
        *exprs: AbstractExpression,
    ):
        """
        :param predicate: The property to use as a predicate.  For convenience,
            it can be a property of any subtype of the root node, but it
            needs to return a boolean.

        :param error_location: Expression that evaluates to the error location.

        :param exprs: Every argument to pass to the predicate, logical
            variables first, and extra arguments last.
        """
        super().__init__()
        self.pred_property = predicate.root
        self.pred_error_location = error_location
        self.exprs = exprs

    def construct(self) -> ResolvedExpression:
        prop = self.pred_property
        assert isinstance(prop, PropertyDef)

        # Check the property return type
        check_source_language(
            prop.type.matches(T.Bool),
            'Predicate property must return a boolean, got {}'.format(
                 self.pred_property.type.dsl_name
            )
        )

        # Separate logic variable expressions from extra argument expressions
        logic_var_args: list[ResolvedExpression]
        captured_args: list[ResolvedExpression]
        exprs = [construct(e) for e in self.exprs]
        is_variadic = False
        if exprs[0].type.matches(T.LogicVar.array):
            logic_var_args = [ResetAllLogicVars(exprs[0])]
            captured_args = exprs[1:]
            is_variadic = True
        else:
            logic_var_args, captured_args = funcy.lsplit_by(
                lambda e: e.type == T.LogicVar, exprs
            )
            # Make sure this predicate will work on clean logic variables
            logic_var_args = [ResetLogicVar(expr) for expr in logic_var_args]

        check_source_language(
            len(logic_var_args) > 0, "Predicate instantiation should have at "
            "least one logic variable expression"
        )
        check_source_language(
            all(e.type != T.LogicVar for e in captured_args),
            'Logic variable expressions should be grouped at the beginning,'
            ' and should not appear after non logic variable expressions'
        )
        check_source_language(
            all(e.type != T.LogicVar.array for e in captured_args),
            'Unexpected logic variable array'
        )

        pred_id, captured_args = create_property_closure(
            prop, is_variadic, logic_var_args, captured_args,
            LogicClosureKind.Predicate
        )

        if self.pred_property.predicate_error is not None:
            check_source_language(
                self.pred_error_location is not None,
                "missing error_location argument for predicate"
            )
            error_loc_expr = construct(self.pred_error_location)
            check_source_language(
                error_loc_expr.type.matches(T.root_node),
                "error_location must be a bare node"
            )
            captured_args.append(error_loc_expr)

        saved_exprs: list[ResolvedExpression] = []
        arity_expr: ResolvedExpression | None = None

        if len(logic_var_args) > 1:
            arity_expr = IntegerLiteralExpr(len(logic_var_args))
        elif is_variadic:
            var_array_expr = SavedExpr("Logic_Vars", logic_var_args[0])
            saved_exprs.append(var_array_expr)
            var_array = LiteralExpr(
                "Entity_Vars.Logic_Var_Array ({}.Items)", T.LogicVar.array,
                [var_array_expr.result_var_expr]
            )
            logic_var_args = [var_array]
            arity_expr = LiteralExpr(
                "{}.N", None, [var_array_expr.result_var_expr]
            )

        predicate_expr = logic_closure_instantiation_expr(
            f"{pred_id}_Predicate", captured_args, arity_expr
        )

        result: ResolvedExpression = Predicate.Expr(
            self.pred_property, pred_id, logic_var_args, predicate_expr,
            abstract_expr=self
        )

        for e in reversed(saved_exprs):
            result = SequenceExpr(e, result)

        return result

    def __repr__(self):
        return (
            f"<Predicate on {self.pred_property.qualname}"
            f" at {self.location_repr}>"
        )


@abstract_expression_from_construct
def get_value(
    self: AbstractExpression,
    logic_var: AbstractExpression,
) -> ResolvedExpression:
    """
    Extract the value out of a logic variable. The returned type is always the
    root entity type. If the variable is not defined, return a null entity.
    """
    from langkit.expressions import If

    PropertyDef.get()._gets_logic_var_value = True

    rtype = T.root_node.entity

    logic_var_expr = construct(logic_var, T.LogicVar)
    logic_var_ref = logic_var_expr.create_result_var('Logic_Var_Value')

    return If.Expr(
        cond=CallExpr('Is_Logic_Var_Defined', 'Entity_Vars.Is_Defined',
                      T.Bool, [logic_var_expr]),
        then=CallExpr('Eq_Solution', 'Entity_Vars.Get_Value', rtype,
                      [logic_var_ref]),
        else_then=NullExpr(T.root_node.entity),
        abstract_expr=self
    )


@abstract_expression_from_construct
def solve(
    self: AbstractExpression,
    equation: AbstractExpression,
    with_diagnostics: bool,
) -> ResolvedExpression:
    """
    Call ``solve`` on the given ``equation``.

    If ``with_diagnostics`` is false, return whether any solution was found or
    not. The solutions are not returned, instead, logic variables are bound to
    their values in the current solution.

    If ``with_diagnostics`` is true, return a ``SolverResult`` struct instead,
    which ``success`` field indicates whether resolution was successful or not.
    If not, its ``diagnostics`` field contains an array of
    ``SolverDiagnostic``.

    .. todo::

        For the moment, since properties returning equations will reconstruct
        them everytime, there is no way to get the second solution if there is
        one. Also you cannot do that manually either since a property exposing
        equations cannot be public at the moment.
    """
    PropertyDef.get()._solves_equation = True
    return CallExpr(
        "Solve_Success",
        "Solve_With_Diagnostics" if with_diagnostics else "Solve_Wrapper",
        T.SolverResult if with_diagnostics else T.Bool,
        [construct(equation, T.Equation), construct(Self, T.root_node)],
        abstract_expr=self,
    )


class LogicBooleanOp(AbstractExpression):
    """
    Internal Expression that will combine sub logic expressions via an Or or
    an And logic operator.
    """

    KIND_OR = 0
    KIND_AND = 1

    def __init__(self, equation_array, kind=KIND_OR):
        """
        :param AbstractExpression equation_array: An array of equations to
            logically combine via the or operator.
        """
        super().__init__()
        self.equation_array = equation_array
        self.kind = kind

    @property
    def kind_name(self):
        return 'Any' if self.kind == self.KIND_OR else 'All'

    def construct(self):
        # The equation constructor takes an Ada array as a parameter, not our
        # access to record: unwrap it.
        relation_array = untyped_literal_expr(
            'Relation_Array ({}.Items)',
            [construct(self.equation_array, T.Equation.array)]
        )

        return CallExpr(
            "Logic_Boolean_Op", f"Solver.Create_{self.kind_name}",
            T.Equation,
            [relation_array, sloc_info_arg(self.location)],
            abstract_expr=self
        )

    def __repr__(self):
        return f"<Logic{self.kind_name} at {self.location_repr}>"


class Any(LogicBooleanOp):
    """
    Combine all equations in the `equations` array vie an OR logic operation.
    Use this when you have an unbounded number of sub-equations to bind.
    """

    def __init__(self, equations):
        super().__init__(equations, LogicBooleanOp.KIND_OR)


class All(LogicBooleanOp):
    """
    Combine all equations in the `equations` array vie an AND logic operation.
    Use this when you have an unbounded number of sub-equations to bind.
    """

    def __init__(self, equations):
        super().__init__(equations, LogicBooleanOp.KIND_AND)


@dsl_document
class LogicTrue(AbstractExpression):
    """
    Return an equation that always return True.
    """

    def __init__(self):
        super().__init__()

    def construct(self):
        return CallExpr(
            'True_Rel', 'Solver.Create_True', T.Equation,
            [sloc_info_arg(self.location)]
        )


@dsl_document
class LogicFalse(AbstractExpression):
    """
    Return an equation that always return False.
    """

    def __init__(self):
        super().__init__()

    def construct(self):
        return CallExpr(
            'False_Rel', 'Solver.Create_False', T.Equation,
            [sloc_info_arg(self.location)]
        )


class ResetLogicVar(ResolvedExpression):
    """
    Resolved expression wrapper to reset a logic variable.

    We use this wrapper during logic equation construction so that they can
    work on logic variables that don't hold stale results.
    """

    def __init__(self, logic_var_expr):
        assert logic_var_expr.type == T.LogicVar
        self.logic_var_expr = logic_var_expr
        self.static_type = T.LogicVar
        super().__init__()

    def _render_pre(self):
        return '\n'.join([
            '{pre}',
            '{var}.Value := No_Entity;',
            'Entity_Vars.Reset ({var});',
        ]).format(pre=self.logic_var_expr.render_pre(),
                  var=self.logic_var_expr.render_expr())

    def _render_expr(self):
        return self.logic_var_expr.render_expr()

    @property
    def subexprs(self):
        return {'logic_var': self.logic_var_expr}

    def __repr__(self):
        return '<ResetLogicVar>'


class ResetAllLogicVars(ResolvedExpression):
    """
    Resolved expression wrapper to reset an array of logic variables.

    We use this wrapper during logic equation construction (specifically for
    equations that take arrays of logic variables, such as the variadic version
    of NPropagate) so that they can work on logic variables that don't hold
    stale results.
    """

    def __init__(self, logic_vars_expr):
        assert logic_vars_expr.type == T.LogicVar.array
        self.logic_vars_expr = logic_vars_expr
        self.static_type = T.LogicVar.array
        super().__init__(skippable_refcount=True)

    def _render_pre(self):
        return '\n'.join([
            '{pre}',
            'for Var of {var}.Items loop',
            '   Var.Value := No_Entity;',
            '   Entity_Vars.Reset (Var);',
            'end loop;',
        ]).format(pre=self.logic_vars_expr.render_pre(),
                  var=self.logic_vars_expr.render_expr())

    def _render_expr(self):
        return self.logic_vars_expr.render_expr()

    @property
    def subexprs(self):
        return {'logic_vars': self.logic_vars_expr}

    def __repr__(self):
        return '<ResetAllLogicVars>'
