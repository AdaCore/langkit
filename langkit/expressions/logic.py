from itertools import zip_longest
from typing import Any as _Any, List, Optional, Tuple, Union

import funcy

from langkit import names
from langkit.compiled_types import Argument, T, no_compiled_type
from langkit.diagnostics import check_multiple, check_source_language, error
from langkit.expressions.base import (
    AbstractExpression, CallExpr, ComputingExpr, DynamicVariable,
    IntegerLiteralExpr, LiteralExpr, NullExpr, PropertyDef, ResolvedExpression,
    Self, aggregate_expr, auto_attr, construct, dsl_document, render,
    resolve_property, sloc_info_arg
)


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
    return LiteralExpr(expr_str, no_compiled_type, operands)


class BindExpr(CallExpr):
    """
    Base class for resolved expressions that create Assign/Propagate/Unify
    equations.
    """

    def __init__(self,
                 constructor_name: str,
                 constructor_args: List[Union[str, ResolvedExpression]],
                 abstract_expr: Optional[AbstractExpression] = None):
        """
        :param constructor_name: Name of the function to create the equation.
        :param constructor_args: Its arguments, exclusing the "Debug_String"
            one, which we automatically add.
        :param abstract_expr: Reference to the corresponding abstract
            expression, if any.
        """
        args: List[Union[str, ResolvedExpression]] = list(constructor_args)
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
    def functor_expr(type_name: str,
                     prop: PropertyDef,
                     arity: Optional[int] = None) -> LiteralExpr:
        """
        Return an expression to create a functor for ``Prop``.

        :param type_name: Name of the functor derived type.
        :param prop: Property called by the functor.
        :param arity: If the functor type handles variadic functions, this
            should be the number of entity arguments to pass to "prop".
        """
        assocs: List[Tuple[Union[str, names.Name], ResolvedExpression]] = []

        if arity is not None:
            assocs.append(("N", IntegerLiteralExpr(arity)))

        assocs.extend([
            ("Ref_Count", IntegerLiteralExpr(1)),
            ("Cache_Set", LiteralExpr("False", None)),
            ("Cache_Key", LiteralExpr("<>", None)),
            ("Cache_Value", LiteralExpr("<>", None)),
        ])

        assocs.extend(
            (dynvar.argument_name, construct(dynvar))
            for dynvar in prop.dynamic_vars
        )

        return aggregate_expr(type_name, assocs)


class AssignExpr(BindExpr):
    """
    Resolved expression that creates Unify equations.
    """

    def __init__(self,
                 logic_var: ResolvedExpression,
                 value: ResolvedExpression,
                 conv_prop: Optional[PropertyDef],
                 abstract_expr: Optional[AbstractExpression] = None):
        self.logic_var = logic_var
        self.value = value
        self.conv_prop = conv_prop

        constructor_args: List[Union[str, ResolvedExpression]] = [
            logic_var,
            value,
            self.functor_expr(f"Logic_Functor_{conv_prop.uid}", conv_prop)
            if conv_prop else
            "Solver_Ifc.No_Converter",
        ]

        super().__init__(
            "Solver.Create_Assign",
            constructor_args,
            abstract_expr=abstract_expr
        )

    @property
    def subexprs(self):
        return {
            'logic_var': self.logic_var,
            'value': self.value,
            'conv_prop': self.conv_prop,
        }

    def __repr__(self):
        return '<AssignExpr>'


class PropagateExpr(BindExpr):
    """
    Resolved expression that creates Propagate/N_Propagate equations.
    """

    def __init__(self,
                 dest_var: ResolvedExpression,
                 arg_vars: List[ResolvedExpression],
                 prop: PropertyDef,
                 abstract_expr: Optional[AbstractExpression] = None):
        self.dest_var = dest_var
        self.arg_vars = arg_vars
        self.prop = prop

        constructor_name: str
        constructor_args: List[Union[str, ResolvedExpression]]

        if len(arg_vars) == 1:
            constructor_name = "Solver.Create_Propagate"
            constructor_args = [
                dest_var,
                arg_vars[0],
                self.functor_expr(f"Logic_Functor_{prop.uid}", prop)
                if prop else
                "Solver_Ifc.No_Converter",
            ]
        else:
            constructor_name = "Solver.Create_N_Propagate"
            constructor_args = [
                dest_var,
                self.functor_expr(
                    f"Logic_Functor_{prop.uid}", prop, len(arg_vars)
                )
                if prop else
                "Solver_Ifc.No_Combiner",
                aggregate_expr(
                    type=None,
                    assocs=[(str(i), v) for i, v in enumerate(arg_vars, 1)],
                ),
            ]

        super().__init__(
            constructor_name, constructor_args, abstract_expr=abstract_expr
        )

    @property
    def subexprs(self):
        return {
            'dest_var': self.dest_var,
            'arg_vars': self.arg_vars,
            'prop': self.prop,
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
                 abstract_expr: Optional[AbstractExpression] = None):
        self.left_var = left_var
        self.right_var = right_var

        super().__init__(
            "Solver.Create_Unify",
            [self.left_var, self.right_var],
            abstract_expr=abstract_expr
        )

    @property
    def subexprs(self):
        return {
            'left_var': self.left_var,
            'right_var': self.right_var,
        }

    def __repr__(self):
        return '<UnifyExpr>'


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

    def __init__(self, from_expr, to_expr, conv_prop=None):
        """
        :param AbstractExpression from_expr: An expression resolving to a
            logical variable that is the source of the bind.
        :param AbstractExpression to_expr: An expression resolving to a
            logical variable that is the destination of the bind.
        :param PropertyDef|None conv_prop: The property to apply on the
            value of from_expr that will yield the value to give to to_expr.
            For convenience, it can be a property on any subclass of the root
            AST node class, and can return any subclass of the root AST node
            class.
        """
        super().__init__()
        self.from_expr = from_expr
        self.to_expr = to_expr
        self.conv_prop = conv_prop

    @staticmethod
    def _resolve_property(name: str,
                          prop_ref: _Any,
                          arity: int) -> Optional[PropertyDef]:
        """
        Resolve the ``prop`` property reference (if any, built in the DSL) to
        the referenced property. If it is present, check its signature.

        :param name: Name of the property in the DSL construct. Used to format
            the error message.
        :param prop_ref: Property reference to resolve.
        :param arity: Expected number of entity arguments for this property
            ("Self" included).
        """
        from langkit.expressions import FieldAccess

        # First, resolve the property

        prop: PropertyDef

        if prop_ref is None:
            return None

        elif isinstance(prop_ref, FieldAccess):
            node_data = prop_ref.resolve_field()
            if isinstance(node_data, PropertyDef):
                prop = node_data
            else:
                error(f"{name} must be a property")

        elif isinstance(prop_ref, T.Defer):
            prop = prop_ref.get()

        elif isinstance(prop_ref, PropertyDef):
            prop = prop_ref

        else:
            error(
                f"{name} must be either a FieldAccess resolving to a property,"
                " or a direct reference to a property"
            )

        # Second, check its signature

        prop = prop.root_property
        assert prop.struct
        check_source_language(
            prop.struct.matches(T.root_node),
            f"{name} must belong to a subtype of {T.root_node.dsl_name}",
        )

        # Check that it takes the expected number of arguments. "Self" counts
        # as an implicit argument, so we expect at least ``arity - 1`` natural
        # arguments.
        n_args = arity - 1
        entity_args = prop.natural_arguments[:n_args]
        extra_args = prop.natural_arguments[n_args:]
        check_source_language(
            len(entity_args) == n_args
            and all(arg.type.is_entity_type for arg in entity_args),
            f"{name} property must accept {n_args} entity arguments (only"
            f" {len(entity_args)} found)",
        )

        # The other argumenst must be optional
        check_source_language(
            all(arg.default_value is not None for arg in extra_args),
            f"extra arguments for {name} must be optional",
        )

        # Check the property return type
        check_source_language(
            prop.type.matches(T.root_node.entity),
            f"{name} must return a subtype of {T.entity.dsl_name}",
        )

        # Check that all dynamic variables for this property are bound in the
        # current expression context.
        DynamicVariable.check_call_bindings(
            prop, f"In call to {{prop}} as {name}"
        )

        # Third, generate a functor for this property, so that equations can
        # refer to it.
        from langkit.compile_context import get_context
        get_context().do_generate_logic_functors(prop, arity)

        return prop

    @staticmethod
    def _construct_logic_var(var_expr) -> ResolvedExpression:
        """
        Construct a logic variable expression, making sure it is reset.
        """
        return ResetLogicVar(construct(var_expr, T.LogicVar))

    def construct(self):
        # Resolve the converter property, make sure it has an acceptable
        # signature and generate a functor for it.
        self.conv_prop = self._resolve_property(
            "Bind's conv_prop", self.conv_prop, 1
        )

        # Left operand must be a logic variable. Make sure the resulting
        # equation will work on a clean logic variable.
        lhs = self._construct_logic_var(self.from_expr)

        # Second one can be either a logic variable or an entity (or an AST
        # node that is promoted to an entity).
        rhs = construct(self.to_expr)

        if rhs.type.matches(T.LogicVar):
            # The second operand is a logic variable: this is a Propagate or a
            # Unify equation depending on whether we have a conversion
            # property.

            # For this operand too, make sure it will work on a clean logic
            # variable.
            rhs = ResetLogicVar(rhs)

            return (
                PropagateExpr(lhs, [rhs], self.conv_prop, abstract_expr=self)
                if self.conv_prop else
                UnifyExpr(lhs, rhs, abstract_expr=self)
            )

        else:
            # The second operand is a value: this is an Assign equation

            if rhs.type.matches(T.root_node):
                from langkit.expressions import make_as_entity
                rhs = make_as_entity(rhs)
            else:
                check_source_language(
                    rhs.type.matches(T.root_node.entity)
                    or rhs.type.matches(T.LogicVar),
                    "Right operand must be either a logic variable or an"
                    f" entity, got {rhs.type.dsl_name}"
                )

            # Because of Ada OOP typing rules, for code generation to work
            # properly, make sure the type of `rhs` is the root node entity.
            if rhs.type is not T.root_node.entity:
                from langkit.expressions import Cast
                rhs = Cast.Expr(rhs, T.root_node.entity)

            return AssignExpr(lhs, rhs, self.conv_prop, abstract_expr=self)


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

    def __init__(self, dest_var, comb_prop, *arg_vars):
        """
        :param dest_var: Logic variable that is assigned the result of the
            combiner property.
        :param comb_prop: Combiner property used during the assignment. This
            property must take N entity arguments (``N = len(arg_vars)``) and
            return an entity.
        :param arg_vars: Logic variables whose values are passed as arguments
            to the combiner property.
        """
        super().__init__()
        self.dest_var = dest_var
        self.comb_prop = comb_prop
        self.arg_vars = list(arg_vars)

    def construct(self):
        # Resolve logic variables
        dest_var = Bind._construct_logic_var(self.dest_var)
        arg_vars = [Bind._construct_logic_var(v) for v in self.arg_vars]
        check_source_language(
            len(arg_vars) >= 1,
            "At least one argument logic variable expected"
        )

        # Resolve the combiner property, make sure it matches the argument
        # logic variables and generate a functor for it.
        self.comb_prop = Bind._resolve_property(
            "NPropagate's comb_prop", self.comb_prop, len(arg_vars)
        )
        assert self.comb_prop is not None

        return PropagateExpr(
            dest_var, arg_vars, self.comb_prop, abstract_expr=self
        )


class DomainExpr(ComputingExpr):
    static_type = T.Equation

    def __init__(self, domain, logic_var_expr, abstract_expr=None):
        self.domain = domain
        ":type: ResolvedExpression"

        self.logic_var_expr = logic_var_expr
        ":type: ResolvedExpression"

        super().__init__('Domain_Equation', abstract_expr=abstract_expr)

    def _render_pre(self):
        return render('properties/domain_ada',
                      expr=self,
                      sloc_info_arg=sloc_info_arg(self.abstract_expr.location))

    @property
    def subexprs(self):
        return {'domain': self.domain, 'logic_var_expr': self.logic_var_expr}


@auto_attr
def domain(self, logic_var_expr, domain):
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
        def __init__(self, pred_property, pred_id, logic_var_exprs,
                     predicate_expr, abstract_expr=None):
            self.pred_property = pred_property
            self.pred_id = pred_id
            self.logic_var_exprs = logic_var_exprs
            self.predicate_expr = predicate_expr

            if len(logic_var_exprs) > 1:
                strn = "({})".format(", ".join(["{}"] * len(logic_var_exprs)))
                vars_array = untyped_literal_expr(
                    strn, operands=logic_var_exprs
                )
                super().__init__(
                    'Pred', 'Solver.Create_N_Predicate',
                    T.Equation, [vars_array, predicate_expr],
                    abstract_expr=abstract_expr
                )
            else:
                super().__init__(
                    'Pred', 'Solver.Create_Predicate',
                    T.Equation, [logic_var_exprs[0], predicate_expr],
                    abstract_expr=abstract_expr
                )

        @property
        def subexprs(self):
            return {'pred': self.pred_property,
                    'pred_id': self.pred_id,
                    'logic_var_exprs': self.logic_var_exprs,
                    'predicate_expr': self.predicate_expr}

        def __repr__(self):
            return '<Predicate.Expr {}>'.format(self.pred_id)

    def __init__(self, predicate, *exprs):
        """
        :param PropertyDef predicate: The property to use as a predicate.
            For convenience, it can be a property of any subtype of the root
            AST node, but it needs to return a boolean.

        :param [AbstractExpression] exprs: Every argument to pass to the
            predicate, logical variables first, and extra arguments last.
        """
        super().__init__()
        self.pred_property = predicate
        self.exprs = exprs

    def do_prepare(self):
        self.pred_property = resolve_property(self.pred_property).root_property

    def construct(self):
        check_multiple([
            (self.pred_property.type.matches(T.Bool),
             'Predicate property must return a boolean, got {}'.format(
                 self.pred_property.type.dsl_name
            )),

            (self.pred_property.struct.matches(T.root_node),
             'Predicate property must belong to a subtype of {}'.format(
                 T.root_node.dsl_name
            )),
        ])

        # Separate logic variable expressions from extra argument expressions
        exprs = [construct(e) for e in self.exprs]
        logic_var_exprs, closure_exprs = funcy.lsplit_by(
            lambda e: e.type == T.LogicVar, exprs
        )
        check_source_language(
            len(logic_var_exprs) > 0, "Predicate instantiation should have at "
            "least one logic variable expression"
        )
        check_source_language(
            all(e.type != T.LogicVar for e in closure_exprs),
            'Logic variable expressions should be grouped at the beginning,'
            ' and should not appear after non logic variable expressions'
        )

        # Make sure this predicate will work on clean logic variables
        logic_var_exprs = [ResetLogicVar(expr) for expr in logic_var_exprs]

        # Compute the list of arguments to pass to the property (Self
        # included).
        args = ([Argument(names.Name('Self'),
                 self.pred_property.struct.entity)] +
                self.pred_property.natural_arguments)

        # Then check that 1) all extra passed actuals match what the property
        # arguments expect and that 2) arguments left without an actual have a
        # default value.
        default_passed_args = 0
        for i, (expr, arg) in enumerate(zip_longest(exprs, args)):

            if expr is None:
                check_source_language(
                    arg.default_value is not None,
                    'Missing an actual for argument #{} ({})'.format(
                        i, arg.name.lower
                    )
                )
                default_passed_args += 1
                continue

            check_source_language(
                arg is not None,
                'Too many actuals: at most {} expected, got {}'.format(
                    len(args), len(exprs)
                )
            )

            if expr.type == T.LogicVar:
                check_source_language(
                    arg.type.matches(T.root_node.entity),
                    "Argument #{} of predicate "
                    "is a logic variable, the corresponding property formal "
                    "has type {}, but should be a descendent of {}".format(
                        i, arg.type.dsl_name, T.root_node.entity.dsl_name
                    )
                )
            else:
                check_source_language(
                    expr.type.matches(arg.type), "Argument #{} of predicate "
                    "has type {}, should be {}".format(
                        i, expr.type.dsl_name, arg.type.dsl_name
                    )
                )

        DynamicVariable.check_call_bindings(
            self.pred_property, 'In predicate property {prop}'
        )

        # Append dynamic variables to embed their values in the closure
        closure_exprs.extend(
            construct(dynvar) for dynvar in self.pred_property.dynamic_vars
        )

        pred_id = self.pred_property.do_generate_logic_predicate(
            tuple(e.type for e in closure_exprs),
            default_passed_args
        )

        args = " ({})".format(
            ', '.join(["{}" for _ in range(len(closure_exprs))])
        ) if closure_exprs else ""
        predicate_expr = untyped_literal_expr(
            f"Create_{pred_id}_Predicate{args}", operands=closure_exprs
        )

        return Predicate.Expr(self.pred_property, pred_id,
                              logic_var_exprs, predicate_expr,
                              abstract_expr=self)

    def __repr__(self):
        return (
            f"<Predicate on {self.pred_property.qualname}"
            f" at {self.location_repr}>"
        )


@auto_attr
def get_value(self, logic_var):
    """
    Extract the value out of a logic variable. The returned type is always the
    root entity type. If the variable is not defined, return a null entity.

    :param AbstractExpression logic_var: The logic var from which we want to
        extract the value.
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


@auto_attr
def solve(self, equation):
    """
    Call ``solve`` on the given `equation` and return whether any solution was
    found or not. The solutions are not returned, instead, logic variables are
    bound to their values in the current solution.

    .. todo::

        For the moment, since properties returning equations will reconstruct
        them everytime, there is no way to get the second solution if there is
        one. Also you cannot do that manually either since a property exposing
        equations cannot be public at the moment.

    :param AbstractExpression equation: The equation to solve.
    """
    PropertyDef.get()._solves_equation = True
    return CallExpr('Solve_Success', 'Solve_Wrapper', T.Bool,
                    [construct(equation, T.Equation),
                     construct(Self, T.root_node)],
                    abstract_expr=self)


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


@dsl_document
class Any(LogicBooleanOp):
    """
    Combine all equations in the `equations` array vie an OR logic operation.
    Use this when you have an unbounded number of sub-equations to bind.
    """

    def __init__(self, equations):
        super().__init__(equations, LogicBooleanOp.KIND_OR)


@dsl_document
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
