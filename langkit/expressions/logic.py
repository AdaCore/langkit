from __future__ import absolute_import, division, print_function

# Python 2/3 compatibility
try:
    from itertools import zip_longest
except ImportError:
    from itertools import izip_longest as zip_longest

import funcy

from langkit import names
from langkit.compiled_types import Argument, T, no_compiled_type
from langkit.diagnostics import check_multiple, check_source_language
from langkit.expressions.base import (
    AbstractExpression, CallExpr, ComputingExpr, DynamicVariable, LiteralExpr,
    NullExpr, PropertyDef, ResolvedExpression, Self, aggregate_expr, auto_attr,
    construct, dsl_document, render, resolve_property, sloc_info_arg
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


@dsl_document
class Bind(AbstractExpression):
    """
    Bind the two logic variables `from_expr` and `to_expr`, or one logic
    variable `from_expr` and an entity `to_expr`, through a property call.

    If provided, `conv_prop` must be a property that takes no argument and that
    return any ``ASTNode`` subclass. It is used to convert `from_expr` into a
    value to which `to_expr` is compared.

    If provided, `eq_prop` must be a property that takes one ``ASTNode``
    subclass argument and that compares it to ``Self``. In this case, the
    argument must be of the same type that the node that owns the property. It
    is used to compare the two logic variables (after `conv_prop` call, if
    applicable).

    For instance, in order to express the following logical equation::

        B = A.some_property()

    Write in the DSL::

        Bind(A, B, conv_prop=T.TypeOfA.some_property)
    """

    class Expr(CallExpr):
        def __init__(self, conv_prop, eq_prop, lhs, rhs, abstract_expr=None):
            self.conv_prop = conv_prop
            self.eq_prop = eq_prop
            self.lhs = lhs
            self.rhs = rhs

            constructor_args = [lhs, rhs]

            if conv_prop:
                constructor_args.append(self.dynamic_vars_to_holder(
                    self.conv_prop,
                    'Conv => Logic_Converter_{}'.format(self.conv_prop.uid)
                ))

            if eq_prop:
                constructor_args.append(self.dynamic_vars_to_holder(
                    eq_prop,
                    'Eq => Comparer_{}'.format(self.eq_prop.uid)
                ))

            constructor_args.append('Debug_String => {}'.format(
                sloc_info_arg(abstract_expr.location)
            ))

            if rhs.type.matches(T.root_node.entity):
                fn_name = 'Solver.Create_Assign'
            elif conv_prop or eq_prop:
                fn_name = 'Solver.Create_Propagate'
            else:
                fn_name = 'Solver.Create_Unify'

            super(Bind.Expr, self).__init__(
                'Bind_Result', fn_name,
                T.Equation, constructor_args,
                abstract_expr=abstract_expr
            )

        @staticmethod
        def dynamic_vars_to_holder(prop, type_name):
            """
            Create an aggregate expression to hold the dynamic variables to
            pass to `prop`. This is a helper to generate state for
            conversion/equality properties.

            :param PropertyDef prop: Property to pass the dynamic variables to.
            :param str type_name: Type name for the aggregate to create.

            :rtype: LiteralExpr
            """
            return aggregate_expr(
                type_name,
                [(dynvar.argument_name, construct(dynvar))
                 for dynvar in prop.dynamic_vars]
            )

        @property
        def subexprs(self):
            return {'conv_prop': self.conv_prop,
                    'eq_prop':   self.eq_prop,
                    'lhs':       self.lhs,
                    'rhs':       self.rhs}

        def __repr__(self):
            return '<Bind.Expr>'

    def __init__(self, from_expr, to_expr, conv_prop=None, eq_prop=None):
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
        :param PropertyDef|None eq_prop: The property to use to test for
            equality between the value of the two expressions. For convenience,
            it can be a property on a subclass of the root AST node class,
            however:

            1. It needs to take exactly two parameters, the self parameter and
               another parameter.
            2. The two parameters must be of exactly the same type.
        """
        super(Bind, self).__init__()
        self.from_expr = from_expr
        self.to_expr = to_expr
        self.conv_prop = conv_prop
        self.eq_prop = eq_prop

    def resolve_props(self):
        from langkit.expressions import FieldAccess

        def resolve(name, prop):
            if not prop:
                return
            if isinstance(prop, FieldAccess):
                prop = prop.resolve_field()
            elif isinstance(prop, T.Defer):
                prop = prop.get()

            check_source_language(
                isinstance(prop, PropertyDef),
                "{} must be either a FieldAccess resolving to a property, or"
                " a direct reference to a property".format(name)
            )

            return prop

        self.eq_prop = resolve('eq_prop', self.eq_prop)
        if self.eq_prop:
            self.eq_prop.root_property

        self.conv_prop = resolve('conv_prop', self.conv_prop)
        if self.conv_prop:
            self.conv_prop = self.conv_prop.root_property

    def construct(self):
        from langkit.compile_context import get_context
        self.resolve_props()

        get_context().do_generate_logic_functors(self.conv_prop, self.eq_prop)

        # We have to wait for the construct pass for the following checks
        # because they rely on type information, which is not supposed to be
        # computed before this pass.
        if self.conv_prop:
            check_multiple([
                (self.conv_prop.type.matches(T.root_node.entity),
                 'Bind property must return a subtype of {}'.format(
                     T.root_node.entity.dsl_name
                )),

                (self.conv_prop.struct.matches(T.root_node),
                 'Bind property must belong to a subtype of {}'.format(
                     T.root_node.dsl_name
                )),
            ])

            DynamicVariable.check_call_bindings(
                self.conv_prop, "In Bind's conv_prop {prop}"
            )

        # Those checks are run in construct, because we need the eq_prop to be
        # prepared already, which is not certain in do_prepare (order
        # dependent).

        if self.eq_prop:
            args = self.eq_prop.natural_arguments
            check_multiple([
                (self.eq_prop.type == T.Bool,
                 'Equality property must return boolean'),

                (self.eq_prop.struct.matches(T.root_node),
                 'Equality property must belong to a subtype of {}'.format(
                     T.root_node.dsl_name
                )),

                (len(args) == 1,
                 'Equality property: expected 1 argument, got {}'.format(
                     len(args)
                )),
            ])

            other_type = args[0].type
            check_source_language(
                other_type.is_entity_type,
                "First arg of equality property should be an entity type"
            )
            check_source_language(
                other_type.element_type == self.eq_prop.struct,
                "Self and first argument should be of the same type"
            )

            DynamicVariable.check_call_bindings(
                self.eq_prop, "In Bind's eq_prop {prop}"
            )

        # Left operand must be a logic variable
        lhs = construct(self.from_expr, T.LogicVar)

        # Second one can be either a logic variable or an entity (or an AST
        # node that is promoted to an entity).
        rhs = construct(self.to_expr)

        if rhs.type.matches(T.root_node):
            from langkit.expressions import make_as_entity
            rhs = make_as_entity(rhs)
        else:
            check_source_language(
                rhs.type.matches(T.root_node.entity)
                or rhs.type.matches(T.LogicVar),
                'Right operand must be either a logic variable or an entity,'
                ' got {}'.format(rhs.type.dsl_name)
            )

        # Because of Ada OOP typing rules, for code generation to work
        # properly, make sure the type of `rhs` is the root node entity.
        if (
            rhs.type.matches(T.root_node.entity)
            and rhs.type is not T.root_node.entity
        ):
            from langkit.expressions import Cast
            rhs = Cast.Expr(rhs, T.root_node.entity)

        return Bind.Expr(self.conv_prop, self.eq_prop,
                         lhs, rhs, abstract_expr=self)

    def __repr__(self):
        return '<Bind>'


class DomainExpr(ComputingExpr):
    static_type = T.Equation

    def __init__(self, domain, logic_var_expr, abstract_expr=None):
        self.domain = domain
        ":type: ResolvedExpression"

        self.logic_var_expr = logic_var_expr
        ":type: ResolvedExpression"

        super(DomainExpr, self).__init__('Domain_Equation',
                                         abstract_expr=abstract_expr)

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

        A.domain([1, 2]) or B.Domain([3, 4])

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
        construct(logic_var_expr, T.LogicVar),
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
            if len(logic_var_exprs) > 1:
                strn = "({})".format(", ".join(["{}"] * len(logic_var_exprs)))
                vars_array = untyped_literal_expr(
                    strn, operands=logic_var_exprs
                )
                super(Predicate.Expr, self).__init__(
                    'Pred', 'Solver.Create_N_Predicate',
                    T.Equation, [vars_array, predicate_expr],
                    abstract_expr=abstract_expr
                )
            else:
                super(Predicate.Expr, self).__init__(
                    'Pred', 'Solver.Create_Predicate',
                    T.Equation, [logic_var_exprs[0], predicate_expr],
                    abstract_expr=abstract_expr
                )

        @property
        def subexprs(self):
            return {'pred': self.pred_property,
                    'pred_id': self.pred_id,
                    'logic_var_exprs': self.logic_var_exprs}

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
        super(Predicate, self).__init__()
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
        logic_var_exprs, closure_exprs = funcy.split_by(
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
            ', '.join(['{}' for _ in range(len(closure_exprs))])
        ) if closure_exprs else ""
        predicate_expr = untyped_literal_expr(
            'Create_{}_Predicate{}'.format(
                pred_id, args
            ), operands=closure_exprs
        )

        return Predicate.Expr(self.pred_property, pred_id,
                              logic_var_exprs, predicate_expr,
                              abstract_expr=self)

    def __repr__(self):
        return '<Predicate on {}>'.format(self.pred_property.qualname)


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
        super(LogicBooleanOp, self).__init__()
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
            'Logic_Boolean_Op', 'Solver.Create_{}'.format(self.kind_name),
            T.Equation,
            [relation_array, sloc_info_arg(self.location)],
            abstract_expr=self
        )

    def __repr__(self):
        return '<Logic{}>'.format(self.kind_name)


@dsl_document
class Any(LogicBooleanOp):
    """
    Combine all equations in the `equations` array vie an OR logic operation.
    Use this when you have an unbounded number of sub-equations to bind.
    """

    def __init__(self, equations):
        super(Any, self).__init__(equations, LogicBooleanOp.KIND_OR)


@dsl_document
class All(LogicBooleanOp):
    """
    Combine all equations in the `equations` array vie an AND logic operation.
    Use this when you have an unbounded number of sub-equations to bind.
    """

    def __init__(self, equations):
        super(All, self).__init__(equations, LogicBooleanOp.KIND_AND)


@dsl_document
class LogicTrue(AbstractExpression):
    """
    Return an equation that always return True.
    """

    def __init__(self):
        super(LogicTrue, self).__init__()

    def construct(self):
        return CallExpr(
            'True_Rel', 'Solver.Create_True', T.Equation,
            [sloc_info_arg(self.location)]
        )

    def __repr__(self):
        return '<LogicTrue>'


@dsl_document
class LogicFalse(AbstractExpression):
    """
    Return an equation that always return False.
    """

    def __init__(self):
        super(LogicFalse, self).__init__()

    def construct(self):
        return CallExpr(
            'False_Rel', 'Solver.Create_False', T.Equation,
            [sloc_info_arg(self.location)]
        )

    def __repr__(self):
        return '<LogicFalse>'
