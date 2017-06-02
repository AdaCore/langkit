from __future__ import absolute_import, division, print_function
import funcy

from langkit.compiled_types import (
    BoolType, EquationType, LogicVarType, NoCompiledType, T
)

from langkit.diagnostics import check_multiple, check_source_language
from langkit.expressions.base import (
    AbstractExpression, CallExpr, ComputingExpr, LiteralExpr, PropertyDef,
    aggregate_expr, auto_attr, construct, render
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
    return LiteralExpr(expr_str, NoCompiledType, operands)


class Bind(AbstractExpression):
    """
    This expression binds two logic variables A and B, through a property call,
    so that, in logical terms::

        B = A.property_call()

    Which is expressed in the DSL as::

        Bind(A, B, property)
    """

    class Expr(CallExpr):
        def __init__(self, conv_prop, eq_prop, cprop_uid, eprop_uid, lhs, rhs,
                     pred_func, abstract_expr=None):
            self.conv_prop = conv_prop
            self.eq_prop = eq_prop
            self.cprop_uid = cprop_uid
            self.eprop_uid = eprop_uid
            self.lhs = lhs
            self.rhs = rhs
            self.pred_func = pred_func

            constructor_args = [lhs, rhs, pred_func]

            if eq_prop:
                constructor_args.append(self.dynamic_vars_to_holder(
                    eq_prop,
                    'Equals_Data_{}'.format(eq_prop.uid)
                ))
            else:
                constructor_args.append('No_Equals_Data_Default')

            super(Bind.Expr, self).__init__(
                'Bind_Result',
                'Bind_{}_{}.Create'.format(cprop_uid, eprop_uid),
                EquationType, constructor_args,
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
                    'cprop_uid': self.cprop_uid,
                    'eprop_uid': self.eprop_uid,
                    'lhs':       self.lhs,
                    'rhs':       self.rhs,
                    'pred_func': self.pred_func}

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
        self.conv_prop = resolve('conv_prop', self.conv_prop)

    def construct(self):
        from langkit.compile_context import get_context
        self.resolve_props()

        get_context().do_generate_logic_binder(self.conv_prop, self.eq_prop)

        # We have to wait for the construct pass for the following checks
        # because they rely on type information, which is not supposed to be
        # computed before this pass.
        if self.conv_prop:
            check_multiple([
                (self.conv_prop.type.matches(T.root_node.entity()),
                 'Bind property must return a subtype of {}'.format(
                     T.root_node.entity().name().camel
                )),

                (self.conv_prop.struct.matches(T.root_node),
                 'Bind property must belong to a subtype of {}'.format(
                     T.root_node.name().camel
                )),

                (not self.conv_prop.dynamic_vars,
                 'Property passed to bind must have no dynamically bound'
                 ' variable'),
            ])

        # Those checks are run in construct, because we need the eq_prop to be
        # prepared already, which is not certain in do_prepare (order
        # dependent).

        if self.eq_prop:
            args = self.eq_prop.natural_arguments
            check_multiple([
                (self.eq_prop.type == BoolType,
                 'Equality property must return boolean'),

                (self.eq_prop.struct.matches(T.root_node),
                 'Equality property must belong to a subtype of {}'.format(
                     T.root_node.name().camel
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
                other_type.el_type == self.eq_prop.struct,
                "Self and first argument should be of the same type"
            )

        cprop_uid = (self.conv_prop.uid if self.conv_prop else "Default")
        eprop_uid = (self.eq_prop.uid if self.eq_prop else "Default")

        pred_func = untyped_literal_expr(
            "Logic_Converter_{}'(null record)".format(cprop_uid)
            if self.conv_prop
            else "No_Logic_Converter_Default"
        )

        def construct_operand(op):
            from langkit.expressions import Cast, make_as_entity
            expr = construct(op)

            if expr.type.matches(T.root_node):
                expr = make_as_entity(expr)
                expr.create_result_var('Ent')

            check_source_language(

                expr.type == LogicVarType
                or expr.type.matches(T.root_node.entity()),

                "Operands to a logic bind operator should be either "
                "a logic variable or an entity, got {}".format(expr.type)
            )

            if (
                expr.type.matches(T.root_node.entity())
                and expr.type is not T.root_node.entity()
            ):
                expr = Cast.Expr(expr, T.root_node.entity())

            return expr

        lhs = construct_operand(self.from_expr)
        rhs = construct_operand(self.to_expr)

        return Bind.Expr(self.conv_prop, self.eq_prop, cprop_uid, eprop_uid,
                         lhs, rhs, pred_func, abstract_expr=self)

    def __repr__(self):
        return '<Bind>'


class DomainExpr(ComputingExpr):
    static_type = EquationType

    def __init__(self, domain, logic_var_expr, abstract_expr=None):
        self.domain = domain
        ":type: ResolvedExpression"

        self.logic_var_expr = logic_var_expr
        ":type: ResolvedExpression"

        super(DomainExpr, self).__init__('Domain_Equation',
                                         abstract_expr=abstract_expr)

    def _render_pre(self):
        return render('properties/domain_ada', expr=self)

    @property
    def subexprs(self):
        return {'domain': self.domain, 'logic_var_expr': self.logic_var_expr}


@auto_attr
def domain(self, logic_var_expr, domain):
    """
    Define the domain of a logical variable. Several important properties about
    this expression:

    This is the entry point into the logic DSL. A variable of LogicVarType
    *must* have a domain defined in the context of an equation. If it doesn't,
    its solution set is empty, and thus the only possible value for it is
    undefined.

    If an equation is defined in which the only constraint on variables is
    their domains, then, for a set A, B, .., N of logical variables, with
    domains DA, DB, .., DN, the set of solutions will be of the product of the
    set of every domains.

    So for example, in the equation::
        Domain(A, [1, 2]) and Domain(B, [1, 2])

    The set of solutions is::
        [(1, 1), (1, 2), (2, 1), (2, 2)]

    The 'or' operator acts like concatenation on domains of logic variable, so
    for example::

        Domain(A, [1, 2]) or Domain(A, [3, 4])

    is equivalent to (but slower than) Domain(A, [1, 2, 3, 4]).

    You can define an equation that is invalid, in that not every equation has
    a domain, and, due to runtime dispatch , we cannot statically predict if
    that's gonna happen. Thus, trying to solve such an equation will result in
    an error.

    Please note that for the moment equations can exist only on AST nodes,
    so the above examples are invalid, and just meant to illustrate the
    semantics.

    :param AbstractExpression logic_var_expr: An expression
        that must resolve to an instance of a logic variable.
    :param AbstractExpression domain: An expression that must resolve to
        the domain, which needs to be a collection of a type that
        derives from the root grammar class, or the root grammar class
        itself.
    """
    return DomainExpr(
        construct(domain, lambda d: d.is_collection(), "Type given "
                  "to LogicVar must be collection type, got {expr_type}"),
        construct(logic_var_expr, LogicVarType),
        abstract_expr=self,
    )


class Predicate(AbstractExpression):
    """
    The predicate expression will ensure that a certain property is maintained
    on one or several logical variables in all possible solutions, so that the
    only solutions in the equations are the equations where the property is
    True.

    Expressions that are passed that are not logical variables will be passed
    as extra arguments to the property, so their types need to match::

        class BaseNode(ASTNode):
            a = UserField(LogicVarType)
            b = UserField(LogicVarType)

            @langkit_property(return_type=BoolType)
            def test_property(other_node=BaseNode, int_arg=LongType):
                ...

            # This is a valid Predicate instantiation for the above property
            equation = Property(
                Predicate(FooNode.fields.test_property, Self.a, Self.b, 12)
            )

    """

    class Expr(CallExpr):
        def __init__(self, pred_property, pred_id, logic_var_exprs,
                     abstract_expr=None):
            self.pred_property = pred_property
            self.pred_id = pred_id
            self.logic_var_exprs = logic_var_exprs

            super(Predicate.Expr, self).__init__(
                'Pred', '{}_Pred.Create'.format(pred_id),
                EquationType, logic_var_exprs,
                abstract_expr=abstract_expr
            )

        @property
        def subexprs(self):
            return {'pred': self.pred_property,
                    'pred_id': self.pred_id,
                    'logic_var_exprs': self.logic_var_exprs}

        def __repr__(self):
            return '<Predicate.Expr {}>'.format(self.pred_id)

    def __init__(self, pred_property, *exprs):
        """
        :param PropertyDef pred_property: The property to use as a predicate.
            For convenience, it can be a property of any subtype of the root
            AST node, but it needs to return a boolean.

        :param [AbstractExpression] exprs: Every argument to pass to the
            predicate, logical variables first, and extra arguments last.
        """
        super(Predicate, self).__init__()
        self.pred_property = pred_property
        self.exprs = exprs

    def construct(self):
        check_multiple([
            (isinstance(self.pred_property, PropertyDef),
             'Predicates requires a property reference, got {} instead'.format(
                 self.pred_property
            )),

            (self.pred_property.type.matches(BoolType),
             'Predicate property must return a boolean, got {}'.format(
                 self.pred_property.type.name().camel
            )),

            (self.pred_property.struct.matches(T.root_node),
             'Predicate property must belong to a subtype of {}'.format(
                 T.root_node.name().camel
            )),

            (not self.pred_property.dynamic_vars,
             'Predicate property must have no dynamically bound variables'),
        ])

        exprs = [construct(e) for e in self.exprs]

        prop_types = [self.pred_property.struct] + [
            a.type for a in self.pred_property.natural_arguments
        ]

        # Separate logic variable expressions from extra argument expressions
        logic_var_exprs, closure_exprs = funcy.split_by(
            lambda e: e.type == LogicVarType, exprs
        )

        check_source_language(
            len(logic_var_exprs) > 0, "Predicate instantiation should have at "
            "least one logic variable expression"
        )

        check_source_language(
            all(e.type != LogicVarType for e in closure_exprs), "Logic "
            "variable expressions should be grouped at the beginning, and "
            "should not appear after non logic variable expressions"
        )

        for i, (expr, arg_type) in enumerate(zip(exprs, prop_types)):
            if expr.type == LogicVarType:
                check_source_language(
                    arg_type.matches(T.root_node), "Argument #{} of predicate "
                    "is a logic variable, the corresponding property formal "
                    "has type {}, but should be a descendent of {}".format(
                        i, arg_type.name().camel, T.root_node.name().camel
                    )
                )
            else:
                check_source_language(
                    expr.type.matches(arg_type), "Argument #{} of predicate "
                    "has type {}, should be {}".format(
                        i, expr.type.name().camel, arg_type.name().camel
                    )
                )

        pred_id = self.pred_property.do_generate_logic_predicate(*[
            e.type for e in closure_exprs
        ])

        # Append the debug image for the predicate
        closure_exprs.append(untyped_literal_expr('"{}.{}"'.format(
            self.pred_property.name.camel_with_underscores,
            self.pred_property.struct.name().camel_with_underscores
        )))

        logic_var_exprs.append(
            untyped_literal_expr("Create ({})".format(", ".join(
                ["{}" for _ in range(len(closure_exprs) - 1)]
                + ["Dbg_Img => (if Debug then new String'({})"
                   "            else null)"]
            )), operands=closure_exprs)
        )

        return Predicate.Expr(self.pred_property, pred_id, logic_var_exprs,
                              abstract_expr=self)

    def __repr__(self):
        return '<Predicate on {}>'.format(self.pred_property.qualname)


@auto_attr
def get_value(self, logic_var):
    """
    Expression that'll extract the value out of a logic variable. The type is
    always the root grammar class.

    :param AbstractExpression logic_var: The logic var from which we want to
        extract the value.
    """
    return CallExpr(
        'Eq_Solution', 'Eq_Node.Refs.Get_Value', T.root_node.entity(),
        [construct(logic_var, LogicVarType)],
        abstract_expr=self,
    )


@auto_attr
def solve(self, equation):
    """
    Expression that will call solve on an instance of EquationType,
    and return whether any solution was found or not. The solutions are not
    returned, instead, logic variables are bound to their values in the
    current solution.

    TODO: For the moment, since properties returning equations will
    reconstruct them everytime, there is no way to get the second solution
    if there is one. Also you cannot do that manually either since a
    property exposing equations cannot be public at the moment.

    :param AbstractExpression equation: The equation to solve.
    """
    return CallExpr('Solve_Success', 'Solve', BoolType,
                    [construct(equation, EquationType)],
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
        # The equation constructor takes an Ada array as a paramater, not our
        # access to record: unwrap it.
        relation_array = untyped_literal_expr(
            'Relation_Array ({}.Items)',
            [construct(self.equation_array, EquationType.array_type())]
        )

        return CallExpr('Logic_Boolean_Op', 'Logic_{}'.format(self.kind_name),
                        EquationType, [relation_array],
                        abstract_expr=self)

    def __repr__(self):
        return '<Logic{}>'.format(self.kind_name)


class Any(LogicBooleanOp):
    """
    Expression that will combine sub logic expressions via an Or logic
    operator. Use this when you have an unbounded number of sub-equations to
    bind. The parameter is an array of equations.
    """

    def __init__(self, equation_array):
        super(Any, self).__init__(equation_array, LogicBooleanOp.KIND_OR)


class All(LogicBooleanOp):
    """
    Expression that will combine sub logic expressions via an And logic
    operator. Use this when you have an unbounded number of sub-equations to
    bind. The parameter is an array of equations.
    """

    def __init__(self, equation_array):
        super(All, self).__init__(equation_array, LogicBooleanOp.KIND_AND)


class LogicTrue(AbstractExpression):
    """
    An equation that always return True.
    """

    def __init__(self):
        super(LogicTrue, self).__init__()

    def construct(self):
        return CallExpr('Logic_True', 'True_Rel', EquationType, [])

    def __repr__(self):
        return '<LogicTrue>'


class LogicFalse(AbstractExpression):
    """
    An equation that always return False.
    """

    def __init__(self):
        super(LogicFalse, self).__init__()

    def construct(self):
        return CallExpr('Logic_False', 'False_Rel', EquationType, [])

    def __repr__(self):
        return '<LogicFalse>'
