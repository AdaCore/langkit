from langkit.compiled_types import (
    LogicVarType, EquationType, BoolType, StructMetaclass
)

from langkit.diagnostics import check_multiple
from langkit.expressions.base import (
    AbstractExpression, BuiltinCallExpr, LiteralExpr, PropertyDef,
    ResolvedExpression, construct, BasicExpr
)
from langkit.expressions.envs import Env


def untyped_literal_expr(expr_str):
    """
    Create an untyped LiteralExpr instance for "expr_str" and return it.

    This is a helper for code that generates expressions which have no
    corresponding CompiledType in Langkit. Materializing such values in
    ResolvedExpression trees can be useful anayway to leverage BuiltinCallExpr
    code generation capabilities, in particular temporary creation for the
    result. We can do this because BuiltinCallExpr does not need its operands'
    types to be valid.

    :param str expr_str: The generated code for this literal expression.
    :rtype: LiteralExpr
    """
    return LiteralExpr(expr_str, None)


class Bind(AbstractExpression):
    """
    This expression binds two logic variables A and B, through a property call,
    so that, in logical terms::

        B = A.property_call()

    Which is expressed in the DSL as::

        Bind(A, B, property)
    """

    def __init__(self, from_var, to_var, bind_property):
        """
        :param AbstractExpression from_var: An expression resolving to a
            logical variable that is the source of the bind.
        :param AbstractExpression to_var: An expression resolving to a
            logical variable that is the destination of the bind.
        :param PropertyDef bind_property: The property to apply on the value of
            from_var that will yield the value to give to to_var. For
            convenience, it can be a property on any subclass of the root
            ast node class, and can return any subclass of the root ast node
            class.
        """
        super(Bind, self).__init__()
        self.from_var = from_var
        self.to_var = to_var
        self.bind_property = bind_property

    def do_prepare(self):
        root_class = StructMetaclass.root_grammar_class
        check_multiple([
            (self.bind_property.type.matches(root_class),
             "The property passed to bind must return a subtype "
             "of {}".format(root_class.name().camel)),

            (self.bind_property.struct.matches(root_class),
             "The property passed to bind must belong to a subtype "
             "of {}".format(root_class.name().camel))
        ])

        self.bind_property.do_generate_logic_binder()

    def construct(self):
        t = self.bind_property.struct.name()
        p = self.bind_property.name
        pred_func = untyped_literal_expr(
            "{}_{}_Logic_Binder'(Env => {})".format(
                t, p, construct(Env).render_expr()
            )
        )

        return BuiltinCallExpr(
            "{}_{}_Bind.Create".format(t, p), EquationType,
            [construct(self.from_var, LogicVarType),
             construct(self.to_var, LogicVarType),
             pred_func],
            "Bind_Result"
        )


class Domain(AbstractExpression):
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
    """

    class Expr(ResolvedExpression):
        static_type = EquationType

        def __init__(self, domain, logic_var_expr):
            self.domain = domain
            ":type: ResolvedExpression"

            self.logic_var_expr = logic_var_expr
            ":type: ResolvedExpression"

            self.res_var = PropertyDef.get().vars.create("Var", EquationType)

            super(Domain.Expr, self).__init__()

        def _render_pre(self):
            return "\n".join([
                self.domain.render_pre(),
                self.logic_var_expr.render_pre(), """
                declare
                    Dom : {domain_type} := {domain};
                    A   : Eq_Node.Raw_Member_Array (1 .. Length (Dom));
                begin
                    for J in 0 .. Length (Dom) - 1 loop
                        A (J + 1) := Get (Dom, J);
                    end loop;

                    {res_var} := Member ({logic_var}, A);
                end;
                """.format(logic_var=self.logic_var_expr.render_expr(),
                           domain=self.domain.render_expr(),
                           domain_type=self.domain.type.name(),
                           res_var=self.res_var.name)
            ])

        def _render_expr(self):
            return str(self.res_var.name)

    def __init__(self, logic_var_expr, domain):
        """
        :param AbstractExpression logic_var_expr: An expression
            that must resolve to an instance of a logic variable.
        :param AbstractExpression domain: An expression that must resolve to
            the domain, which needs to be a collection of a type that
            derives from the root grammar class, or the root grammar class
            itself.
        """
        super(Domain, self).__init__()

        self.domain = domain
        ":type: AbstractExpression"

        self.logic_var_expr = logic_var_expr
        ":type: AbstractExpression"

    def construct(self):
        return Domain.Expr(
            construct(self.domain, lambda d: d.is_collection(), "Type given "
                      "to LogicVar must be collection type, got {expr_type}"),
            construct(self.logic_var_expr, LogicVarType)
        )


class Predicate(AbstractExpression):
    """
    The predicate expression will ensure that a certain property is
    maintained on a logical variable in all possible solutions, so that the
    only solutions in the equations are the equations where the property is
    True.
    """

    def __init__(self, logic_var_expr, pred_property):
        """
        :param AbstractExpression logic_var_expr: The logic variable on
            which to apply the predicate.
        :param AbstractExpression pred_property: The property to use as a
            predicate. For convenience, it can be a property of any subtype of
            the root ast node, but it needs to return a boolean.
        """
        super(Predicate, self).__init__()
        self.pred_property = pred_property
        self.logic_var_expr = logic_var_expr

    def do_prepare(self):
        root_class = StructMetaclass.root_grammar_class

        check_multiple([
            (isinstance(self.pred_property, PropertyDef),
             "Needs a property reference, got {}".format(self.pred_property)),

            (self.pred_property.type.matches(BoolType),
             "The property passed to predicate must return a boolean, "
             "got {}".format(self.pred_property.type.name().camel)),

            (self.pred_property.struct.matches(root_class),
             "The property passed to bind must belong to a subtype "
             "of {}".format(root_class.name().camel))
        ])

        self.pred_property.do_generate_logic_predicate()

    def construct(self):
        t = self.pred_property.struct.name()
        p = self.pred_property.name
        logic_var_expr = construct(self.logic_var_expr, LogicVarType)
        pred_func = untyped_literal_expr(
            "{}_{}_Predicate_Caller'(Env => {})".format(
                t, p, construct(Env).render_expr()
            )
        )

        return BuiltinCallExpr(
            "{}_{}_Pred.Create".format(t, p), EquationType,
            [logic_var_expr, pred_func],
            "Pred"
        )


class GetLogicValue(AbstractExpression):
    """
    Expression that'll extract the value out of a logic variable. The type is
    always the root grammar class.
    """

    def __init__(self, logic_var):
        super(GetLogicValue, self).__init__()

        self.logic_var = logic_var
        ":type: AbstractExpression"

    def construct(self):
        return BuiltinCallExpr(
            "Eq_Node.Refs.GetL", StructMetaclass.root_grammar_class,
            [construct(self.logic_var, LogicVarType)]
        )


class SolveEquation(AbstractExpression):
    """
    Expression that will call solve on an instance of EquationType,
    and return whether any solution was found or not. The solutions are not
    returned, instead, logic variables are bound to their values in the
    current solution.

    TODO: For the moment, since properties returning equations will
    reconstruct them everytime, there is no way to get the second solution
    if there is one. Also you cannot do that manually either since a
    property exposing equations cannot be public at the moment.
    """

    def __init__(self, equation):
        super(SolveEquation, self).__init__()

        self.equation = equation
        ":type: AbstractExpression"

    def construct(self):
        return BuiltinCallExpr("Solve", BoolType,
                               [construct(self.equation, EquationType)])


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

    def construct(self):
        return BasicExpr(
            "Variadic_{} (Relation_Array ({{}}.Items))".format(
                "Or" if self.kind == self.KIND_OR else "And"
            ),
            EquationType,
            [construct(self.equation_array, EquationType.array_type())]
        )


class LogicOr(LogicBooleanOp):
    """
    Expression that will combine sub logic expressions via an Or logic
    operator. Use this when you have an unbounded number of sub-equations to
    bind. The parameter is an array of equations.
    """

    def __init__(self, equation_array):
        super(LogicOr, self).__init__(equation_array, LogicBooleanOp.KIND_OR)


class LogicAnd(LogicBooleanOp):
    """
    Expression that will combine sub logic expressions via an And logic
    operator. Use this when you have an unbounded number of sub-equations to
    bind. The parameter is an array of equations.
    """

    def __init__(self, equation_array):
        super(LogicAnd, self).__init__(equation_array, LogicBooleanOp.KIND_AND)
