from langkit import names
from langkit.compiled_types import BoolType, ASTNode, LongType, Struct
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, LiteralExpr, No, PropertyDef,
    ResolvedExpression, render, construct,
)
from langkit.utils import assert_type


class BinaryBooleanOperator(AbstractExpression):
    """
    Abstract expression for binary boolean expressions.
    """

    AND = 'and'
    OR = 'or'

    def __init__(self, kind, lhs, rhs):
        """
        :param str kind: Kind for this binary boolean operator
            (short-circuiting).
        :param AbstractExpression lhs: Left operand.
        :param AbstractExpression rhs: Right operand.
        """
        super(BinaryBooleanOperator, self).__init__()
        assert kind in (self.AND, self.OR)
        self.kind = kind
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: IfExpr
        """
        lhs = construct(self.lhs, BoolType, "Operands of binary logic operator"
                        " must be of boolean type, got {expr_type}")

        rhs = construct(self.rhs, BoolType, "Operands of binary logic operator"
                        " must be of boolean type, got {expr_type}")

        if self.kind == self.AND:
            then = rhs
            else_then = LiteralExpr('False', BoolType)
        else:
            then = LiteralExpr('True', BoolType)
            else_then = rhs
        return If.Expr(lhs, then, else_then, BoolType)


# noinspection PyPep8Naming
def And(*args):
    """
    Syntactic sugar for nested "&" operators.

    And(X, Y, Z) is expanded into X & (Y & Z).

    :param list[AbstractExpression] args: Operands.
    :rtype: BinaryBooleanOperator
    """
    return reduce(lambda a, b: a & b, args)


# noinspection PyPep8Naming
def Or(*args):
    """
    Syntactic sugar for nested "|" operators.

    Or(X, Y, Z) is expanded into X | (Y | Z).

    :param list[AbstractExpression] args: Operands.
    :rtype: BinaryBooleanOperator
    """
    return reduce(lambda a, b: a | b, args)


class Eq(AbstractExpression):
    """
    Expression for equality test expression.
    """

    class Expr(ResolvedExpression):
        def __init__(self, lhs, rhs):
            self.lhs = lhs
            self.rhs = rhs

        @property
        def type(self):
            return BoolType

        def render_pre(self):
            return '{}\n{}'.format(
                self.lhs.render_pre(),
                self.rhs.render_pre()
            )

        def render_expr(self):
            return '{} = {}'.format(self.lhs.render_expr(),
                                    self.rhs.render_expr())

        def __repr__(self):
            return '<Eq.Expr>'

    def __init__(self, lhs, rhs):
        """
        :param AbstractExpression lhs: Left operand.
        :param AbstractExpression rhs: Right operand.
        """
        super(Eq, self).__init__()
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: EqExpr
        """
        from langkit.expressions.structs import Cast
        lhs = construct(self.lhs)
        rhs = construct(self.rhs)

        # Don't use CompiledType.matches since in the generated code, we need
        # both operands to be *exactly* the same types, so handle specifically
        # each case.
        if issubclass(lhs.type, ASTNode):
            # Handle checks between two subclasses without explicit casts. In
            # order to help users to detect dubious checks, forbid operands
            # that can never be equal because they have no subclass in common.
            if issubclass(lhs.type, rhs.type):
                lhs = Cast.Expr(lhs, assert_type(rhs.type, ASTNode))
            elif issubclass(rhs.type, lhs.type):
                rhs = Cast.Expr(rhs, assert_type(lhs.type, ASTNode))
            else:
                assert False, '{} and {} values are never equal'.format(
                    lhs.type.name().camel, rhs.type.name().camel
                )
        else:
            check_source_language(
                lhs.type == rhs.type,
                'Incompatible types for equality: {} and {}'.format(
                    lhs.type.name().camel, rhs.type.name().camel
                )
            )

        return Eq.Expr(lhs, rhs)


class OrderingTest(AbstractExpression):
    """
    Expression for ordering test expression (less than, greater than).
    """

    LT = 'lt'  # Less than (strict)
    LE = 'le'  # Less than or equal
    GT = 'gt'  # Greater than (strict)
    GE = 'ge'  # Greater than or equal

    OPERATOR_IMAGE = {
        LT: '<',
        LE: '<=',
        GT: '>',
        GE: '>=',
    }

    class Expr(ResolvedExpression):
        def __init__(self, operator, lhs, rhs):
            self.operator = operator
            self.lhs = lhs
            self.rhs = rhs

        @property
        def type(self):
            return BoolType

        def render_pre(self):
            return '{}\n{}'.format(
                self.lhs.render_pre(),
                self.rhs.render_pre()
            )

        def render_expr(self):
            return '{} {} {}'.format(
                self.lhs.render_expr(),
                OrderingTest.OPERATOR_IMAGE[self.operator],
                self.rhs.render_expr()
            )

        def __repr__(self):
            return '<OrderingTest.Expr {}>'.format(self.operator)

    def __init__(self, operator, lhs, rhs):
        """
        :param AbstractExpression lhs: Left operand.
        :param AbstractExpression rhs: Right operand.
        """
        super(OrderingTest, self).__init__()
        assert operator in OrderingTest.OPERATOR_IMAGE
        self.operator = operator
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: OrderingTest.Expr
        """
        return OrderingTest.Expr(self.operator, *[
            construct(e, LongType,
                      "Comparisons only work on scalars, not {expr_type}")
            for e in (self.lhs, self.rhs)
        ])


class If(AbstractExpression):
    """
    Abstract expression for a conditional expression.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression for a conditional expression.
        """

        def __init__(self, cond, then, else_then, rtype):
            """
            :param ResolvedExpression cond: A boolean expression.
            :param ResolvedExpression then: If "cond" is evaluated to true,
                this part is returned.
            :param ResolvedExpression else_then: If "cond" is evaluated to
                false, this part is returned.
            :param langkit.compiled_types.CompiledType rtype: Type parameter.
                The type that is returned by then and else_then.
            """
            self.cond = cond
            self.then = then
            self.else_then = else_then
            self.rtype = rtype
            self.result_var = PropertyDef.get().vars.create('Result', rtype)

        @property
        def type(self):
            return self.rtype

        def render_pre(self):
            return render('properties/if_ada', expr=self)

        def render_expr(self):
            return self.result_var.name.camel_with_underscores

        def __repr__(self):
            return '<If.Expr>'

    def __init__(self, cond, then, else_then):
        """
        :param AbstractExpression cond: A boolean expression.
        :param AbstractExpression then: If "cond" is evaluated to true, this
            part is returned.
        :param AbstractExpression else_then: If "cond" is evaluated to false,
            this part is returned.
        """
        super(If, self).__init__()
        self.cond = cond
        self.then = then
        self.else_then = else_then

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: IfExpr
        """
        then = construct(self.then)
        else_then = construct(self.else_then)
        rtype = then.type.unify(else_then.type)
        return If.Expr(construct(self.cond, BoolType), then, else_then, rtype)


class Not(AbstractExpression):
    """
    Expression for "not" boolean expressions.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr):
            self.expr = expr

        @property
        def type(self):
            return BoolType

        def render_pre(self):
            return self.expr.render_pre()

        def render_expr(self):
            return 'not ({})'.format(self.expr.render_expr())

        def __repr__(self):
            return '<Not.Expr>'

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Operand for the "not" expression.
        """
        super(Not, self).__init__()
        self.expr = expr

    def construct(self):
        return Not.Expr(construct(self.expr, BoolType))


class Then(AbstractExpression):
    """
    Expression for the then boolean combinator that works as follows::

        expression.then(
            lambda expr_result: arbitrary_expression, default_expression
        )

    This property code will evaluate the arbitrary expression if expression
    evaluates to a not null result, and will evaluate the default_expression
    otherwise.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr, var_expr, then_expr, default_expr):
            self.expr = expr
            self.var_expr = var_expr
            self.then_expr = then_expr
            self.default_expr = default_expr
            self.result = PropertyDef.get().vars.create("Result_Var",
                                                        self.type)

        @property
        def type(self):
            return self.then_expr.type

        def render_pre(self):
            return render('properties/then_ada', then=self)

        def render_expr(self):
            return self.result.name.camel_with_underscores

        def __repr__(self):
            return '<Then.Expr>'

    def __init__(self, expr, then_fn, default_val=None):
        """
        :param AbstractExpression expr: The expression to use as a source for
            the then. Must be of a pointer type.
        :param (AbstractExpression) -> AbstractExpression then_fn: The
            function describing the expression to compute if expr is not null.
        :param AbstractExpression default_val: The expression to use as
            fallback if expr is null.
        """
        super(Then, self).__init__()
        self.expr = expr
        self.then_fn = then_fn
        self.default_val = default_val
        self.var_expr = self.then_expr = None

    def do_prepare(self):
        self.var_expr = AbstractVariable(names.Name("Var_Expr"),
                                         create_local=True)
        self.then_expr = self.then_fn(self.var_expr)

    def construct(self):
        # Accept as a prefix:
        # * any pointer, since it can be checked against "null";
        # * any Struct, since its "Is_Null" field can be checked.
        expr = construct(self.expr,
                         lambda cls: cls.is_ptr or issubclass(cls, Struct))
        self.var_expr.set_type(expr.type)

        then_expr = construct(self.then_expr)

        # Affect default value to the fallback expression. For the moment,
        # only booleans and structs are handled.
        if not self.default_val:
            if then_expr.type.matches(BoolType):
                default_expr = construct(False)
            elif issubclass(then_expr.type, Struct):
                default_expr = construct(No(
                    # Because we're doing issubclass instead of isinstance,
                    # PyCharm do not understand that then_exp.type is a Struct,
                    # so the following is necessary not to have warnings.
                    assert_type(then_expr.type, Struct)
                ))
            else:
                raise AssertionError(
                    "Then expression should have a default value provided, "
                    "in cases where the provided function's return type is "
                    "not Bool, here {}".format(then_expr.type.name().camel)
                )
        else:
            default_expr = construct(self.default_val, then_expr.type)

        return Then.Expr(expr, construct(self.var_expr), then_expr,
                         default_expr)
