from langkit import names
from langkit.compiled_types import BoolType, ASTNode
from langkit.expressions.base import (
    render, Property, LiteralExpr, AbstractExpression, construct,
    ResolvedExpression, AbstractVariable
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
        :param langkit.expressions.base.AbstractExpression lhs: Left operand.
        :param langkit.expressions.base.AbstractExpression rhs: Right operand.
        """
        assert kind in (self.AND, self.OR)
        self.kind = kind
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: IfExpr
        """
        lhs = construct(self.lhs)
        rhs = construct(self.rhs)
        assert lhs.type.matches(BoolType)
        assert rhs.type.matches(BoolType)

        if self.kind == self.AND:
            then = rhs
            else_then = LiteralExpr('False', BoolType)
        else:
            then = LiteralExpr('True', BoolType)
            else_then = rhs
        return If.Expr(lhs, then, else_then, BoolType)


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

    def __init__(self, lhs, rhs):
        """
        :param langkit.expressions.base.AbstractExpression lhs: Left operand.
        :param langkit.expressions.base.AbstractExpression rhs: Right operand.
        """
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
            assert lhs.type == rhs.type, (
                'Incompatible types for equality: {} and {}'
            ).format(lhs.type.name().camel, rhs.type.name().camel)

        return Eq.Expr(lhs, rhs)


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
            :param langkit.expressions.base.ResolvedExpression cond: A
            boolean expression.
            :param langkit.expressions.base.ResolvedExpression then: If
            "cond" is evaluated to true,
                this part is returned.
            :param langkit.expressions.base.ResolvedExpression else_then: If
            "cond" is evaluated to
                false, this part is returned.
            :param langkit.compiled_types.CompiledType rtype: Type parameter.
                The type that is returned by then and else_then.
            """
            self.cond = cond
            self.then = then
            self.else_then = else_then
            self.rtype = rtype
            self.result_var = Property.get().vars.create('Result', rtype)

        @property
        def type(self):
            return self.rtype

        def render_pre(self):
            return render('properties/if_ada', expr=self)

        def render_expr(self):
            return self.result_var.name.camel_with_underscores

    def __init__(self, cond, then, else_then):
        """
        :param langkit.expressions.base.AbstractExpression cond: A boolean
            expression.
        :param langkit.expressions.base.AbstractExpression then: If "cond"
            is evaluated to true, this part is returned.
        :param langkit.expressions.base.AbstractExpression else_then: If "cond"
            is evaluated to false,
            this part is returned.
        """
        self.cond = cond
        self.then = then
        self.else_then = else_then

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: IfExpr
        """
        cond = self.cond.construct()
        assert cond.type.matches(BoolType)

        then = self.then.construct()
        else_then = self.else_then.construct()

        rtype = then.type.unify(else_then.type)
        return If.Expr(cond, then, else_then, rtype)


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

    def __init__(self, expr):
        """
        :param langkit.expressions.base.AbstractExpression expr: Operand for
            the "not" expression.
        """
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
            self.result = Property.get().vars.create("Result_Var", self.type)

        @property
        def type(self):
            return self.then_expr.type

        def render_pre(self):
            return render('properties/then_ada', then=self)

        def render_expr(self):
            return self.result.name.camel_with_underscores

    def __init__(self, expr, then_fn, default_val=None):
        """
        :param AbstractExpression expr: The expression to use as a source
            for the then. Must be of a pointer type.
        :param (AbstractExpression) -> AbstractExpression then_fn: The
            function describing the expression to compute if expr is not null.
        :param AbstractExpression default_val: The expression to use as
            fallback if expr is null.
        """
        self.expr = expr
        self.then_fn = then_fn
        self.default_val = default_val

    def construct(self):
        expr = construct(self.expr, lambda t: t.is_ptr)
        var_expr = AbstractVariable(names.Name("Var_Expr"), expr.type,
                                    create_local=True)
        then_expr = construct(self.then_fn(var_expr))

        # Affect default value to the fallback expression. For the moment,
        # only booleans are handled.
        if not self.default_val:
            if then_expr.type.matches(BoolType):
                default_expr = construct(False)
            else:
                raise AssertionError(
                    "Then expression should have a default value provided, "
                    "in cases where the provided function's return type is "
                    "not Bool, here {}".format(then_expr.type)
                )
        else:
            default_expr = construct(self.default_val, then_expr.type)

        return Then.Expr(expr, construct(var_expr), then_expr, default_expr)
