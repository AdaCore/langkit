import inspect

from langkit import names
from langkit.compiled_types import (
    ASTNode, BoolType, EquationType, LexicalEnvType, LongType, Struct, Symbol
)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, LiteralExpr, No, PropertyDef,
    ResolvedExpression, render, construct, BuiltinCallExpr, BasicExpr,
    attr_call
)
from langkit.expressions.envs import EmptyEnv
from langkit.utils import assert_type


@attr_call('and_then', 'and')
@attr_call('or_else', 'or')
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
        def construct_op(op):
            return construct(op, lambda t: t in (BoolType, EquationType),
                             "Operands of binary logic operator must be of "
                             "boolean or equation type, got {expr_type}")

        lhs, rhs = map(construct_op, [self.lhs, self.rhs])

        check_source_language(
            lhs.type is rhs.type, "Left and right operands to binary logic "
            "operator should have the same type"
        )

        if lhs.type is BoolType:
            # Boolean case
            if self.kind == self.AND:
                then = rhs
                else_then = LiteralExpr('False', BoolType)
            else:
                then = LiteralExpr('True', BoolType)
                else_then = rhs
            return If.Expr(lhs, then, else_then, BoolType)
        else:
            # Equation case
            return BuiltinCallExpr(
                names.Name("Logic") + names.Name.from_lower(self.kind),
                EquationType, [lhs, rhs],
                '{}_Pred'.format(self.kind.capitalize())
            )


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


@attr_call("equals")
class Eq(AbstractExpression):
    """
    Expression for equality test expression.
    """

    @staticmethod
    def make_expr(lhs, rhs):
        return BasicExpr('{} = {}', BoolType, [lhs, rhs])

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
                check_source_language(
                    False, '{} and {} values are never equal'.format(
                        lhs.type.name().camel, rhs.type.name().camel
                    )
                )
        else:
            check_source_language(
                lhs.type == rhs.type,
                'Incompatible types for equality: {} and {}'.format(
                    lhs.type.name().camel, rhs.type.name().camel
                )
            )

        return self.make_expr(lhs, rhs)


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
        static_type = BoolType
        pretty_class_name = 'OrdTest'

        def __init__(self, operator, lhs, rhs):
            self.operator = operator
            self.lhs = lhs
            self.rhs = rhs

            super(OrderingTest.Expr, self).__init__()

        def _render_pre(self):
            return '{}\n{}'.format(
                self.lhs.render_pre(),
                self.rhs.render_pre()
            )

        def _render_expr(self):
            return '{} {} {}'.format(
                self.lhs.render_expr(),
                OrderingTest.OPERATOR_IMAGE[self.operator],
                self.rhs.render_expr()
            )

        @property
        def subexprs(self):
            return {'op': self.operator, 'lhs': self.lhs, 'rhs': self.rhs}

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

        pretty_class_name = 'If'

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
            self.static_type = rtype
            self.result_var = PropertyDef.get().vars.create('Result', rtype)

            super(If.Expr, self).__init__()

        def _render_pre(self):
            return render('properties/if_ada', expr=self)

        def _render_expr(self):
            return self.result_var.name.camel_with_underscores

        @property
        def subexprs(self):
            return {'0-cond': self.cond,
                    '1-then': self.then,
                    '2-else': self.else_then}

        def __repr__(self):
            return '<If.Expr>'

    def __init__(self, cond, then, else_then):
        """
        :param cond: A boolean expression.
        :param then: If "cond" is evaluated to true, this
            part is returned.
        :param else_then: If "cond" is evaluated to false,
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
        check_source_language(
            then.type.matches(else_then.type),
            "Mismatching types in If expression: {} and {}".format(
                then.type.name().camel, else_then.type.name().camel
            )
        )
        rtype = then.type.unify(else_then.type)

        # If then/else_then have actually subtypes of the unified result type,
        # we need to perform a conversion for the Ada code generation.
        if then != rtype:
            then = BuiltinCallExpr(rtype.name(), rtype, [then])
        if else_then != rtype:
            else_then = BuiltinCallExpr(rtype.name(), rtype, [else_then])

        return If.Expr(construct(self.cond, BoolType), then, else_then, rtype)


class Not(AbstractExpression):
    """
    Expression for "not" boolean expressions.
    """

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Operand for the "not" expression.
        """
        super(Not, self).__init__()
        self.expr = expr

    def construct(self):
        return Not.make_expr(construct(self.expr, BoolType))

    @staticmethod
    def make_expr(expr):
        return BasicExpr('not ({})', BoolType, [expr])

    @property
    def subexprs(self):
        return [self.expr]


@attr_call('then')
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
        pretty_name = 'Then'

        def __init__(self, expr, var_expr, then_expr, default_expr):
            self.expr = expr
            self.var_expr = var_expr
            self.then_expr = then_expr
            self.default_expr = default_expr
            self.static_type = self.then_expr.type
            self.result_var = PropertyDef.get().vars.create("Result_Var",
                                                            self.type)

            super(Then.Expr, self).__init__()

        def _render_pre(self):
            return render('properties/then_ada', then=self)

        def _render_expr(self):
            return self.result_var.name.camel_with_underscores

        @property
        def subexprs(self):
            return {'0-prefix': self.expr,
                    '1-then': self.then_expr,
                    '2-default': self.default_expr}

        def _bindings(self):
            return [self.var_expr]

        def __repr__(self):
            return '<Then.Expr>'

    @staticmethod
    def create_from_exprs(base, then_expr, var_expr):
        """
        Create a Then expression without going through a lambda. Used
        internally to constructs then expressions for the underscore operator.
        """
        ret = Then(base, None)
        ret.then_expr = then_expr
        ret.var_expr = var_expr
        return ret

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
        self.underscore_then = False

    def do_prepare(self):
        # If this Then was created using create_from exprs, there is no lambda
        # expansion to do.
        if self.then_expr:
            return

        argspec = inspect.getargspec(self.then_fn)
        check_source_language(
            len(argspec.args) == 1
            and not argspec.varargs
            and not argspec.keywords
            and not argspec.defaults,
            'Invalid lambda for Then expression: exactly one parameter is'
            ' required, without a default value'
        )

        self.var_expr = AbstractVariable(
            names.Name("Var_Expr"), create_local=True,
            source_name=names.Name(argspec.args[0]))
        self.then_expr = self.then_fn(self.var_expr)

    def construct(self):
        # Add var_expr to the scope for this Then expression
        PropertyDef.get_scope().add(self.var_expr.local_var)

        # Accept as a prefix:
        # * any pointer, since it can be checked against "null";
        # * any Struct, since its "Is_Null" field can be checked.
        expr = construct(self.expr,
                         lambda cls: cls.is_ptr or issubclass(cls, Struct))
        self.var_expr.set_type(expr.type)

        then_expr = construct(self.then_expr)

        # Affect default value to the fallback expression. For the moment,
        # only booleans and structs are handled.
        if self.default_val is None:
            if then_expr.type.matches(BoolType):
                default_expr = construct(False)
            elif issubclass(then_expr.type, Struct):
                default_expr = construct(No(
                    # Because we're doing issubclass instead of isinstance,
                    # PyCharm do not understand that then_exp.type is a Struct,
                    # so the following is necessary not to have warnings.
                    assert_type(then_expr.type, Struct)
                ))
            elif then_expr.type.matches(LexicalEnvType):
                default_expr = construct(EmptyEnv)
            elif then_expr.type.matches(Symbol):
                default_expr = LiteralExpr(Symbol.nullexpr(), Symbol)
            else:
                # The following is not actually used but PyCharm's typer
                # requires it.
                default_expr = None

                check_source_language(
                    False,
                    "Then expression should have a default value provided, "
                    "in cases where the provided function's return type is "
                    "not Bool, here {}".format(then_expr.type.name().camel)
                )
        else:
            default_expr = construct(self.default_val, then_expr.type)

        return Then.Expr(expr, construct(self.var_expr), then_expr,
                         default_expr)

    def __repr__(self):
        return "<Then {}: {} {}>".format(self.expr, self.var_expr,
                                         self.then_expr)
