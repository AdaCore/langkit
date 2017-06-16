from __future__ import absolute_import, division, print_function
import inspect

from langkit import names
from langkit.compiled_types import (
    T, analysis_unit_type, bool_type, equation_type, lexical_env_type,
    long_type, symbol_type
)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, BasicExpr, BindingScope, CallExpr,
    ComputingExpr, LiteralExpr, No, NullExpr, PropertyDef, attr_call,
    construct, render
)
from langkit.expressions.envs import EmptyEnv


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
            return construct(op, lambda t: t in (bool_type, equation_type),
                             "Operands of binary logic operator must be of "
                             "boolean or equation type, got {expr_type}")

        lhs, rhs = map(construct_op, [self.lhs, self.rhs])

        check_source_language(
            lhs.type is rhs.type, "Left and right operands to binary logic "
            "operator should have the same type"
        )

        if lhs.type is bool_type:
            # Boolean case
            if self.kind == self.AND:
                then = rhs
                else_then = LiteralExpr('False', bool_type)
            else:
                then = LiteralExpr('True', bool_type)
                else_then = rhs
            return If.Expr(lhs, then, else_then, bool_type)

        else:
            # Equation case
            kind_name = self.kind.capitalize()
            return CallExpr(
                '{}_Pred'.format(kind_name), 'Logic_{}'.format(kind_name),
                equation_type, [lhs, rhs],
                abstract_expr=self
            )

    def __repr__(self):
        return '<{}>'.format(self.kind.capitalize())


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

    @classmethod
    def make_expr(cls, lhs, rhs, abstract_expr=None):
        return (cls.make_expr_for_entities(lhs, rhs, abstract_expr)
                if lhs.type.is_entity_type else
                BasicExpr('Is_Equal', '{} = {}', bool_type, [lhs, rhs],
                          abstract_expr=abstract_expr))

    @staticmethod
    def make_expr_for_entities(lhs, rhs, abstract_expr=None):
        from langkit.expressions.structs import Cast

        if lhs.type != T.entity:
            lhs = Cast.Expr(lhs, T.entity)
        if rhs.type != T.entity:
            rhs = Cast.Expr(rhs, T.entity)
        return CallExpr('Is_Equiv', 'Is_Equivalent', bool_type,
                        [lhs, rhs],
                        abstract_expr=abstract_expr)

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

        def check_type_compatibility(is_valid):
            check_source_language(
                is_valid,
                'Incompatible types for equality: {} and {}'.format(
                    lhs.type.name.camel, rhs.type.name.camel
                )
            )

        def check_never_equal(can_be_equal):
            check_source_language(
                can_be_equal,
                '{} and {} values are never equal'.format(
                    lhs.type.name.camel, rhs.type.name.camel
                )
            )

        # Don't use CompiledType.matches since in the generated code, we need
        # both operands to be *exactly* the same types, so handle specifically
        # each case.
        if lhs.type.is_ast_node:
            check_type_compatibility(rhs.type.is_ast_node)

            # Handle checks between two subclasses without explicit casts. In
            # order to help users to detect dubious checks, forbid operands
            # that can never be equal because they have no subclass in common.
            if lhs.type.matches(rhs.type):
                lhs = Cast.Expr(lhs, rhs.type)
            elif rhs.type.matches(lhs.type):
                rhs = Cast.Expr(rhs, lhs.type)
            else:
                check_never_equal(False)

        # Likewise for entities. Moreover, we need to use a special comparison
        # predicate for them.
        elif lhs.type.is_entity_type:
            check_type_compatibility(rhs.type.is_entity_type)
            check_never_equal(
                lhs.type.el_type.matches(rhs.type.el_type)
                or rhs.type.el_type.matches(lhs.type.el_type)
            )
            return self.make_expr_for_entities(lhs, rhs, self)

        else:
            check_type_compatibility(lhs.type == rhs.type)

        return self.make_expr(lhs, rhs, self)

    def __repr__(self):
        return '<Eq>'


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

    class Expr(BasicExpr):
        pretty_class_name = 'OrdTest'

        def __init__(self, operator, lhs, rhs, abstract_expr=None):
            self.operator = operator
            self.lhs = lhs
            self.rhs = rhs

            template = '{{}} {} {{}}'.format(
                OrderingTest.OPERATOR_IMAGE[self.operator]
            )

            super(OrderingTest.Expr, self).__init__(
                'Comp_Result', template, bool_type, [lhs, rhs],
                abstract_expr=abstract_expr
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
            construct(e, long_type,
                      "Comparisons only work on scalars, not {expr_type}")
            for e in (self.lhs, self.rhs)
        ])


class If(AbstractExpression):
    """
    Abstract expression for a conditional expression.
    """

    class Expr(ComputingExpr):
        """
        Resolved expression for a conditional expression.
        """

        pretty_class_name = 'If'

        def __init__(self, cond, then, else_then, rtype, abstract_expr=None):
            """
            :param ResolvedExpression cond: A boolean expression.
            :param ResolvedExpression then: If "cond" is evaluated to true,
                this part is returned.
            :param ResolvedExpression else_then: If "cond" is evaluated to
                false, this part is returned.
            :param langkit.compiled_types.CompiledType rtype: The type that is
                returned by then and else_then.
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.cond = cond
            self.then = then
            self.else_then = else_then
            self.static_type = rtype

            super(If.Expr, self).__init__('If_Result',
                                          abstract_expr=abstract_expr)

        def _render_pre(self):
            return render('properties/if_ada', expr=self)

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
        from langkit.expressions import Cast

        then = construct(self.then)
        else_then = construct(self.else_then)
        rtype = then.type.unify(
            else_then.type,
            'Mismatching types in If expression: {self} and {other}'
        )

        # If then/else_then have actually subtypes of the unified result type,
        # we need to perform a conversion for the Ada code generation.
        if then.type != rtype:
            then = Cast.Expr(then, rtype)
        if else_then.type != rtype:
            else_then = Cast.Expr(else_then, rtype)

        return If.Expr(construct(self.cond, bool_type), then, else_then, rtype,
                       abstract_expr=self)

    def __repr__(self):
        return '<If>'


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
        return Not.make_expr(construct(self.expr, bool_type),
                             abstract_expr=self)

    @staticmethod
    def make_expr(expr, abstract_expr=None):
        return BasicExpr('Not_Val', 'not ({})', bool_type, [expr],
                         abstract_expr=abstract_expr)

    def __repr__(self):
        return '<Not>'


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

    class Expr(ComputingExpr):
        pretty_name = 'Then'

        def __init__(self, expr, var_expr, then_expr, default_expr,
                     then_scope, abstract_expr=None):
            self.expr = expr
            self.var_expr = var_expr
            self.then_expr = then_expr
            self.default_expr = default_expr
            self.then_scope = then_scope
            self.static_type = self.then_expr.type

            super(Then.Expr, self).__init__('Result_Var',
                                            abstract_expr=abstract_expr)

        def _render_pre(self):
            return render('properties/then_ada', then=self)

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
        # Accept as a prefix:
        # * any pointer, since it can be checked against "null";
        # * any StructType, since structs are nullable.
        expr = construct(self.expr,
                         lambda cls: cls.is_ptr or cls.is_struct_type)
        self.var_expr.set_type(expr.type)

        # Create a then-expr specific scope to restrict the span of the "then"
        # variable in the debugger.
        with PropertyDef.get_scope().new_child() as then_scope:
            then_scope.add(self.var_expr.local_var)
            then_expr = construct(self.then_expr)
            var_expr = construct(self.var_expr)
        then_expr = BindingScope(then_expr, [var_expr], scope=then_scope)

        # Affect default value to the fallback expression. For the moment,
        # only booleans and structs are handled.
        if self.default_val is None:
            if then_expr.type.matches(bool_type):
                default_expr = construct(False)
            elif then_expr.type.is_base_struct_type:
                default_expr = construct(No(then_expr.type))
            elif then_expr.type.matches(lexical_env_type):
                default_expr = construct(EmptyEnv)
            elif then_expr.type.matches(symbol_type):
                default_expr = NullExpr(symbol_type)
            elif then_expr.type.matches(analysis_unit_type):
                default_expr = construct(No(analysis_unit_type))
            else:
                # The following is not actually used but PyCharm's typer
                # requires it.
                default_expr = None

                check_source_language(
                    False,
                    "Then expression should have a default value provided,"
                    " in cases where the provided function's return type is"
                    " not Bool, here {}".format(then_expr.type.name.camel)
                )
        else:
            default_expr = construct(self.default_val, then_expr.type)

        return Then.Expr(expr, construct(self.var_expr), then_expr,
                         default_expr, then_scope)

    def __repr__(self):
        return "<Then {}: {} {}>".format(self.expr, self.var_expr,
                                         self.then_expr)
