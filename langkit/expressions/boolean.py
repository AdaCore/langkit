from __future__ import annotations

import abc
import enum
from functools import reduce
import funcy
import inspect

from langkit import names
from langkit.compiled_types import CompiledType, T, TypeRepo
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, BasicExpr, BindingScope, CallExpr,
    ComputingExpr, LambdaArgInfo, LiteralExpr, PropertyDef, ResolvedExpression,
    Self, SequenceExpr, attr_call, construct, dsl_document, expr_or_null,
    render, sloc_info_arg
)


@attr_call('and_then')
def and_then(lhs, rhs):
    """
    If `lhs` and `rhs` are booleans, this evaluates them in a short-circuit AND
    boolean operator fashion. Otherwise, both must be equations, and this
    returns a new equation that describes the logical conjunction.
    """
    return BinaryBooleanOperator('and', lhs, rhs)


@attr_call('or_else')
def or_else(lhs, rhs):
    """
    Like :dsl:`and_then`, but for the OR boolean operator or the logical
    disjunction.
    """
    return BinaryBooleanOperator('or', lhs, rhs)


class BinaryOpKind(enum.Enum):
    AND = "and"
    OR = "or"


class BaseBinaryOp(AbstractExpression):
    """
    Base class for boolean and logic binary operators.
    """
    def __init__(self, kind: BinaryOpKind, lhs, rhs):
        """
        :param kind: Kind for this binary boolean operator.
        :param lhs: Left operand.
        :param rhs: Right operand.
        """
        super().__init__()
        self.kind = kind
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self):
        return (
            f"<{type(self).__name__} {self.kind.value.capitalize()}"
            f" at {self.location_repr}>"
        )


class AbstractBinaryOp(BaseBinaryOp):
    """
    Base class or binary operators that work exclusively on booleans or on
    logic equations.
    """
    @staticmethod
    @abc.abstractmethod
    def operand_type() -> CompiledType:
        """
        Static method that returns the operand/result type for this operator.
        """
        pass

    def construct(self) -> ResolvedExpression:
        t = self.operand_type()
        lhs = construct(self.lhs, t)
        rhs = construct(self.rhs, t)
        return self.common_construct(self.kind, lhs, rhs, self)


class BooleanBinaryOp(AbstractBinaryOp):
    """
    Boolean binary operator expression.
    """
    @staticmethod
    def operand_type() -> CompiledType:
        return T.Bool

    @staticmethod
    def common_construct(
        kind: BinaryOpKind,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
        abstract_expr: AbstractExpression,
    ) -> ResolvedExpression:
        """
        Common code to build a resolved expression for and/or expressions.
        """
        then: ResolvedExpression
        else_then: ResolvedExpression
        if kind == BinaryOpKind.AND:
            then = rhs
            else_then = LiteralExpr("False", T.Bool)
        else:
            then = LiteralExpr("True", T.Bool)
            else_then = rhs
        return If.Expr(lhs, then, else_then, abstract_expr=abstract_expr)


class LogicBinaryOp(AbstractBinaryOp):
    """
    Logic binary operator expression.
    """
    @staticmethod
    def operand_type() -> CompiledType:
        return T.Equation

    @staticmethod
    def common_construct(
        kind: BinaryOpKind,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
        abstract_expr: AbstractExpression,
    ) -> ResolvedExpression:
        """
        Common code to build a resolved expression for %and/%or expressions.
        """
        kind_name = kind.value.capitalize()
        return CallExpr(
            f"{kind_name}_Pred",
            f"Create_{kind_name}",
            T.Equation,
            [lhs, rhs, sloc_info_arg(abstract_expr.location)],
            abstract_expr=abstract_expr,
        )


class BinaryBooleanOperator(BaseBinaryOp):
    """
    Binary operator expression that works equally on booleans and logic
    equations. Note that this is specific to the Python DSL: this kind of
    expression does not exist in Lkt: we will get rid of this class once we can
    stop supporting the Python DSL.
    """
    def __init__(
        self,
        kind: BinaryOpKind,
        lhs: AbstractExpression,
        rhs: AbstractExpression,
    ):
        super().__init__(kind, lhs, rhs)
        self._is_equation = None

    @property
    def is_equation(self) -> bool:
        """
        Whether this expression computes an equation.

        Computed during the "construct" pass.
        """
        assert self._is_equation is not None
        return self._is_equation

    def construct(self):
        def construct_op(op):
            return construct(op, lambda t: t in (T.Bool, T.Equation),
                             "Operands of binary logic operator must be of "
                             "boolean or equation type, got {expr_type}")

        lhs, rhs = map(construct_op, [self.lhs, self.rhs])

        check_source_language(
            lhs.type is rhs.type, "Left and right operands to binary logic "
            "operator should have the same type"
        )

        if lhs.type is T.Bool:
            # Boolean case
            self._is_equation = False
            return BooleanBinaryOp.common_construct(self.kind, lhs, rhs, self)

        else:
            # Equation case
            self._is_equation = True
            return LogicBinaryOp.common_construct(self.kind, lhs, rhs, self)


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


@attr_call("any_of")
class AnyOf(AbstractExpression):
    """
    Return whether ``expr`` is equal to one of ``values``.
    """

    def __init__(self, expr, *values):
        super().__init__()
        self.expr = expr
        self.values = values

    def construct(self) -> ResolvedExpression:
        # Lower all sub-expressions. If the prefix is a node/entity, then
        # we only require that other operands are nodes/entities themselves.
        # Otherwise, operand types must match the prefix's.
        expr = construct(self.expr)
        expected_type: TypeRepo.Defer | CompiledType
        if expr.type.is_ast_node:
            expected_type = T.root_node
        elif expr.type.is_entity_type:
            expected_type = T.entity
        else:
            expected_type = expr.type
        values = [construct(v, expected_type) for v in self.values]
        assert len(values) >= 1

        # Make sure the prefix has a result variable so that equality tests do
        # not re-evaluate it over and over.
        expr_var = expr.create_result_var("Any_Of_Prefix")
        result = Eq.make_expr(expr_var, values.pop())
        while values:
            result = If.Expr(
                Eq.make_expr(expr_var, values.pop()),
                LiteralExpr("True", T.Bool),
                result,
            )

        return SequenceExpr(expr, result, abstract_expr=self)


@attr_call("equals")
class Eq(AbstractExpression):
    """
    Return whether `lhs` equals `rhs`.
    """

    @classmethod
    def make_expr(cls, lhs, rhs, abstract_expr=None):
        if lhs.type.is_entity_type:
            return cls.make_expr_for_entities(lhs, rhs, abstract_expr)
        elif lhs.type.has_equivalent_function:
            return CallExpr('Is_Equal', 'Equivalent', T.Bool, [lhs, rhs],
                            abstract_expr=abstract_expr)
        else:
            return BasicExpr('Is_Equal', '{} = {}', T.Bool, [lhs, rhs],
                             abstract_expr=abstract_expr)

    @staticmethod
    def make_expr_for_entities(lhs, rhs, abstract_expr=None):
        from langkit.expressions.structs import Cast

        if lhs.type != T.entity:
            lhs = Cast.Expr(lhs, T.entity)
        if rhs.type != T.entity:
            rhs = Cast.Expr(rhs, T.entity)
        return CallExpr('Is_Equiv', 'Equivalent', T.Bool, [lhs, rhs],
                        abstract_expr=abstract_expr)

    def __init__(self, lhs, rhs):
        """
        :param AbstractExpression lhs: Left operand.
        :param AbstractExpression rhs: Right operand.
        """
        super().__init__()
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
                    lhs.type.dsl_name, rhs.type.dsl_name
                )
            )

        def check_never_equal(can_be_equal):
            check_source_language(
                can_be_equal,
                '{} and {} values are never equal'.format(
                    lhs.type.dsl_name, rhs.type.dsl_name
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
                lhs.type.element_type.matches(rhs.type.element_type)
                or rhs.type.element_type.matches(lhs.type.element_type)
            )
            return self.make_expr_for_entities(lhs, rhs, self)

        else:
            check_type_compatibility(lhs.type == rhs.type)

        return self.make_expr(lhs, rhs, self)


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

            super().__init__(
                'Comp_Result', template, T.Bool, [lhs, rhs],
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
        super().__init__()
        assert operator in OrderingTest.OPERATOR_IMAGE
        self.operator = operator
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: OrderingTest.Expr
        """
        lhs, rhs = construct(self.lhs), construct(self.rhs)
        check_source_language(
            lhs.type.is_long_type or
            lhs.type.is_big_integer_type or
            lhs.type.is_ast_node,
            'Comparisons only work on {}, {} or nodes not {}'
            .format(T.Int.dsl_name, T.BigInt.dsl_name,
                    lhs.type.dsl_name)
        )

        # If we are comparing two nodes, just use the dedicated helper
        if lhs.type.is_ast_node:
            check_source_language(
                rhs.type.is_ast_node,
                'A node can only be compared to another node (got {} and {})'
                .format(lhs.type.dsl_name, rhs.type.dsl_name)
            )
            relation = {self.LT: 'Less_Than',
                        self.LE: 'Less_Or_Equal',
                        self.GT: 'Greater_Than',
                        self.GE: 'Greater_Or_Equal'}[self.operator]
            return CallExpr(
                'Node_Comp',
                'Compare',
                T.Bool,
                [construct(Self), lhs, rhs, relation],
                abstract_expr=self,
            )

        # Otherwise, expect strict equality for both operands and use the
        # native comparison operator for code generation.
        check_source_language(
            lhs.type == rhs.type,
            'Comparisons require the same type for both operands'
            ' (got {} and {})'.format(lhs.type.dsl_name, rhs.type.dsl_name)
        )
        return OrderingTest.Expr(self.operator, lhs, rhs)

    def __repr__(self):
        return f"<OrderingTest {repr(self.operator)} at {self.location_repr}>"


@dsl_document
class If(AbstractExpression):
    """
    Return `then` if `cond` is true, return `else_then` otherwise.
    """

    class Expr(ComputingExpr):
        """
        Resolved expression for a conditional expression.
        """

        pretty_class_name = 'If'

        def __init__(self, cond, then, else_then, abstract_expr=None):
            """
            :param ResolvedExpression cond: A boolean expression.
            :param ResolvedExpression then: If "cond" is evaluated to true,
                this part is returned.
            :param ResolvedExpression else_then: If "cond" is evaluated to
                false, this part is returned.
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.cond = cond
            self.then = then
            self.else_then = else_then
            self.static_type = then.type

            super().__init__('If_Result', abstract_expr=abstract_expr)

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
        super().__init__()
        self.cond = cond
        self._then = then
        self.else_then = else_then

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: If.Expr
        """
        then, else_then = expr_or_null(self._then, self.else_then,
                                       'If expression', None)
        return If.Expr(construct(self.cond, T.Bool), then, else_then,
                       abstract_expr=self)


@dsl_document
class Not(AbstractExpression):
    """
    Return true if `expr` is false and conversely.
    """

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Operand for the "not" expression.
        """
        super().__init__()
        self.expr = expr

    def construct(self):
        return Not.make_expr(construct(self.expr, T.Bool),
                             abstract_expr=self)

    @staticmethod
    def make_expr(expr, abstract_expr=None):
        return BasicExpr('Not_Val', 'not ({})', T.Bool, [expr],
                         abstract_expr=abstract_expr)


@attr_call('then')
class Then(AbstractExpression):
    """
    Evaluate and return the result of `then_fn` if `expr` is not null.
    Otherwise, evaluate and return the result of `default_val`.

    For instance, to evaluate the property of a node or return false when this
    node is null::

        node.then(lambda n: n.my_property,
                  False)
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

            super().__init__('Result_Var', abstract_expr=abstract_expr)

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
    def create_from_exprs(
        base: AbstractExpression,
        then_expr: AbstractExpression,
        lambda_arg_infos: list[LambdaArgInfo],
        var_expr: AbstractVariable,
        default_val: AbstractExpression | None = None,
    ):
        """
        Create a Then expression without going through a lambda. Used
        internally to constructs then expressions for the underscore operator.
        """
        ret = Then(expr=base, then_fn=None, default_val=default_val)
        ret.then_expr = then_expr
        ret.lambda_arg_infos = lambda_arg_infos
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
        super().__init__()
        self.expr = expr
        self.then_fn = then_fn
        self.default_val = default_val
        self.var_expr = self.then_expr = None
        self.underscore_then = False
        self.lambda_arg_infos = []

    def do_prepare(self):
        # If this Then was created using create_from exprs, there is no lambda
        # expansion to do.
        if self.then_expr:
            return

        argspec = inspect.getfullargspec(self.then_fn)
        check_source_language(
            len(argspec.args) == 1
            and not argspec.varargs
            and not argspec.varkw
            and not argspec.defaults,
            'Invalid lambda for Then expression: exactly one parameter is'
            ' required, without a default value'
        )

        self.var_expr = AbstractVariable(
            names.Name("Var_Expr"),
            create_local=True,
            source_name=argspec.args[0]
        )
        self.then_expr = self.then_fn(self.var_expr)

    def construct(self):
        # Accept as a prefix all types that can have a null value
        expr = construct(
            self.expr,
            lambda cls: cls.null_allowed,
            'Invalid prefix type for .then: {expr_type}'
        )
        self.var_expr.set_type(expr.type)

        # Now that the variable is typed, ensure that its type annotation in
        # the lambda expression (if present) is correct.
        LambdaArgInfo.check_list(self.lambda_arg_infos)

        # Create a then-expr specific scope to restrict the span of the "then"
        # variable in the debugger.
        with PropertyDef.get_scope().new_child() as then_scope:
            then_scope.add(self.var_expr.local_var)
            then_expr = construct(self.then_expr)
            var_expr = construct(self.var_expr)
        then_expr = BindingScope(then_expr, [var_expr], scope=then_scope)

        # Affect default value to the fallback expression
        then_expr, default_expr = expr_or_null(
            then_expr, self.default_val,
            'Then expression', "function's return type"
        )

        return Then.Expr(expr, construct(self.var_expr), then_expr,
                         default_expr, then_scope)


@dsl_document
class Cond(AbstractExpression):
    """
    Evaluate a specific expression depending on a chain of expressions in an
    IF/ELSE IF/.../ELSE fashion.

    For instance, the following::

        Cond(cond1, expr1,
             cond2, expr2,
             cond3, expr3,
             expr4)

    Is equivalent to::

        If(cond1, expr1,
           If(cond2, expr2,
              If(cond3, expr3,
                 expr4)))

    Since there is always one condition per expression except for the last
    expression, `args` must contain an odd number of expressions.
    """

    def __init__(self, *args):
        """
        :param list[AbstractExpression] args: List of operands. They must have
            the following organization::

                arg 1   (boolean): arg 2 (T)
                arg 3   (boolean): arg 4 (T)
                ...
                arg N-1 (boolean): arg N (T)
                -                  arg N+1 (T)

            This has the same semantics as::

                if arg 1      then arg 2
                elsif arg 3   then arg 4
                ...
                elsif arg N-1 then arg N
                else               arg N+1
        """
        super().__init__()
        self.args = args

    @property
    def branches(self):
        """
        Return the branches for this cond expression.

        This returns all couples of expressions (condition, return value), i.e.
        all result expressions except the fallback one (the constructor's last
        argument).

        :rtype: list[(AbstractExpression, AbstractExpression)]
        """
        return funcy.lpartition(2, self.args)

    @property
    def else_expr(self):
        """
        Return the else expr for this expression.

        :rtype: AbstractExpression
        """
        return self.args[-1]

    def construct(self):
        from langkit.expressions import Cast

        check_source_language(len(self.args) > 1, 'Missing Cond arguments')
        check_source_language(len(self.args) % 2 == 1,
                              'Missing last Cond argument')

        # Lower each pair of condition/expression in resolved expression
        pairs = [(
            construct(a_cond, T.Bool,
                      custom_msg='Condition in Cond expression should be'
                                 ' {expected}, got {expr_type}'),
            construct(a_expr)
        ) for a_cond, a_expr in self.branches]

        else_expr = construct(self.else_expr)

        # Unify types for all return expression
        rtype = else_expr.type
        for _, expr in pairs:
            rtype = rtype.unify(
                expr.type,
                'Mismatching types in Cond expression: {self} expected but got'
                ' {other} instead'
            )

        # Since we have subtypes in the DSL but incompatible types in Ada (for
        # instance for AST nodes, or entities), we sometimes need to convert
        # sub-expressions to the returned type.
        def cast_if_needed(expr):
            return expr if expr.type == rtype else Cast.Expr(expr, rtype)

        # Build this Cond as a big resolved expression
        result = cast_if_needed(else_expr)
        for cond, expr in reversed(pairs):
            result = If.Expr(cond, cast_if_needed(expr), result)
        result.abstract_expr = self

        return result
