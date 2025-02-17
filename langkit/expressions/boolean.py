from __future__ import annotations

import abc
import enum

from langkit.compiled_types import ASTNodeType, CompiledType, T
from langkit.diagnostics import Location, check_source_language
from langkit.expressions.base import (
    AbstractExpression,
    AbstractVariable,
    BasicExpr,
    BindingScope,
    CallExpr,
    ComputingExpr,
    ExprDebugInfo,
    LambdaArgInfo,
    LiteralExpr,
    LocalVars,
    PropertyDef,
    ResolvedExpression,
    SequenceExpr,
    VariableExpr,
    construct,
    dsl_document,
    expr_or_null,
    render,
    sloc_info_arg,
)


class BinaryOpKind(enum.Enum):
    AND = "and"
    OR = "or"


class BaseBinaryOp(AbstractExpression):
    """
    Base class for boolean and logic binary operators.
    """
    def __init__(
        self,
        location: Location,
        kind: BinaryOpKind,
        lhs: AbstractExpression,
        rhs: AbstractExpression,
    ):
        """
        :param kind: Kind for this binary boolean operator.
        :param lhs: Left operand.
        :param rhs: Right operand.
        """
        super().__init__(location)
        self.kind = kind
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self) -> str:
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

    @staticmethod
    def common_construct(
        debug_info: ExprDebugInfo | None,
        kind: BinaryOpKind,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
    ) -> ResolvedExpression:
        raise NotImplementedError

    def construct(self) -> ResolvedExpression:
        t = self.operand_type()
        lhs = construct(self.lhs, t)
        rhs = construct(self.rhs, t)
        return self.common_construct(self.debug_info, self.kind, lhs, rhs)


class BooleanBinaryOp(AbstractBinaryOp):
    """
    Boolean binary operator expression.
    """
    @staticmethod
    def operand_type() -> CompiledType:
        return T.Bool

    @staticmethod
    def common_construct(
        debug_info: ExprDebugInfo | None,
        kind: BinaryOpKind,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
    ) -> ResolvedExpression:
        """
        Common code to build a resolved expression for and/or expressions.
        """
        then: ResolvedExpression
        else_then: ResolvedExpression
        if kind == BinaryOpKind.AND:
            then = rhs
            else_then = LiteralExpr(None, "False", T.Bool)
        else:
            then = LiteralExpr(None, "True", T.Bool)
            else_then = rhs
        return If.Expr(debug_info, lhs, then, else_then)


class LogicBinaryOp(AbstractBinaryOp):
    """
    Logic binary operator expression.
    """
    @staticmethod
    def operand_type() -> CompiledType:
        return T.Equation

    @staticmethod
    def common_construct(
        debug_info: ExprDebugInfo | None,
        kind: BinaryOpKind,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
    ) -> ResolvedExpression:
        """
        Common code to build a resolved expression for %and/%or expressions.
        """
        kind_name = kind.value.capitalize()
        assert debug_info is not None
        return CallExpr(
            debug_info,
            f"{kind_name}_Pred",
            f"Create_{kind_name}",
            T.Equation,
            [lhs, rhs, sloc_info_arg(debug_info.location)],
        )


class BinaryBooleanOperator(BaseBinaryOp):
    """
    Binary operator expression that works equally on booleans and logic
    equations. Note that this is specific to the Python DSL: this kind of
    expression does not exist in Lkt: we will get rid of this class once we can
    stop supporting the Python DSL.
    """
    def construct(self) -> ResolvedExpression:
        def construct_op(op: AbstractExpression) -> ResolvedExpression:
            return construct(op, lambda t: t in (T.Bool, T.Equation),
                             "Operands of binary logic operator must be of "
                             "boolean or equation type, got {expr_type}")

        lhs, rhs = map(construct_op, [self.lhs, self.rhs])

        check_source_language(
            lhs.type is rhs.type, "Left and right operands to binary logic "
            "operator should have the same type"
        )

        return (
            # Boolean case
            BooleanBinaryOp.common_construct(
                self.debug_info, self.kind, lhs, rhs
            )
            if lhs.type is T.Bool else
            # Equation case
            LogicBinaryOp.common_construct(
                self.debug_info, self.kind, lhs, rhs
            )
        )


class AnyOf(AbstractExpression):
    """
    Return whether ``expr`` is equal to one of ``values``.
    """

    def __init__(
        self,
        location: Location,
        expr: AbstractExpression,
        *values: AbstractExpression,
    ):
        super().__init__(location)
        self.expr = expr
        self.values = values

    def construct(self) -> ResolvedExpression:
        # Lower all sub-expressions. If the prefix is a node/entity, then
        # we only require that other operands are nodes/entities themselves.
        # Otherwise, operand types must match the prefix's.
        expr = construct(self.expr)
        expected_type: CompiledType
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
        result = Eq.make_expr(None, expr_var, values.pop())
        while values:
            result = If.Expr(
                None,
                Eq.make_expr(None, expr_var, values.pop()),
                LiteralExpr(None, "True", T.Bool),
                result,
            )

        return SequenceExpr(self.debug_info, expr, result)


class Eq(AbstractExpression):
    """
    Return whether ``lhs`` equals ``rhs``.
    """

    @classmethod
    def make_expr(
        cls,
        debug_info: ExprDebugInfo | None,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
    ) -> ResolvedExpression:
        if lhs.type.is_entity_type:
            return cls.make_expr_for_entities(debug_info, lhs, rhs)
        elif lhs.type.has_equivalent_function:
            return CallExpr(
                debug_info,
                "Is_Equal",
                "Equivalent",
                T.Bool,
                [lhs, rhs],
            )
        else:
            return BasicExpr(
                debug_info, "Is_Equal", "{} = {}", T.Bool, [lhs, rhs]
            )

    @staticmethod
    def make_expr_for_entities(
        debug_info: ExprDebugInfo | None,
        lhs: ResolvedExpression,
        rhs: ResolvedExpression,
    ) -> ResolvedExpression:
        from langkit.expressions.structs import Cast

        if lhs.type != T.entity:
            lhs = Cast.Expr(None, lhs, T.entity)
        if rhs.type != T.entity:
            rhs = Cast.Expr(None, rhs, T.entity)
        return CallExpr(
            debug_info, "Is_Equiv", "Equivalent", T.Bool, [lhs, rhs]
        )

    def __init__(
        self,
        location: Location,
        lhs: AbstractExpression,
        rhs: AbstractExpression,
    ):
        super().__init__(location)
        self.lhs = lhs
        self.rhs = rhs

    def construct(self) -> ResolvedExpression:
        from langkit.expressions.structs import Cast
        lhs = construct(self.lhs)
        rhs = construct(self.rhs)

        def check_type_compatibility(is_valid: bool) -> None:
            check_source_language(
                is_valid,
                'Incompatible types for equality: {} and {}'.format(
                    lhs.type.dsl_name, rhs.type.dsl_name
                )
            )

        def check_never_equal(can_be_equal: bool) -> None:
            check_source_language(
                can_be_equal,
                '{} and {} values are never equal'.format(
                    lhs.type.dsl_name, rhs.type.dsl_name
                )
            )

        # Don't use CompiledType.matches since in the generated code, we need
        # both operands to be *exactly* the same types, so handle specifically
        # each case.
        if isinstance(lhs.type, ASTNodeType):
            check_type_compatibility(rhs.type.is_ast_node)
            assert isinstance(rhs.type, ASTNodeType)

            # Handle checks between two subclasses without explicit casts. In
            # order to help users to detect dubious checks, forbid operands
            # that can never be equal because they have no subclass in common.
            if lhs.type.matches(rhs.type):
                lhs = Cast.Expr(None, lhs, rhs.type)
            elif rhs.type.matches(lhs.type):
                assert isinstance(lhs.type, ASTNodeType)
                rhs = Cast.Expr(None, rhs, lhs.type)
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
            return self.make_expr_for_entities(self.debug_info, lhs, rhs)

        else:
            check_type_compatibility(lhs.type == rhs.type)

        return self.make_expr(self.debug_info, lhs, rhs)


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

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            operator: str,
            lhs: ResolvedExpression,
            rhs: ResolvedExpression,
        ):
            self.operator = operator
            self.lhs = lhs
            self.rhs = rhs

            template = '{{}} {} {{}}'.format(
                OrderingTest.OPERATOR_IMAGE[self.operator]
            )

            super().__init__(
                debug_info, "Comp_Result", template, T.Bool, [lhs, rhs]
            )

        @property
        def subexprs(self) -> dict:
            return {'op': self.operator, 'lhs': self.lhs, 'rhs': self.rhs}

        def __repr__(self) -> str:
            return '<OrderingTest.Expr {}>'.format(self.operator)

    def __init__(
        self,
        location: Location,
        operator: str,
        lhs: AbstractExpression,
        rhs: AbstractExpression,
    ):
        super().__init__(location)
        assert operator in OrderingTest.OPERATOR_IMAGE
        self.operator = operator
        self.lhs = lhs
        self.rhs = rhs

    def construct(self) -> ResolvedExpression:
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
            p = PropertyDef.get()
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
                self.debug_info,
                'Node_Comp',
                'Compare',
                T.Bool,
                [construct(p.node_var), lhs, rhs, relation],
            )

        # Otherwise, expect strict equality for both operands and use the
        # native comparison operator for code generation.
        check_source_language(
            lhs.type == rhs.type,
            'Comparisons require the same type for both operands'
            ' (got {} and {})'.format(lhs.type.dsl_name, rhs.type.dsl_name)
        )
        return OrderingTest.Expr(self.debug_info, self.operator, lhs, rhs)

    def __repr__(self) -> str:
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

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            cond: ResolvedExpression,
            then: ResolvedExpression,
            else_then: ResolvedExpression,
        ):
            """
            :param cond: A boolean expression.
            :param then: If "cond" is evaluated to true, this part is returned.
            :param else_then: If "cond" is evaluated to false, this part is
                returned.
            """
            self.cond = cond
            self.then = then
            self.else_then = else_then
            self.static_type = then.type

            super().__init__(debug_info, 'If_Result')

        def _render_pre(self) -> str:
            return render('properties/if_ada', expr=self)

        @property
        def subexprs(self) -> dict:
            return {'0-cond': self.cond,
                    '1-then': self.then,
                    '2-else': self.else_then}

        def __repr__(self) -> str:
            return '<If.Expr>'

    def __init__(
        self,
        location: Location,
        cond: AbstractExpression,
        then: AbstractExpression,
        else_then: AbstractExpression,
    ):
        """
        :param cond: A boolean expression.
        :param then: If "cond" is evaluated to true, this part is returned.
        :param else_then: If "cond" is evaluated to false, this part is
            returned.
        """
        super().__init__(location)
        self.cond = cond
        self._then = then
        self.else_then = else_then

    def construct(self) -> ResolvedExpression:
        then, else_then = expr_or_null(self._then, self.else_then,
                                       'If expression', None)
        return If.Expr(
            self.debug_info, construct(self.cond, T.Bool), then, else_then
        )


@dsl_document
class Not(AbstractExpression):
    """
    Return true if `expr` is false and conversely.
    """

    def __init__(self, location: Location, expr: AbstractExpression):
        super().__init__(location)
        self.expr = expr

    def construct(self) -> ResolvedExpression:
        return Not.make_expr(self.debug_info, construct(self.expr, T.Bool))

    @staticmethod
    def make_expr(
        debug_info: ExprDebugInfo | None,
        expr: ResolvedExpression,
    ) -> ResolvedExpression:
        return BasicExpr(debug_info, "Not_Val", "not ({})", T.Bool, [expr])


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

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            expr: ResolvedExpression,
            var_expr: VariableExpr,
            then_expr: ResolvedExpression,
            default_expr: ResolvedExpression,
            then_scope: LocalVars.Scope,
        ):
            self.expr = expr
            self.var_expr = var_expr
            self.then_expr = then_expr
            self.default_expr = default_expr
            self.then_scope = then_scope
            self.static_type = self.then_expr.type

            super().__init__(debug_info, "Result_Var")

        def _render_pre(self) -> str:
            return render('properties/then_ada', then=self)

        @property
        def subexprs(self) -> dict:
            return {'0-prefix': self.expr,
                    '1-then': self.then_expr,
                    '2-default': self.default_expr}

        def _bindings(self) -> list[VariableExpr]:
            return [self.var_expr]

        def __repr__(self) -> str:
            return '<Then.Expr>'

    def __init__(
        self,
        location: Location,
        base: AbstractExpression,
        var_expr: AbstractVariable,
        lambda_arg_infos: list[LambdaArgInfo],
        then_expr: AbstractExpression,
        default_expr: AbstractExpression | None = None,
    ):
        """
        :param base: The expression to use as a source for the ``then``.
        :param lambda_arg_infos: Information for the arguments for the Lkt
            lambda corresponding to this expression.
        :param then_expr: The expression to evaluate if ``base`` is not null.
        :param default_expr: The expression to use as a fallback if ``expr`` is
            null. If omitted, use the result type's null expression.
        """
        super().__init__(location)
        self.base = base
        self.var_expr = var_expr
        self.lambda_arg_infos = lambda_arg_infos
        self.then_expr = then_expr
        self.default_expr = default_expr

        # Whether this ``then`` expression comes from the expansion of the
        # null-cond operator (``?.`` in Lkt sources).
        self.underscore_then = False

    def construct(self) -> ResolvedExpression:
        # Accept as a prefix all types that can have a null value
        base = construct(
            self.base,
            lambda cls: cls.null_allowed,
            'Invalid prefix type for .then: {expr_type}'
        )
        self.var_expr.set_type(base.type)

        # Now that the variable is typed, ensure that its type annotation in
        # the lambda expression (if present) is correct.
        LambdaArgInfo.check_list(self.lambda_arg_infos)

        # Create a then-expr specific scope to restrict the span of the "then"
        # variable in the debugger.
        with PropertyDef.get_scope().new_child() as then_scope:
            assert self.var_expr.local_var
            then_scope.add(self.var_expr.local_var)
            then_expr = construct(self.then_expr)
            var_expr = construct(self.var_expr)
            assert isinstance(var_expr, VariableExpr)
        then_expr = BindingScope(None, then_expr, [var_expr], scope=then_scope)

        # Affect default value to the fallback expression
        then_expr, default_expr = expr_or_null(
            then_expr,
            self.default_expr,
            "Then expression",
            "function's return type",
        )

        var_expr = construct(self.var_expr)
        assert isinstance(var_expr, VariableExpr)

        return Then.Expr(
            self.debug_info,
            base,
            var_expr,
            then_expr,
            default_expr,
            then_scope,
        )
