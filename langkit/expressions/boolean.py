from __future__ import annotations

from langkit.compiled_types import T
from langkit.expressions.base import (
    BasicExpr,
    CallExpr,
    ComputingExpr,
    ExprDebugInfo,
    LocalVars,
    PropertyDef,
    ResolvedExpression,
    VariableExpr,
    render,
)


class Eq:
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


class OrderingTest:
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

    @staticmethod
    def make_compare_nodes(
        debug_info: ExprDebugInfo | None,
        current_property: PropertyDef,
        operator: str,
        left: ResolvedExpression,
        right: ResolvedExpression,
    ) -> ResolvedExpression:
        """
        Return an expression that performs an ordering test on two nodes.

        :param debug_info: Debug information for the expression to return.
        :param current_property: Property that owns this expression.
        :param operator: Comparison operator.
        :param left: Comparison left operand.
        :param right: Comparison right operand.
        """
        relation = {
            OrderingTest.LT: "Less_Than",
            OrderingTest.LE: "Less_Or_Equal",
            OrderingTest.GT: "Greater_Than",
            OrderingTest.GE: "Greater_Or_Equal"
        }[operator]
        return CallExpr(
            debug_info,
            "Node_Comp",
            "Compare",
            T.Bool,
            [current_property.node_var.ref_expr, left, right, relation],
        )


class If:
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


class Not:
    """
    Return true if `expr` is false and conversely.
    """

    @staticmethod
    def make_expr(
        debug_info: ExprDebugInfo | None,
        expr: ResolvedExpression,
    ) -> ResolvedExpression:
        return BasicExpr(debug_info, "Not_Val", "not ({})", T.Bool, [expr])


class Then:
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
