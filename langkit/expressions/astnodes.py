from __future__ import annotations

from typing import Callable, Sequence, cast

from langkit.compiled_types import ASTNodeType, AbstractNodeData, BaseField, T
from langkit.expressions.base import (
    CallExpr,
    ExprDebugInfo,
    NullCheckExpr,
    ResolvedExpression,
)
from langkit.expressions.structs import FieldAccess


def get_builtin_field(name: str) -> AbstractNodeData:
    """
    Fetch a builtin field/property in the root AST node.

    :param name: Name of the builtin to fetch.
    """
    return T.root_node.get_abstract_node_data_dict()[name]


def build_field_access(
    debug_info: ExprDebugInfo | None,
    node_expr: ResolvedExpression,
    builtin_field_name: str,
    args: Sequence[ResolvedExpression | None],
    bare_node_expr_constructor: Callable[[], ResolvedExpression],
) -> ResolvedExpression:
    """
    Helper for abstract expressions below. Return a resolved expression to
    evaluate either `node_expr`'s builtin property `field_name` (if `node_expr`
    is an entity) or the builtin field `field_name` (if `node_expr` is a bare
    node).

    We don't use the builtin property in the bare node case as the expression
    must return the same type as its input, while the property always returns
    an entity.

    :param prefix: Expression for the input node/entity.
    :param builtin_field_name: Name of the builtin field to access.
    :param args: Arguments for the field access.
    :param bare_node_expr_constructor: Callback used to build the expression
        that computes the field access in the case we have a bare node input.
    """
    if node_expr.type.is_entity_type:
        return FieldAccess.Expr(
            debug_info,
            node_expr,
            get_builtin_field(builtin_field_name),
            args,
            implicit_deref=True,
        )
    else:
        return bare_node_expr_constructor()


def parents_access_constructor(
    debug_info: ExprDebugInfo | None,
    prefix: ResolvedExpression,
    node_data: AbstractNodeData,
    args: list[ResolvedExpression | None],
) -> ResolvedExpression:
    """
    Return an access to the "fields" parents, whether called on a node or an
    entity.

    .. todo::

        Implement rebindings shedding.
    """
    # We expect exactly one argument: with_self. If not provided, use the
    # default value.
    assert len(args) == 1
    with_self: ResolvedExpression
    if args[0] is None:
        with_self_arg = node_data.natural_arguments[0]
        assert with_self_arg.default_value is not None
        with_self = with_self_arg.default_value
    else:
        with_self = args[0]

    cons_args = [with_self]

    return build_field_access(
        debug_info,
        prefix,
        "parents",
        cons_args,
        lambda: CallExpr(
            debug_info,
            "Node_Parents",
            "Parents",
            T.root_node.array,
            [cast(ResolvedExpression, NullCheckExpr(prefix))] + cons_args,
        ),
    )


class CreateCopyNodeBuilder:
    """
    Expression to create a non-synthetizing node builder.
    """

    @staticmethod
    def common_construct(
        debug_info: ExprDebugInfo | None,
        value: ResolvedExpression,
    ) -> ResolvedExpression:
        node_type = value.type
        assert isinstance(node_type, ASTNodeType)

        return CallExpr(
            debug_info,
            "Builder",
            "Create_Copy_Node_Builder",
            node_type.builder_type,
            [value],
        )


def make_synth_node_builder(
    debug_info: ExprDebugInfo | None,
    node_type: ASTNodeType,
    field_builders: dict[BaseField, ResolvedExpression],
) -> ResolvedExpression:
    """
    Create an expression to create a synthetizing builder for ``node_type``.
    ``field_builders`` provide node builders for each field in this node type.
    """

    # Enable code generation for synthetizing node builds for this node type
    builder_type = node_type.builder_type
    builder_type.synth_node_builder_needed = True

    # Make sure the required compiled types for the synthetizing node builder
    # constructor are known before code generation starts.
    _ = builder_type.synth_constructor_args

    field_values_list = [
        expr for _, expr in sorted(field_builders.items())
    ]

    return CallExpr(
        debug_info,
        "Builder",
        builder_type.synth_constructor,
        builder_type,
        field_values_list,
    )
