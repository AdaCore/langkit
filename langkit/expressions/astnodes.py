from __future__ import annotations

from typing import Callable, Sequence, cast

from langkit.compiled_types import ASTNodeType, AbstractNodeData, T
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions.base import (
    AbstractExpression,
    CallExpr,
    ExprDebugInfo,
    FieldAccessExpr,
    NullCheckExpr,
    ResolvedExpression,
    abstract_expression_from_construct,
    construct,
)
from langkit.expressions.structs import FieldAccess, New


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


@abstract_expression_from_construct
def parent(
    self: AbstractExpression,
    node: AbstractExpression,
) -> ResolvedExpression:
    """
    Return `node`'s parent in the parse tree.

    This works on both bare nodes and entities.

    .. todo::

        Implement rebindings shedding.
    """
    node_expr = construct(node)
    check_source_language(
        node_expr.type.is_ast_node or node_expr.type.is_entity_type,
        'Invalid prefix for "parent": got {} but AST node or entity'
        ' expected'.format(node_expr.type.dsl_name)
    )

    return build_field_access(
        self.debug_info,
        node_expr,
        "parent",
        [],
        lambda: FieldAccessExpr(
            self.debug_info,
            node_expr,
            "Parent",
            T.root_node,
            do_explicit_incref=False,
        ),
    )


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
    if args[0] is None:
        with_self_arg = node_data.natural_arguments[0]
        with_self_default = with_self_arg.abstract_default_value
        assert with_self_default is not None
        with_self = construct(with_self_default)
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


@abstract_expression_from_construct
def children(
    self: AbstractExpression,
    node: AbstractExpression,
) -> ResolvedExpression:
    """
    Return `node`'s children in the parse tree as an array of root nodes.

    This works on both bare nodes and entities.
    """
    node_expr = construct(node)
    check_source_language(
        node_expr.type.is_ast_node or node_expr.type.is_entity_type,
        'Invalid prefix for "children": got {} but AST node or entity'
        ' expected'.format(node_expr.type.dsl_name)
    )

    return build_field_access(
        self.debug_info,
        node_expr,
        "children",
        [],
        lambda: CallExpr(
            self.debug_info,
            "Node_Children",
            "Children",
            T.root_node.array,
            [NullCheckExpr(node_expr)],
        ),
    )


class CreateCopyNodeBuilder(AbstractExpression):
    """
    Expression to create a non-synthetizing node builder.
    """
    def __init__(self, location: Location, value: AbstractExpression):
        super().__init__(location)
        self.value = value

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

    def construct(self) -> ResolvedExpression:
        value = construct(self.value)
        if not isinstance(value.type, ASTNodeType):
            error(f"node expected, got {value.type.dsl_name}")
        return self.common_construct(self.debug_info, value=value)


class CreateSynthNodeBuilder(AbstractExpression):
    """
    Expression to create a synthetizing node builder.
    """
    def __init__(
        self,
        location: Location,
        node_type: ASTNodeType,
        **field_builders: AbstractExpression,
    ):
        super().__init__(location)
        self.node_type = node_type
        self.field_builders = field_builders

    def construct(self) -> ResolvedExpression:
        if (
            not isinstance(self.node_type, ASTNodeType)
            or not self.node_type.synthetic
        ):
            error("node builders can yield synthetic nodes only")

        # Enable code generation for synthetizing node builds for this node
        # type.
        builder_type = self.node_type.builder_type
        builder_type.synth_node_builder_needed = True

        # Make sure the required compiled types for the synthetizing node
        # builder constructor are known before code generation starts.
        _ = builder_type.synth_constructor_args

        field_values_map = New.construct_fields(
            self.node_type, self.field_builders, for_node_builder=True
        )
        field_values_list = [
            expr for _, expr in sorted(field_values_map.items())
        ]

        return CallExpr(
            self.debug_info,
            "Builder",
            builder_type.synth_constructor,
            builder_type,
            field_values_list,
        )
