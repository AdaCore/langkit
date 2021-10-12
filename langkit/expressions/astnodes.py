from typing import Callable, List, Optional, Sequence, cast

from langkit.compiled_types import AbstractNodeData, T
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, CallExpr, FieldAccessExpr, NullCheckExpr,
    ResolvedExpression, auto_attr, construct
)
from langkit.expressions.structs import FieldAccess


def get_builtin_field(name: str) -> AbstractNodeData:
    """
    Fetch a builtin field/property in the root AST node.

    :param name: Name of the builtin to fetch.
    """
    return T.root_node.get_abstract_node_data_dict()[name]


def build_field_access(
    node_expr: ResolvedExpression,
    builtin_field_name: str,
    args: Sequence[Optional[ResolvedExpression]],
    bare_node_expr_constructor: Callable[[], ResolvedExpression],
    abstract_expr: Optional[AbstractExpression],
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
    :param abstract_expr: Abstract expression corresponding to this field
        access.
    """
    if node_expr.type.is_entity_type:
        return FieldAccess.Expr(
            node_expr, get_builtin_field(builtin_field_name), args,
            implicit_deref=True, abstract_expr=abstract_expr
        )
    else:
        return bare_node_expr_constructor()


@auto_attr
def parent(self, node):
    """
    Return `node`'s parent in the AST.

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
        node_expr, 'parent', [],
        lambda: FieldAccessExpr(
            node_expr, 'Parent', T.root_node,
            do_explicit_incref=False, abstract_expr=self
        ),
        abstract_expr=self,
    )


def parents_access_constructor(
    prefix: ResolvedExpression,
    node_data: AbstractNodeData,
    args: List[Optional[ResolvedExpression]],
    abstract_expr: Optional[AbstractExpression] = None
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
    with_self: ResolvedExpression = (
        args[0]
        or construct(node_data.natural_arguments[0].abstract_default_value)
    )

    cons_args = [with_self]

    return build_field_access(
        prefix, 'parents', cons_args,
        lambda: CallExpr(
            'Node_Parents', 'Parents', T.root_node.array,
            [cast(ResolvedExpression, NullCheckExpr(prefix))] + cons_args,
            abstract_expr=abstract_expr,
        ),
        abstract_expr=abstract_expr,
    )


@auto_attr
def children(self, node):
    """
    Return `node`'s children in the AST.

    This works on both bare nodes and entities.
    """
    node_expr = construct(node)
    check_source_language(
        node_expr.type.is_ast_node or node_expr.type.is_entity_type,
        'Invalid prefix for "children": got {} but AST node or entity'
        ' expected'.format(node_expr.type.dsl_name)
    )

    return build_field_access(
        node_expr, 'children', [],
        lambda: CallExpr(
            'Node_Children', 'Children', T.root_node.array, [node_expr],
            abstract_expr=self
        ),
        abstract_expr=self,
    )
