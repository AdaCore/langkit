from __future__ import absolute_import, division, print_function

from langkit.compiled_types import T
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (CallExpr, FieldAccessExpr, auto_attr,
                                      construct)
from langkit.expressions.structs import FieldAccess


def get_builtin_field(name):
    """
    Fetch a builtin field/property in the root AST node.

    :param str name: Name of the builtin to fetch.
    :rtype: langkit.compiled_types.AbstractNodeData
    """
    return T.root_node.get_abstract_node_data_dict()[name]


def build_field_access(self, node_expr, builtin_field_name,
                       bare_node_expr_constructor):
    """
    Helper for abstract expressions below. Return a resolved expression to
    evaluate either `node_expr`'s builtin property `field_name` (if `node_expr`
    is an entity) or the builtin field `field_name` (if `node_expr` is a bare
    node).

    We don't use the builtin property in the bare node case as the expression
    must return the same type as its input, while the property always returns
    an entity.

    :param langkit.expressions.ResolvedExpression node_expr: Expression for the
        input node/entity.
    :param str builtin_field_name: Name of the builtin field to access.
    :param bare_node_expr_constructor: Callback used to build the expression
        that computes the field access in the case we have a bare node input.
    :rtype: langkit.expressions.ResolvedExpression
    """
    if node_expr.type.is_entity_type:
        return FieldAccess.Expr(
            node_expr, get_builtin_field(builtin_field_name), [],
            implicit_deref=True, abstract_expr=self)
    else:
        return bare_node_expr_constructor(node_expr, abstract_expr=self)


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
        self, node_expr, 'parent',
        lambda node_expr, abstract_expr: FieldAccessExpr(
            node_expr, 'Parent', T.root_node,
            do_explicit_incref=False, abstract_expr=self))


@auto_attr
def parents(self, node):
    """
    Return an array that contains the lexical parents (this node included).
    Nearer parents are first in the list.

    This works on both bare nodes and entities.

    .. todo::

        Implement rebindings shedding.
    """
    node_expr = construct(node)
    check_source_language(
        node_expr.type.is_ast_node or node_expr.type.is_entity_type,
        'Invalid prefix for "parents": got {} but AST node or entity'
        ' expected'.format(node_expr.type.dsl_name)
    )

    return build_field_access(
        self, node_expr, 'parents',
        lambda node_expr, abstract_expr: CallExpr(
            'Node_Parents', 'Parents', T.root_node.array, [node_expr],
            abstract_expr=self))


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
        self, node_expr, 'children',
        lambda node_expr, abstract_expr: CallExpr(
            'Node_Children', 'Children', T.root_node.array, [node_expr],
            abstract_expr=self))
