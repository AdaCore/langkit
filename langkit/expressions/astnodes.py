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

    if node_expr.type.is_entity_type:
        return FieldAccess.Expr(node_expr, get_builtin_field('parent'),
                                [], implicit_deref=True, abstract_expr=self)
    else:
        return FieldAccessExpr(node_expr, 'Parent', T.root_node,
                               do_explicit_incref=False, abstract_expr=self)


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

    if node_expr.type.is_entity_type:
        return FieldAccess.Expr(node_expr, get_builtin_field('parents'),
                                [], implicit_deref=True, abstract_expr=self)
    else:
        return CallExpr('Node_Parents', 'Parents', T.root_node.array,
                        [node_expr], abstract_expr=self)


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

    if node_expr.type.is_entity_type:
        return FieldAccess.Expr(node_expr, get_builtin_field('children'),
                                [], implicit_deref=True, abstract_expr=self)
    else:
        return CallExpr('Node_Children', 'Children', T.root_node.array,
                        [node_expr], abstract_expr=self)
