from __future__ import absolute_import, division, print_function

from langkit import names
from langkit.compiled_types import (T, analysis_unit_kind, analysis_unit_type,
                                    bool_type)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractVariable, CallExpr, FieldAccessExpr, NullCheckExpr, auto_attr,
    auto_attr_custom, construct
)


UnitSpecification = AbstractVariable(
    names.Name('Unit_Specification'),
    type=analysis_unit_kind
)
UnitBody = AbstractVariable(
    names.Name('Unit_Body'),
    type=analysis_unit_kind
)


@auto_attr
def unit(self, node):
    """
    Return the analysis unit that owns `node`.

    :param AbstractExpression node: Node for which we want the embedding
        analysis unit.
    :rtype: ResolvedExpression
    """
    node_expr = construct(node)

    # Automatically extract AST nodes from entities
    if node_expr.type.is_entity_type:
        node_expr = FieldAccessExpr(node_expr, 'El', node_expr.type.el_type,
                                    do_explicit_incref=True)

    # Make sure that in the end, the prefix is an AST node
    check_source_language(
        node_expr.type.is_ast_node,
        'The "unit" field is available only for AST nodes; instead we have'
        ' here a {}'.format(node_expr.type.name.lower)
    )

    # From the point of view of properties, analysis units are not ref-counted,
    # so we must not inc-ref here.
    return FieldAccessExpr(node_expr, 'Unit', analysis_unit_type,
                           do_explicit_incref=False,
                           abstract_expr=self)


@auto_attr
def is_referenced_from(self, referenced_unit, base_unit):
    """
    Return whether the `referenced_unit` analysis unit is referenced from
    `base_unit`.

    :param AbstractExpression referenced_unit: The unit that may be referenced
        from base_unit.
    :param AbstractExpression base_unit: The unit from which we want to check
        the reference.

    :rtype: ResolvedExpression
    """
    return CallExpr('Is_Referenced', 'Is_Referenced_From', bool_type,
                    [construct(referenced_unit, analysis_unit_type),
                     construct(base_unit, analysis_unit_type)],
                    abstract_expr=self)


@auto_attr_custom("root")
def analysis_unit_root(self, unit):
    """
    Return `unit`'s root AST node.

    :param ResolvedExpression unit: Expression that yields the analysis
        unit for which we want to extract the root AST node.
    """
    unit_expr = NullCheckExpr(construct(unit, analysis_unit_type))
    return FieldAccessExpr(unit_expr, 'AST_Root', T.root_node,
                           do_explicit_incref=False, abstract_expr=self)
