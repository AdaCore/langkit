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
    Expression that gets the analysis unit that "node_expr" belongs to.

    :param AbstractExpression node: Node for which we want the embedding
        analysis unit.
    :rtype: AbstractExpression
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
        ' here a {}'.format(node_expr.type.name().lower)
    )

    # From the point of view of properties, analysis units are not ref-counted,
    # so we must not inc-ref here.
    return FieldAccessExpr(node_expr, 'Unit', analysis_unit_type,
                           do_explicit_incref=False,
                           abstract_expr=self)


@auto_attr
def is_referenced_from(self, referenced_unit, base_unit):
    """
    Expression to compute whether an analysis unit is referenced from another
    unit.

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
    Construct a resolved expression for the evaluation of a property on an
    analysis unit.

    :param ResolvedExpression unit_expr: Expression that yields the analysis
        unit for which we evaluate a property.
    :param str field_name: Name of the property to evaluate.
    :param list[AbstractExpression] arguments: List of arguments associated to
        this property evaluation.
    """
    unit_expr = construct(unit, analysis_unit_type)
    return CallExpr('Unit_Root', 'Root', T.root_node,
                    [NullCheckExpr(unit_expr)], abstract_expr=self)
