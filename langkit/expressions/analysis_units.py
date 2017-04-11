from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit import names
from langkit.compiled_types import (AnalysisUnitKind, AnalysisUnitType,
                                    BoolType, T)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractVariable, BuiltinCallExpr, FieldAccessExpr, NullCheckExpr,
    auto_attr, auto_attr_custom, construct
)


UnitSpecification = AbstractVariable(
    names.Name('Unit_Specification'),
    type=AnalysisUnitKind
)
UnitBody = AbstractVariable(
    names.Name('Unit_Body'),
    type=AnalysisUnitKind
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
    check_source_language(
        issubclass(node_expr.type, T.root_node),
        'The "unit" field is available only for AST nodes; instead we have'
        ' here a {}'.format(node_expr.type.name().lower)
    )
    return FieldAccessExpr(node_expr, 'Unit', AnalysisUnitType,
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
    return BuiltinCallExpr(
        'Is_Referenced_From', BoolType,
        [construct(referenced_unit, AnalysisUnitType),
         construct(base_unit, AnalysisUnitType)],
        abstract_expr=self
    )


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
    unit_expr = construct(unit, AnalysisUnitType)
    return BuiltinCallExpr(
        'Root', T.root_node, [NullCheckExpr(unit_expr)], abstract_expr=self
    )
