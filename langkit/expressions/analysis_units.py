from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit import names
from langkit.compiled_types import (AnalysisUnitKind, AnalysisUnitType,
                                    BoolType, T)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractVariable, BuiltinCallExpr, FieldAccessExpr, NullCheckExpr,
    ResolvedExpression, auto_attr, construct
)


UnitSpecification = AbstractVariable(
    names.Name('Unit_Specification'),
    type=AnalysisUnitKind
)
UnitBody = AbstractVariable(
    names.Name('Unit_Body'),
    type=AnalysisUnitKind
)


class AnalysisUnitRoot(ResolvedExpression):
    """
    Construct that takes an analysis unit and that returns its root node.

    Note that this automatically generates a check for null analysis units.
    """

    def __init__(self, unit_expr, abstract_expr=None):
        super(AnalysisUnitRoot, self).__init__(abstract_expr=abstract_expr)

        self.static_type = T.root_node
        var_name = None if unit_expr.result_var else 'Unit'
        self.unit_expr = NullCheckExpr(unit_expr, result_var_name=var_name)

    def _render_pre(self):
        return self.unit_expr.render_pre()

    def _render_expr(self):
        return 'Root ({})'.format(self.unit_expr.render_expr())

    @property
    def subexprs(self):
        return [self.unit_expr]


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


def construct_analysis_unit_property(unit_expr, field_name, arguments):
    """
    Construct a resolved expression for the evaluation of a property on an
    analysis unit.

    :param ResolvedExpression unit_expr: Expression that yields the analysis
        unit for which we evaluate a property.
    :param str field_name: Name of the property to evaluate.
    :param list[AbstractExpression] arguments: List of arguments associated to
        this property evaluation.
    """
    if field_name == 'root':
        check_source_language(
            not arguments,
            'This property accepts no argument'
        )
        return AnalysisUnitRoot(unit_expr)

    else:
        check_source_language(
            False,
            'Invalid property for analysis unit: {}'.format(field_name)
        )
