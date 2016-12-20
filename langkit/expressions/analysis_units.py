from langkit.compiled_types import T
from langkit.diagnostics import check_source_language
from langkit.expressions.base import PropertyDef, ResolvedExpression, render


class AnalysisUnitRoot(ResolvedExpression):
    """
    Construct that takes an analysis unit and that returns its root node.
    """

    def __init__(self, unit_expr):
        super(AnalysisUnitRoot, self).__init__()

        self.static_type = T.root_node
        self.unit_expr = unit_expr
        self.prefix_var = PropertyDef.get().vars.create(
            'Unit', self.unit_expr.type
        )

    def _render_pre(self):
        return '{}\n{}'.format(
            self.unit_expr.render_pre(),
            render(
                'properties/null_safety_check_ada',
                expr=self.unit_expr,
                result_var=self.prefix_var
            )
        )

    def _render_expr(self):
        return 'Root ({})'.format(self.prefix_var.name)


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
