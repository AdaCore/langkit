from __future__ import absolute_import, division, print_function

from langkit.compiled_types import T
from langkit.expressions.base import (
    CallExpr, PropertyDef, auto_attr, construct
)


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
    return IsReferencedFrom(referenced_unit, base_unit, abstract_expr=self)


class IsReferencedFrom(CallExpr):
    def __init__(self, referenced_unit, base_unit, abstract_expr=None):
        super(IsReferencedFrom, self).__init__(
            'Is_Referenced', 'Is_Referenced_From', T.Bool,
            [construct(referenced_unit, T.AnalysisUnit),
             construct(base_unit, T.AnalysisUnit)],
            abstract_expr=self
        )

        PropertyDef.get().set_uses_envs()
