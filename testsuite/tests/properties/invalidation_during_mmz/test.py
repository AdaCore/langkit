"""
Check that memoization of a property does not crash when side effects that
invalidate the cache are triggered during the execution of that property.
"""

from langkit.dsl import ASTNode, AnalysisUnit, T
from langkit.expressions import (If, PropertyError, Self, Var, ignore,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(external=True, uses_entity_info=False, uses_envs=False,
                      return_type=AnalysisUnit)
    def fetch_example_unit():
        pass

    @langkit_property(memoized=True, return_type=T.Int)
    def internal_mmz_prop(i=T.Int):
        return If(
            i == 0,
            PropertyError(T.Int),
            i
        )

    @langkit_property(public=True, memoized=True, return_type=T.Int)
    def mmz_prop(i=T.Int):
        # Update context version by parsing a new unit
        ignore(Var(Self.fetch_example_unit))

        # Trigger a cache clear by calling another property
        # (which will call Reset_Caches).
        return Self.internal_mmz_prop(i)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lkt_semantic_checks=True)
print('Done')
