"""
Check that property memoization works as expected whhen memoization table
invalidation happens in the middle of their execution. In particular, check
that the result of memoized properties are consistent, and would be the same as
if properties were not memoized.
"""

from langkit.dsl import ASTNode, AnalysisUnit, T
from langkit.expressions import Self, Var, ignore, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    # External properties

    @langkit_property(external=True, uses_entity_info=False, uses_envs=False,
                      return_type=AnalysisUnit)
    def ext_fetch_example_unit():
        pass

    @langkit_property(external=True, uses_entity_info=False, uses_envs=False,
                      return_type=T.Int)
    def ext_unit_count():
        pass

    # External property wrappers (for tracing)

    @langkit_property(memoized=True, activate_tracing=True)
    def fetch_example_unit():
        return Self.ext_fetch_example_unit

    @langkit_property(memoized=True, activate_tracing=True)
    def unit_count():
        return Self.ext_unit_count

    # Test entry point

    @langkit_property(public=True, memoized=True, activate_tracing=True)
    def mmz_prop():
        # Both calls to unit_count are memoized, but when called for the first
        # time, the first one's result will be different from the second one.
        before = Var([Self.unit_count])

        # Update context version by parsing a new unit
        ignore(Var(Self.fetch_example_unit))

        after = Var([Self.unit_count])

        return before.concat(after)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb',
              show_property_logging=True, lkt_semantic_checks=True)
print('Done')
