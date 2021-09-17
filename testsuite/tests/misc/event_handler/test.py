"""
Check that the file reader APIs work as expected.
"""

from langkit.dsl import ASTNode, AnalysisUnit, Symbol
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(
        return_type=AnalysisUnit,
        external=True,
        uses_entity_info=False,
        uses_envs=False,
        public=True
    )
    def get_unit(name=Symbol):
        pass


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    ada_main="main.adb",
    lkt_semantic_checks=True
)

print("Done")
