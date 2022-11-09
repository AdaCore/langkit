"""
Check that the file reader APIs work as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(
        return_type=T.Bool,
        external=True,
        uses_entity_info=False,
        uses_envs=False,
        public=True
    )
    def trigger_unit_requested(name=T.Symbol, found=T.Bool, error=T.Bool):
        pass


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    ada_main="main.adb",
    types_from_lkt=True,
)

print("Done")
