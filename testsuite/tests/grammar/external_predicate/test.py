"""
Check that Langkit generates valid code when a Predicate parser references an
external property.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True

    @langkit_property(
        external=True,
        uses_envs=False,
        uses_entity_info=False,
        return_type=T.Bool,
    )
    def pred():
        pass


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
