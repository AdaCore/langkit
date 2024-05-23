"""
Check that, when traces are enabled, performing an env lookup on the empty env
does not crash.
"""

from langkit.dsl import ASTNode
from langkit.expressions import No, T, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True, return_type=T.FooNode.entity.array)
    def prop():
        return No(T.LexicalEnv).get("foo")


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
