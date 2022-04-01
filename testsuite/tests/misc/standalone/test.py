"""
Check that the standalone library mode works as expected.
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    generate_unparser=True,
    standalone=True,
    ada_main="main.adb",
)

print("Done")
