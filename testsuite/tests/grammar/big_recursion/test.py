"""
Regression test: parsers code generation used to trigger the codegen for a
single parsing function in chain: generating code for one function would
trigger the generation of another function it needed to use, etc. This could
create stack overflows in Langkit.
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)

print('Done')
