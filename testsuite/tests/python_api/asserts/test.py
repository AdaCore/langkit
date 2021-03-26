"""
Regression test: check that getting the list of diagnostics and the range of
text between two tokens works when Python assertions are disabled. This used
not to work because "operational" code was in assert statements in the Python
bindings.
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
    python_args=["-O"],
)
print('Done')
