"""
Check that compilation succeeds in the presence of an unused user field.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, UserField

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True
    unused_fld = UserField(type=T.LexicalEnv, public=False)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
