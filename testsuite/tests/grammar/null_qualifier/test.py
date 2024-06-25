"""
Check that the Null parser creates the "absent" alternative for a qualifier
node.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field, abstract

from utils import build_and_run, unparse_script


class FooNode(ASTNode):
    pass


class NullQual(FooNode):
    enum_node = True
    qualifier = True


class Identifier(FooNode):
    token_node = True


@abstract
class Decl(FooNode):
    is_null = Field(type=T.NullQual)
    name = Field(type=T.Identifier)


class VarDecl(Decl):
    pass


class FunDecl(Decl):
    pass


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    unparse_script=unparse_script,
    types_from_lkt=True,
)
print("Done")
