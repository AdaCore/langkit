"""
Check that the Children_With_Trivia function works as expected.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class DeclError(FooNode):
    enum_node = True
    qualifier = True


class Identifier(FooNode):
    token_node = True


class Decl(FooNode):
    id = Field(type=Identifier)
    error = Field(type=DeclError)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
