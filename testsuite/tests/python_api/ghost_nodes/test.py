"""
Test that the "is_ghost" AST node predicate works in the Python API.
"""

from langkit.dsl import ASTNode, Field, T

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Enum(FooNode):
    enum_node = True
    alternatives = ['null', 'example', 'default']


class PlusQualifier(FooNode):
    enum_node = True
    qualifier = True


class Param(FooNode):
    name = Field(type=T.Name)
    mode = Field(type=T.Enum)
    has_plus = Field(type=T.PlusQualifier)


class Name (FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
