"""
Test the newline and related parsers.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    token_node = True


class NewLineNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()


class IndentNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()


class CompositeNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()
    lit_3 = Field()
    lit_4 = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              unparse_script=unparse_all_script)
print('Done')
