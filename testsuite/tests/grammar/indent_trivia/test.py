"""
Check that trivia are properly scanned when the lexer tracks indentation.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()
    stmts = Field()


class Identifier(FooNode):
    token_node = True


class Call(FooNode):
    name = Field()
    args = Field()


class Indented(FooNode):
    inner = Field()


class Newline(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              unparse_script=unparse_all_script)
print('Done')
