from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.parsers import Grammar

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field(type=T.FooNode.does_not_exist)


fg = Grammar('main_rule')
fg.add_rules(main_rule=Example(Token.Example))
emit_and_print_errors(fg)
print('Done')
