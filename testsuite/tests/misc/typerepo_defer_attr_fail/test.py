from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field(type=T.FooNode.does_not_exist)


fg = Grammar('main_rule')
fg.add_rules(
    main_rule=Example(Tok(Token.Example, keep=True)),
)
emit_and_print_errors(fg)
print('Done')
