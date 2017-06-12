"""
Test the handling of negative indexes in the Python binding of AST nodes child
getters.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import ASTNode, Field, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class Name(FooNode):
    tok = Field(type=T.Token)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.name),
    name=Name(Tok(Token.Identifier, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
