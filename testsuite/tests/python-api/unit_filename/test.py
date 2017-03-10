"""
Test that LogiVarType bindings in the Python API are properly working.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import (
    ASTNode, Field, Token as TokenType, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field(type=TokenType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Example, keep=True)),
)

build_and_run(foo_grammar, 'main.py')

print('Done')
