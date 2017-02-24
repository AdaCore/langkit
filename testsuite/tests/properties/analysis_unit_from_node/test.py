"""
Test getting the analysis unit of a node in the properties DSL.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import (
    ASTNode, Field, abstract, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import (
    Property, Self
)
from langkit.parsers import Grammar, Or, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    root_node = Property(Self.unit.root, public=True)


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    tok = Field()


class Plus(Expression):
    left = Field()
    right = Field()


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(
        Row(foo_grammar.atom, '+', foo_grammar.main_rule) ^ Plus,
        foo_grammar.atom
    ),
    atom=Row(Tok(Token.Number, keep=True)) ^ Literal,
)
build_and_run(foo_grammar, 'main.py')
print('Done')
