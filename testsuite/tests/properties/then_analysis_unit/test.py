"""
Test that automatic default values are provided for Then expressions that
return analysis units.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import ASTNode, Field, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    unit = Property(
        Self.cast(T.Name).then(lambda n: n.unit),
        public=True
    )


class Name(FooNode):
    tok = Field()


class Scope(Name.list_type()):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.name, list_cls=Scope),
    name=Name(Tok(Token.Identifier, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
