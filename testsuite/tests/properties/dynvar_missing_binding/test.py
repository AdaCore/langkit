from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T, root_grammar_class
from langkit.expressions import DynamicVariable, Property, Self
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
dynvar = DynamicVariable('dynvar', T.FooNode)


@root_grammar_class
class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field()

    # The "construct" pass on p1 will require the type of p2 and thus trigger
    # the construction of p2. A bug used to propagate the binding of "dynvar"
    # from the construction of p1 to p2's.
    p1 = Property(dynvar.bind(Self, Self.p2), public=True)
    p2 = Property(dynvar)


def grammar_fn():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Example(Tok(Token.Example, keep=True)),
    )
    return foo_grammar

emit_and_print_errors(grammar_fn)
print('Done')
