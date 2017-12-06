from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.expressions import DynamicVariable, Property, Self
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import emit_and_print_errors


dynvar = DynamicVariable('dynvar', T.FooNode)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field()

    # The "construct" pass on p1 will require the type of p2 and thus trigger
    # the construction of p2. A bug used to propagate the binding of "dynvar"
    # from the construction of p1 to p2's.
    p1 = Property(dynvar.bind(Self, Self.p2).as_bare_entity, public=True)
    p2 = Property(dynvar)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example(Tok(Token.Example, keep=True)),
)
emit_and_print_errors(grammar)
print('Done')
