"""
Test that automatic default values are provided for Then expressions that
return analysis units.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    node_unit = Property(
        Self.cast(T.Name).then(lambda n: n.unit),
        public=True
    )


class Name(FooNode):
    token_node = True


class Scope(Name.list):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.name, list_cls=Scope),
    name=Name(Token.Identifier),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
