from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import New, Property
from langkit.parsers import Grammar, List, Pick

from lexer_example import Token
from utils import emit_and_print_errors


def run(name, prop_fn, prop_memoized):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Literal(FooNode):
        token_node = True

    class EmptyNode(FooNode):
        pass

    class LiteralList(Literal.list):
        prop = Property(prop_fn(), memoized=prop_memoized)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=grammar.list_rule,
        list_rule=Pick(
            '(', List(grammar.list_item, sep=',', cls=LiteralList), ')'
        ),
        list_item=Literal(Token.Number),
    )
    emit_and_print_errors(grammar)
    print('')


run("Not memoized", lambda: New(T.EmptyNode), False)
run("List synthetization", lambda: New(T.LiteralList), True)
print('Done')
