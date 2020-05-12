"""
Test that grammars which are troublesome for the unparsing machinery are
properly detected.
"""

from langkit.dsl import ASTNode, Field

from utils import emit_and_print_errors


def run(name, lkt_file):

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Root(FooNode):
        f = Field()

    class Identifier(FooNode):
        token_node = True

    class Number(FooNode):
        token_node = True

    class Nodes(object):
        pass
    Nodes.Root = Root
    Nodes.Identifier = Identifier
    Nodes.Number = Number

    emit_and_print_errors(lkt_file=lkt_file, generate_unparser=True)
    print('')


run('Pick in Or', 'pick-in-or.lkt')
run('Toplevel Pick', 'toplevel-pick.lkt')
run('Several token kinds for token node (1)', 'several-1.lkt')
run('Several token kinds for token node (2)', 'several-2.lkt')
run('Pick in List', 'pick-in-list.lkt')
run('Ambiguous lists', 'ambiguous-lists.lkt')
print('Done')
