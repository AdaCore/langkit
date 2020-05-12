"""
Test that parsers that produce a synthetic node are rejected.
"""

from langkit.dsl import ASTNode, Field, has_abstract_list, synthetic

from utils import emit_and_print_errors


def run(label, lkt_file):
    print('== {} =='.format(label))

    @has_abstract_list
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        f = Field(FooNode)

    @synthetic
    class SynthExample(FooNode):
        pass

    @synthetic
    class ListSynthExample(FooNode.list):
        pass

    emit_and_print_errors(lkt_file=lkt_file)
    print('')


run('List', 'list.lkt')
run('Null', 'null.lkt')
run('Skip', 'skip.lkt')
run('Transform', 'transform.lkt')
print('Done')
