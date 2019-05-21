"""
Test that parsers that produce a synthetic node are rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, has_abstract_list, synthetic
from langkit.parsers import Grammar, List, Null, Skip

from utils import emit_and_print_errors


def run(label, parser_constructor):
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

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(main_rule=Example(
        'example',
        parser_constructor(SynthExample, ListSynthExample)))

    emit_and_print_errors(foo_grammar)
    print('')


run('List', lambda _, cls: List('example', list_cls=cls))
run('Null', lambda cls, _: Null(cls))
run('Skip', lambda cls, _: Skip(cls))
run('Transform', lambda cls, _: cls())
print('Done')
