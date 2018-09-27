"""
Test that invalid uses of abstract fields are duly diagnosed and rejected.
"""

from __future__ import absolute_import, division, print_function

from contextlib import contextmanager

import langkit
from langkit.dsl import ASTNode, AbstractField, Field, T, abstract
from langkit.parsers import Grammar

from utils import emit_and_print_errors


@contextmanager
def test(label):
    print('== {} =='.format(label))
    yield
    langkit.reset()
    print()


with test('Not overriden'):
    class FooNode(ASTNode):
        pass

    class ExampleHolder(FooNode):
        f1 = AbstractField(T.FooNode)
        f2 = Field(type=T.FooNode)

    class Example(FooNode):
        token_node = True

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(main_rule=ExampleHolder(Example('example')))
    emit_and_print_errors(foo_grammar)


with test('Partly overriden'):
    class FooNode(ASTNode):
        pass

    @abstract
    class BaseExampleHolder(FooNode):
        f = AbstractField(T.FooNode)

    class SomeExampleHolder(BaseExampleHolder):
        f = Field()

    class OtherExampleHolder(BaseExampleHolder):
        pass

    class Example(FooNode):
        token_node = True

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(main_rule=SomeExampleHolder(Example('example')))
    emit_and_print_errors(foo_grammar)


with test('Abstract overriding abstract'):
    class FooNode(ASTNode):
        pass

    @abstract
    class BaseExampleHolder(FooNode):
        f1 = AbstractField(T.FooNode)

    class ExampleHolder(BaseExampleHolder):
        f1 = AbstractField(T.FooNode)
        f2 = Field(type=T.FooNode)

    class Example(FooNode):
        token_node = True

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(main_rule=ExampleHolder(Example('example')))
    emit_and_print_errors(foo_grammar)


with test('Abstract overriding concrete'):
    class FooNode(ASTNode):
        pass

    @abstract
    class BaseExampleHolder(FooNode):
        f = Field(type=T.FooNode)

    class ExampleHolder(BaseExampleHolder):
        f = AbstractField(T.FooNode)

    class Example(FooNode):
        token_node = True

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(main_rule=ExampleHolder(Example('example')))
    emit_and_print_errors(foo_grammar)


with test('Inconsistent overriding type'):
    class FooNode(ASTNode):
        pass

    @abstract
    class BaseExampleHolder(FooNode):
        f = AbstractField(type=T.Example)

    class ExampleHolder(BaseExampleHolder):
        f = Field(type=T.FooNode)

    class Example(FooNode):
        token_node = True

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(main_rule=ExampleHolder(Example('example')))
    emit_and_print_errors(foo_grammar)

print('Done')
