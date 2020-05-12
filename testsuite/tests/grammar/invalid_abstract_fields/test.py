"""
Test that invalid uses of abstract fields are duly diagnosed and rejected.
"""

from contextlib import contextmanager

import langkit
from langkit.dsl import ASTNode, AbstractField, Field, NullField, T, abstract

from utils import emit_and_print_errors


@contextmanager
def test(label, lkt_file):
    print('== {} =='.format(label))
    yield
    emit_and_print_errors(lkt_file=lkt_file)
    langkit.reset()
    print()


with test('Not overriden', 'not-overriden.lkt'):
    class FooNode(ASTNode):
        pass

    class ExampleHolder(FooNode):
        f1 = AbstractField(T.FooNode)
        f2 = Field(type=T.FooNode)

    class Example(FooNode):
        token_node = True

    del FooNode, ExampleHolder, Example


with test('Partly overriden', 'partly-overriden.lkt'):
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

    del (FooNode, BaseExampleHolder, SomeExampleHolder, OtherExampleHolder,
         Example)


with test('Abstract overriding abstract', 'abstract-overriding-abstract.lkt'):
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

    del FooNode, BaseExampleHolder, ExampleHolder, Example


with test('Abstract overriding concrete', 'abstract-overriding-concrete.lkt'):
    class FooNode(ASTNode):
        pass

    @abstract
    class BaseExampleHolder(FooNode):
        f = Field(type=T.FooNode)

    class ExampleHolder(BaseExampleHolder):
        f = AbstractField(T.FooNode)

    class Example(FooNode):
        token_node = True

    del FooNode, BaseExampleHolder, ExampleHolder, Example


with test('Inconsistent overriding type', 'inconsistent-overriding-type.lkt'):
    class FooNode(ASTNode):
        pass

    @abstract
    class BaseExampleHolder(FooNode):
        f = AbstractField(type=T.Example)

    class ExampleHolder(BaseExampleHolder):
        f = Field(type=T.FooNode)

    class Example(FooNode):
        token_node = True

    del FooNode, BaseExampleHolder, ExampleHolder, Example


with test('Free-standing null field', 'free-standing-null-field.lkt'):
    class FooNode(ASTNode):
        pass

    class ExampleHolder(FooNode):
        f = NullField()

    class Example(FooNode):
        token_node = True

    del FooNode, ExampleHolder, Example

print('Done')
