"""
Check that invalid uses of the ".super()" construct are properly rejected.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import AbstractKind, Entity, Self, langkit_property

from utils import emit_and_print_errors


def test_bad_args():
    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=T.Int)
        def p():
            return 1

    class Example(FooNode):
        token_node = True

        @langkit_property()
        def p():
            return Self.super("foo")

    class Number(FooNode):
        token_node = True


def test_no_overridden():
    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=T.Int)
        def p():
            return Self.super() + 1

    class Example(FooNode):
        token_node = True

    class Number(FooNode):
        token_node = True


def test_overridden_abstract():
    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=T.Int,
                          kind=AbstractKind.abstract)
        def p():
            pass

    class Example(FooNode):
        token_node = True

        @langkit_property(public=True, return_type=T.Int)
        def p():
            return Self.super() + 1

    class Number(FooNode):
        token_node = True

        @langkit_property()
        def p():
            return 0


def test_overridden_abstract_runtime_check():
    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=T.Int,
                          kind=AbstractKind.abstract_runtime_check)
        def p():
            pass

    class Example(FooNode):
        token_node = True

        @langkit_property()
        def p():
            return Self.super() + 1

    class Number(FooNode):
        token_node = True


def test_invalid_prefix():
    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=T.Int)
        def p():
            return 1

    class Example(FooNode):
        token_node = True

        @langkit_property()
        def p():
            return Self.parent.super() + 1

    class Number(FooNode):
        token_node = True


def test_self_prefix():
    class FooNode(ASTNode):
        @langkit_property()
        def p1():
            return 1

        @langkit_property(public=True, return_type=T.Int)
        def p2():
            return Entity.p1

    class Example(FooNode):
        token_node = True

        @langkit_property()
        def p2():
            return Self.super()

    class Number(FooNode):
        token_node = True


for key, value in sorted(locals().items()):
    if key.startswith('test'):
        print('== {} =='.format(key))
        value()
        emit_and_print_errors(lkt_file='foo.lkt')
        print('')

print('Done')
