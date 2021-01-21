"""
Check that invalid uses of the Super construct are properly rejected.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import AbstractKind, Super, langkit_property

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
            return Super("foo")

    class Number(FooNode):
        token_node = True


def test_no_overridden():
    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=T.Int)
        def p():
            return Super() + 1

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
            return Super() + 1

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
            return Super() + 1

    class Number(FooNode):
        token_node = True


for key, value in sorted(locals().items()):
    if key.startswith('test'):
        print('== {} =='.format(key))
        value()
        emit_and_print_errors(lkt_file='foo.lkt')
        print('')

print('Done')
