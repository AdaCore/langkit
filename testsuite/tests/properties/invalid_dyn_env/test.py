"""
Check that legality checks for DynamicLexicalEnv expressions work as expected.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import (DynamicLexicalEnv, No, ignore,
                                 langkit_property, lazy_field)

from utils import emit_and_print_errors


def test_not_in_lazy_field():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @langkit_property(return_type=T.LexicalEnv)
        def args_env():
            result = DynamicLexicalEnv(T.Example.assocs_getter)
            return result

        @langkit_property(return_type=T.inner_env_assoc.array)
        def assocs_getter():
            return No(T.inner_env_assoc.array)


def test_assocs_getter_bad_args():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @lazy_field(return_type=T.LexicalEnv)
        def args_env():
            result = DynamicLexicalEnv(T.Example.assocs_getter)
            return result

        @langkit_property(return_type=T.inner_env_assoc.array)
        def assocs_getter(a=T.Bool):
            ignore(a)
            return No(T.inner_env_assoc.array)


def test_assocs_getter_bad_rtype():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @lazy_field(return_type=T.LexicalEnv)
        def args_env():
            result = DynamicLexicalEnv(T.Example.assocs_getter)
            return result

        @langkit_property(return_type=T.Bool)
        def assocs_getter():
            return True


def test_assoc_resolver_bad_args():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @lazy_field(return_type=T.LexicalEnv)
        def args_env():
            return DynamicLexicalEnv(
                T.Example.assocs_getter,
                T.Example.assoc_resolver,
            )

        @langkit_property(return_type=T.inner_env_assoc.array)
        def assocs_getter():
            return No(T.inner_env_assoc.array)

        @langkit_property(return_type=T.FooNode.entity)
        def assoc_resolver(a=T.Bool):
            ignore(a)
            return No(T.FooNode.entity)


def test_assoc_resolver_bad_rtype():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @lazy_field(return_type=T.LexicalEnv)
        def args_env():
            return DynamicLexicalEnv(
                T.Example.assocs_getter,
                T.Example.assoc_resolver,
            )

        @langkit_property(return_type=T.inner_env_assoc.array)
        def assocs_getter():
            return No(T.inner_env_assoc.array)

        @langkit_property(return_type=T.Bool)
        def assoc_resolver():
            return True


for k, v in sorted(locals().items()):
    if k.startswith('test_'):
        print('== {} =='.format(k))
        v()
        emit_and_print_errors(lkt_file='foo.lkt')
        print('')

print('Done')
