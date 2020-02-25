from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import DynamicVariable, ignore, langkit_property
from langkit.expressions.envs import EmptyEnv

from utils import emit_and_print_errors


env = DynamicVariable('env', T.LexicalEnv)


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def as_rettype():
        return EmptyEnv

    @langkit_property(public=True)
    def as_arg(env=T.LexicalEnv):
        ignore(env)
        return True

    @langkit_property(public=True, dynamic_vars=[env])
    def as_dynvar():
        return True


emit_and_print_errors(lkt_file='foo.lkt')

print('')
print('Done')
