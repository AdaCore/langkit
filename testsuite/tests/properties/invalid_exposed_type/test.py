from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import DynamicVariable, ignore, langkit_property
from langkit.expressions.envs import EmptyEnv
from langkit.parsers import Grammar

from utils import emit_and_print_errors


env = DynamicVariable('env', T.LexicalEnvType)


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def as_rettype():
        return EmptyEnv

    @langkit_property(public=True)
    def as_arg(env=T.LexicalEnvType):
        ignore(env)
        return True

    @langkit_property(public=True, dynamic_vars=[env])
    def as_dynvar():
        return True


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example'),
)
emit_and_print_errors(grammar)

print('')
print('Done')
