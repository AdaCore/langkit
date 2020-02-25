from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    env_spec = EnvSpec(add_env(), add_env())


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
