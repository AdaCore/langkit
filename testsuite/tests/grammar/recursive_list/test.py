from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Self

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()
    body = Field()
    env_spec = EnvSpec(add_env(), add_to_env_kv(Self.name, Self))


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
