from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, Struct, abstract, env_metadata
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


@env_metadata
class Metadata(Struct):
    pass


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


@abstract
class Stmt(FooNode):
    pass


class Def(Stmt):
    id = Field()
    body = Field()

    env_spec = EnvSpec(
        add_to_env_kv(Self.id.symbol, Self),
        add_env()
    )

    faulty_prop = Property(Self._env_mappings_0)


class Block(Stmt):
    items = Field()

    env_spec = EnvSpec(add_env())


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
