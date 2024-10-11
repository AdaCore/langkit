"""
Check that setting up cache collection with a custom heuristic one works as
expected.
"""

from langkit.compile_context import CacheCollectionConf, LibraryEntity
from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import (
    If, No, Self, String, Var, langkit_property, new_env_assoc
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(return_type=T.env_assoc.array)
    def dummy_entries(count=T.Int, base=T.String):
        next_base = Var(base.concat(String("o")))
        return new_env_assoc(
            key=base.to_symbol,
            value=Self
        ).singleton.concat(
            If(count == 0,
               No(T.env_assoc.array),
               Self.dummy_entries(count - 1, next_base))
        )

    @langkit_property(return_type=T.Example.entity, public=True)
    def lookup(sym=T.Symbol):
        return Self.children_env.get_first(sym).cast(Example.entity)

    env_spec = EnvSpec(
        add_env(),
        add_to_env(Self.dummy_entries(100, String("foo"))),
    )

build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
    cache_collection_conf=CacheCollectionConf(
        30,
        LibraryEntity(
            "Libfoolang.Implementation.Extensions", "Should_Collect_Env_Caches"
        ),
    ),
)

print("Done")
