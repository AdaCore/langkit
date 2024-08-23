"""
Test that, thanks to the PLE root mechanism, it is possible to run PLE on only
one PLE root in an analysis unit that contains multiple PLE roots.
"""

from langkit.dsl import ASTNode, Annotations, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv, do, reference
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Id(FooNode):
    token_node = True

    @langkit_property(return_type=T.Scope, external=True,
                      uses_entity_info=False, uses_envs=False)
    def referenced_scope_or_error(or_error=T.Bool):
        pass

    @langkit_property(public=True, return_type=T.Scope.entity)
    def referenced_scope():
        return Self.referenced_scope_or_error(False).as_bare_entity

    @langkit_property(return_type=T.LexicalEnv)
    def referenced_env():
        return Self.resolve.cast(T.Scope)._.children_env

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get_first(Self.symbol)


class Scope(FooNode):
    name = Field(type=T.Id)
    deps = Field(type=T.Dep.list)
    defs = Field(type=T.Def.list)
    refs = Field(type=T.Ref.list)

    annotations = Annotations(ple_unit_root=True)

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env(),
        do(Self.deps.map(
            lambda d: d.referenced_scope_or_error(or_error=False)
        )),
        reference(
            nodes=Self.deps.map(lambda d: d.cast(T.FooNode)),
            through=T.Id.referenced_env,
        ),
    )


class Dep(Id):
    pass


class Def(FooNode):
    name = Field(type=T.Id)

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
    )


class Ref(Id):
    pass


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
