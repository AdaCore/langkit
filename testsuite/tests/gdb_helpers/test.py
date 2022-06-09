"""
Check that GDB helpers work as expected.

This is meant to be "the" big test that checks all aspects of GDB helpers:
Langkit-defined commands, pretty-printers, etc. To achieve this in a simple
way (i.e. not creating a big realistic language spec) we just create dummy
properties that implement the control-flow we want, or that deal with the types
for which we need to check pretty-printers: our goal here is just to cover GDB
helpers features.
"""

import subprocess
import sys

from langkit.dsl import ASTNode, AnalysisUnit, Struct, T, UserField
from langkit.envs import EnvSpec, add_env
from langkit.expressions import (
    ArrayLiteral, Entity, If, Let, No, Self, String, Var, langkit_property
)

from utils import build_and_run


class MyStruct(Struct):
    a = UserField(type=T.Int)
    b = UserField(type=T.Int)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def id_unit(u=AnalysisUnit):
        return u

    @langkit_property(public=True)
    def id_node(n=T.FooNode):
        return n

    @langkit_property(public=True)
    def test_strings():
        empty = Var(String(""))
        foo = Var(String("foo"))
        s_quote = Var(String("'"))
        d_quote = Var(String('"'))
        quote_mix = Var(String("\"'"))
        lf = Var(String("\n"))
        nul = Var(String("\x00"))

        arr = Var(ArrayLiteral([
            empty,
            foo,
            s_quote,
            d_quote,
            quote_mix,
            lf,
            nul,
        ]))
        return arr.length  # BREAK:test_strings

    @langkit_property()
    def get_rebindings(inverse=T.Bool):
        example_nodes = Var(Self.parent.cast(T.Example.list).as_array)
        n1 = Var(example_nodes.at(If(inverse, 1, 2)))
        n2 = Var(example_nodes.at(If(inverse, 2, 1)))
        return No(T.EnvRebindings).append_rebinding(
            n1.children_env,
            n2.children_env,
        )

    @langkit_property(public=True)
    def test_rebindings():
        null = Var(No(T.EnvRebindings))
        r1 = Var(Self.get_rebindings(False))
        r2 = Var(Self.get_rebindings(True))
        concat = Var(r1.concat_rebindings(r2))

        arr = Var(ArrayLiteral([
            null,
            r1,
            r2,
            concat,
        ]))
        return arr.length  # BREAK:test_rebindings

    @langkit_property(public=True)
    def test_envs():
        null = Var(No(T.LexicalEnv))
        primary = Var(Self.children_env)
        orphan = Var(primary.env_orphan)
        group = Var(ArrayLiteral([primary, null]).env_group())
        rebound = Var(primary.rebind_env(Self.get_rebindings(False)))

        arr = Var(ArrayLiteral([
            null,
            primary,
            orphan,
            group,
            rebound,
        ]))
        return arr.length  # BREAK:test_envs

    @langkit_property(public=True)
    def test_entities():
        null_root = Var(No(T.FooNode.entity))
        null_example = Var(No(T.Example.entity))
        ent_root = Var(Entity)  # BREAK:test_state
        ent_example = Var(ent_root.cast(T.Example))
        rebound = Var(T.FooNode.entity.new(
            node=Self,
            info=T.EntityInfo.new(
                md=No(T.Metadata),
                rebindings=Self.get_rebindings(False),
                from_rebound=False,
            ),
        ))

        arr = Var(ArrayLiteral([
            null_root,
            null_example.cast(T.FooNode),
            ent_root,
            ent_example.cast(T.FooNode),
            rebound,
        ]))
        return arr.length  # BREAK:test_entities

    @langkit_property(public=True)
    def test_arrays():
        empty = Var(No(T.Example.array))
        single = Var(ArrayLiteral([Self.cast(Example)]))
        complete = Var(Self.parent.cast(T.Example.list).as_array)

        arr = Var(empty.concat(single).concat(complete))
        return arr.length  # BREAK:test_arrays

    @langkit_property(public=True)
    def test_vectors():
        arr = Var(Self.parents().map(
            lambda n:
            n.children  # BREAK:test_vectors
        ))
        return arr.length

    @langkit_property(public=True)
    def test_tokens():
        null = Var(No(T.Token))
        first = Var(Self.token_start)

        arr = Var(ArrayLiteral([null, first]))
        return arr.length  # BREAK:test_tokens

    @langkit_property()
    def control_flow_helper(item=T.FooNode.array):
        return item.length + 1

    @langkit_property(public=True)
    def test_control_flow(i=T.Int):
        nodes = Var(Self.parent.children)
        arr = Var(nodes.map(
            lambda n:
            n.parents().length
            + Let(lambda item=n.children: Self.control_flow_helper(item))
        ))
        return i + arr.length

    @langkit_property(public=True)
    def test_struct(i=T.Int):
        result = Var(MyStruct.new(
            a=i,
            b=i + 10,
        ))
        return result.a  # BREAK:test_struct


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(add_env())


# Build the generated library and the Ada test program
build_and_run(lkt_file="expected_concrete_syntax.lkt", ada_main="main.adb")

# Run the test program under GDB to check the helpers. We keep this part in
# separate scripts to make it convenient, for debugging pruposes, to run these
# checks without re-building the library/program.
for script in ["check_printers.py", "check_control_flow.py", "check_state.py"]:
    subprocess.check_call([sys.executable, script])

print("Done")
