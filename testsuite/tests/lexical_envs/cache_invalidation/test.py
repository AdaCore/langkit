"""
Check that setting up cache collection with the default heuristic as well as
with a custom one works as expected. Since those two configurations require
two different compilation contexts, we call `build_and_run` two times.
"""
import langkit
from langkit.compile_context import CacheCollectionConf, LibraryEntity
from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import (
    If, No, Self, String, Var, langkit_property, new_env_assoc
)

from utils import build_and_run

import os
import os.path
import shutil


configs = [
    ("default", CacheCollectionConf(30, None)),
    ("custom", CacheCollectionConf(30, LibraryEntity(
        "Libfoolang.Implementation.Extensions",
        "Should_Collect_Env_Caches"
    )))
]

test_dir = os.getcwd()

for dirname, config in configs:
    print("== " + dirname + " ==")

    # The AST definition cannot be shared across calls to `build_and_run`,
    # hence why they are inside the loop.
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

    # We want each testcase to live in its own folder to ease debugging
    # (otherwise the second call to `build_and_run` would override the
    # libfoolang generated by the first call), hence we create dedicated
    # directories and copy the required files for each case.
    os.mkdir(dirname)
    shutil.copy('expected_concrete_syntax.lkt', dirname)
    shutil.copy('main.adb', dirname)
    shutil.copy('.gnatdebug', dirname)

    # Each case can have its own extensions directory
    ext_dir = dirname + "_extensions"
    if os.path.isdir(ext_dir):
        shutil.copytree(ext_dir, os.path.join(dirname, "extensions"))

    os.chdir(dirname)
    build_and_run(
        lkt_file='expected_concrete_syntax.lkt',
        gpr_mains=["main.adb"],
        types_from_lkt=True,
        cache_collection_conf=config
    )

    # We need to clean up some internal data structures for two consecutive
    # language creation to work as expected.
    langkit.reset()

    os.chdir(test_dir)
    print()

print('Done')
