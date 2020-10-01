"""
Test that the introspection API works as expected for arrays introspection.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    id_int_array = Property(lambda id=T.Int.array: id, public=True)
    id_bigint_array = Property(lambda id=T.BigInt.array: id, public=True)

    # The following ensure that code generation properly inserts downcasts
    # where needed.
    id_example_array = Property(
        lambda id=T.Example.entity.array: id,
        public=True
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
