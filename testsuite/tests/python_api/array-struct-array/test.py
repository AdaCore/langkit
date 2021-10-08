"""
Check that the Python bindings to wrap/unwrap arrays of structs of arrays work
as expected.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import Entity, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class ExampleHolder(Struct):
    examples = UserField(type=T.Example.entity.array)


class Example(FooNode):
    @langkit_property(return_type=ExampleHolder.array, public=True)
    def example_holders():
        return ExampleHolder.new(examples=Entity.singleton).singleton

    @langkit_property(return_type=ExampleHolder.array, public=True)
    def identity(a=ExampleHolder.array):
        return a


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
