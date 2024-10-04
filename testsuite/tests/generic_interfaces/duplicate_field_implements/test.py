from langkit.compiled_types import T
from langkit.dsl import (
    ASTNode, Struct, UserField, context_init_hook, implements
)
from langkit.generic_interface import GenericInterface

from utils import emit_and_print_errors


@context_init_hook
def init_hook(ctx):
    interface = GenericInterface("Interface", ctx, is_always_node=False)
    interface.add_method("field", [], T.Int)


@implements("Interface")
class FooStruct(Struct):

    f1 = UserField(public=True, type=T.Int, implements="Interface.field")
    f2 = UserField(public=True, type=T.Int, implements="Interface.field")


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    pass


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)

print('')
print('Done')
