from langkit.dsl import ASTNode, Struct, context_init_hook, implements
from langkit.generic_interface import GenericInterface

from utils import emit_and_print_errors


@context_init_hook
def init_hook(ctx):
    GenericInterface("Interface", ctx, is_always_node=False)


@implements("Interface", "Interface")
class FooStruct(Struct):
    pass


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    pass


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)

print('')
print('Done')
