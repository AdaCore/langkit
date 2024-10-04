from langkit.compiled_types import T
from langkit.dsl import ASTNode, context_init_hook, implements
from langkit.generic_interface import GenericInterface

from utils import emit_and_print_errors


@context_init_hook
def init_hook(ctx):
    interface = GenericInterface("Interface", ctx)
    interface.add_method("method", [], T.Int)


class FooNode(ASTNode):
    pass


@implements("Interface")
class BarNode(FooNode):
    pass


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)

print('')
print('Done')
