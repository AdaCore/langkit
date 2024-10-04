from langkit.dsl import ASTNode, context_init_hook
from langkit.generic_interface import GenericInterface

from utils import emit_and_print_errors


@context_init_hook
def init_hook(ctx):
    GenericInterface("Interface", ctx)
    GenericInterface("Interface", ctx)


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    pass


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)

print('')
print('Done')
