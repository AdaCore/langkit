from langkit.dsl import ASTNode, implements

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@implements("NonExisting")
class BarNode(FooNode):
    pass


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)

print('')
print('Done')
