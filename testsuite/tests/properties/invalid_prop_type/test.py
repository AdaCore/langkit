from langkit.dsl import ASTNode, Field
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class BarCode(FooNode):
    a = Field()
    prop_1 = Property(Self.a.prop_2)


class BarNode(FooNode):
    prop_2 = Property(Self.parent.cast(BarCode).prop_1)


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt": {"types_from_lkt": False}}
)
print('')
print('Done')
