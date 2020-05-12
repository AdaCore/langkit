from langkit.dsl import ASTNode, Field, Int
from langkit.expressions import Property, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class BarCode(FooNode):
    a = Field()

    @langkit_property(return_type=Int)
    def base_prop(x=Int):
        return x


class BarNode(BarCode):
    base_prop = Property(lambda: 12)


emit_and_print_errors(lkt_file='foo.lkt')
print('')
print('Done')
