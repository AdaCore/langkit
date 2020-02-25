from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, Int, Symbol
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
    base_prop = Property(lambda x=Symbol: 12)


emit_and_print_errors(lkt_file='foo.lkt')
print('')
print('Done')
