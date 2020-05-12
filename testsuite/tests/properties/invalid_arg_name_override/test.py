from langkit.dsl import ASTNode, Bool
from langkit.expressions import langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    @langkit_property(public=True)
    def prop(a=Bool):
        return a


class Example(FooNode):
    @langkit_property()
    def prop(b=Bool):
        return b


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
