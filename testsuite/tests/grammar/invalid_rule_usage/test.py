from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, abstract

from utils import emit_and_print_errors


@abstract
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
