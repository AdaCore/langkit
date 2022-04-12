"""
Check that the ".find" DSL construct works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import String, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Int)
    def find_above(values=T.Int.array, threshold=T.Int):
        return values.find(lambda i: i > threshold)

    # Dummy "find" expression to exercize the special case of iterating on a
    # list entity.

    @langkit_property(public=True, return_type=T.Example.entity)
    def find_entity(l=T.Example.list.entity):
        return l.find(lambda n: n.text == String("example"))


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=False)
print('Done')
