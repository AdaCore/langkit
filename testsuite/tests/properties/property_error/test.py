from langkit.dsl import ASTNode, T
from langkit.expressions import PropertyError, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def raise_msg():
        return PropertyError(T.Bool, 'Custom error message!')

    @langkit_property(public=True)
    def raise_no_msg():
        return PropertyError(T.Bool)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
