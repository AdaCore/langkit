from langkit.dsl import ASTNode, Field
from langkit.expressions import Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class ListNode(FooNode):
    nb_list = Field()
    prop = Property(Self.nb_list.map(lambda i, _: i), public=True)


class NumberNode(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
