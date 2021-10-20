from langkit.dsl import ASTNode
from langkit.expressions import Let, No, Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    prop = Property(Let(lambda _=Self.parent: No(FooNode.entity)), public=True)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lkt_semantic_checks=True)
print('Done')
