from langkit.dsl import ASTNode, has_abstract_list

from utils import build_and_run


@has_abstract_list
class FooNode(ASTNode):
    pass


class Sequence(FooNode.list):
    pass


class Atom(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
