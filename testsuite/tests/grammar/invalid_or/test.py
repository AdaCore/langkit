from langkit.dsl import ASTNode, has_abstract_list

from utils import emit_and_print_errors


@has_abstract_list
class Element(ASTNode):
    pass


class Sequence(Element.list):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
