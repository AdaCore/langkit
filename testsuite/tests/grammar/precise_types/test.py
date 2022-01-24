"""
Check precise types for parse fields.
"""

from langkit.dsl import ASTNode, Field, T

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class A(FooNode):
    token_node = True


class B(FooNode):
    token_node = True


class SpecialList(FooNode.list):
    pass


class Holder(FooNode):
    a_or_b = Field(type=T.FooNode)
    a_or_b_list = Field(type=T.FooNode.list)
    a_list_or_b_list = Field(type=T.FooNode)
    special_a_list = Field(type=SpecialList)
    special_a_list_or_special_b_list = Field(type=SpecialList)
    special_a_or_b_list = Field(type=SpecialList)


ctx = emit_and_print_errors(lkt_file='foo.lkt')
for n in ctx.astnode_types:
    if n.dsl_name == 'Holder':
        node = n
        break

fields = {f.original_name: f for f in node.get_fields()}
for _, f in sorted(fields.items()):
    print('== Doc for {} =='.format(f.qualname))
    print(f.doc)
    print('')

print('Done')
