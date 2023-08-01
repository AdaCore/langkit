"""
Check precise types for parse fields.
"""

from langkit.dsl import ASTNode, AbstractField, Field, T, abstract, synthetic
from langkit.expressions import Self, langkit_property

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
    a_or_b_or_null = Field(type=T.FooNode)


@abstract
class ForSynthParent(FooNode):
    f = AbstractField(type=T.FooNode)

    @langkit_property(memoized=True)
    def private_create(n=T.B):
        return T.ForSynthChild1.new(f=n)

    @langkit_property(public=True)
    def public_create(n=T.B.entity):
        return Self.private_create(n.node)


@synthetic
class ForSynthChild1(ForSynthParent):
    f = Field(type=T.FooNode)


class ForSynthChild2(ForSynthParent):
    f = Field(type=T.FooNode)


@synthetic
class OnlySynthNode(FooNode):
    f = Field(type=T.FooNode)

    @langkit_property(memoized=True)
    def private_create(n=T.A):
        return T.OnlySynthNode.new(f=n)

    @langkit_property(public=True)
    def public_create(n=T.A.entity):
        return Self.private_create(n.node)


ctx = emit_and_print_errors(lkt_file="foo.lkt")
print("")
for n in ctx.astnode_types:
    fields = {f.original_name: f for f in n.get_fields()}
    for _, f in sorted(fields.items()):
        print(f"== Doc for {f.qualname} ==")
        print(f.doc)
        print("")

print('Done')
