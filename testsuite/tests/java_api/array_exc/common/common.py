from langkit.dsl import ASTNode, T, abstract
from langkit.expressions import PropertyError, langkit_property


@abstract
class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.FooNode.entity.array)
    def prop():
        return PropertyError(T.FooNode.entity.array, "this is an eror")


class Example(FooNode):
    token_node = True
