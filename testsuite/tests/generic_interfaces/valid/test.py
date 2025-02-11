from langkit.compiled_types import T
from langkit.dsl import (
    ASTNode, Struct, UserField, abstract, context_init_hook, implements
)
from langkit.expressions.base import AbstractKind, No, langkit_property
from langkit.generic_interface import (
    ArrayInterface, GenericArgument, GenericInterface
)

from utils import emit_and_print_errors


@context_init_hook
def init_hook(ctx):
    native_types_interface = GenericInterface("NativeTypesInterface", ctx)
    native_types_interface.add_method("returns_int", [], T.Int)
    native_types_interface.add_method(
        "with_args", [GenericArgument("a1", T.Int)], T.Int
    )

    bar_node_interface = GenericInterface("BarNodeInterface", ctx)

    foo_node_interface = GenericInterface("FooNodeInterface", ctx)
    foo_node_interface.add_method("returns_bar", [], bar_node_interface)
    foo_node_interface.add_method(
        "returns_array", [], ArrayInterface(bar_node_interface)
    )
    foo_node_interface.add_method(
        "with_args",
        [GenericArgument("a1", bar_node_interface)],
        bar_node_interface
    )

    abstract_interface = GenericInterface("AbstractInterface", ctx)
    abstract_interface.add_method("p", [], T.Int)

    struct_interface = GenericInterface(
        "StructInterface", ctx, is_always_node=False
    )
    struct_interface.add_method("f_int", [], T.Int)
    struct_interface.add_method(
        "f_node", [], ctx.resolve_interface("FooNodeInterface")
    )


@implements("StructInterface")
class FooStruct(Struct):
    f1 = UserField(public=True, type=T.Int,
                   implements="StructInterface.f_int")
    f2 = UserField(public=True, type=T.FooNode.entity,
                   implements="StructInterface.f_node")


@implements("FooNodeInterface")
class FooNode(ASTNode):

    @langkit_property(public=True, implements="FooNodeInterface.returns_bar")
    def p1():
        return No(T.BarNode.entity)

    @langkit_property(public=True, implements="FooNodeInterface.returns_array")
    def p2():
        return No(T.BarNode.entity.array)

    @langkit_property(public=True, implements="FooNodeInterface.with_args")
    def p3(arg=T.BarNode.entity):
        return arg


@implements("BarNodeInterface")
class BarNode(FooNode):
    pass


@implements("NativeTypesInterface")
class NativeTypes(FooNode):

    @langkit_property(public=True,
                      implements="NativeTypesInterface.returns_int")
    def p_nat1():
        return 1

    @langkit_property(public=True,
                      implements="NativeTypesInterface.with_args")
    def p_nat2(arg=T.Int):
        return arg


@abstract
@implements("AbstractInterface")
class AbstractNode(FooNode):

    @langkit_property(public=True,
                      return_type=T.Int,
                      kind=AbstractKind.abstract,
                      implements="AbstractInterface.p")
    def prop():
        pass


class NonAbstract(AbstractNode):
    @langkit_property(public=True,
                      return_type=T.Int,
                      implements="AbstractInterface.p")
    def prop():
        return 1


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)

print('')
print('Done')
