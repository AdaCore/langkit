from langkit.dsl import ASTNode, T
from langkit.expressions import DynamicVariable, Self, langkit_property

from utils import build_and_run


dyn_var = DynamicVariable("dyn_var", T.Int)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(dynamic_vars=[dyn_var])
    def add(value=T.Int):
        return value + dyn_var

    @langkit_property(public=True)
    def next(value=T.Int):
        return dyn_var.bind(1, Self.add(value))


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
