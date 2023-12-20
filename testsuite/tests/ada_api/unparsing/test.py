"""Check that the Unparsing/Prettier API works as expect."""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field, abstract

from utils import GPRMain, build_and_run


class FooNode(ASTNode):
    pass


class NullQual(FooNode):
    enum_node = True
    qualifier = True


class VarDecl(FooNode):
    is_null = Field(type=T.NullQual)
    name = Field(type=T.Name)
    type_expr = Field(type=T.Name)
    value = Field(type=T.Expr)


class ParamSpec(FooNode):
    name = Field(type=T.Name)
    type_expr = Field(type=T.Name)
    default_expr = Field(type=T.Expr)


class FunDecl(FooNode):
    name = Field(type=T.Name)
    args = Field(type=T.ParamSpec.list)
    return_type = Field(type=T.Name)
    body = Field(type=T.Stmt.list)


class Stmt(FooNode):
    expr = Field(type=T.Expr)


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Addition(Expr):
    lhs = Field(type=T.Expr)
    rhs = Field(type=T.Expr)


class CallArg(FooNode):
    name = Field(type=T.Name)
    expr = Field(type=T.Expr)


class Call(Expr):
    callee = Field(type=T.Name)
    args = Field(type=T.CallArg.list)


class Number(Expr):
    token_node = True


class Ref(Expr):
    name = Field(type=T.Name)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=[
        GPRMain("main.adb", ["config.json", "example.txt"]),
        GPRMain(
            "main.adb", ["-r", "param_spec", "config.json", "param_spec.txt"]
        ),
        "invalid_config.adb",
        "commands.adb",
    ],
    generate_unparser=True,
)
print("Done")
