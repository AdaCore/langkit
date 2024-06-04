"""Check that the Unparsing/Prettier API works as expect."""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, AbstractField, Field, abstract

from utils import GPRMain, build_and_run


class FooNode(ASTNode):
    pass


class Block(FooNode):
    items = Field(type=T.Decl.list)


@abstract
class Decl(FooNode):
    name = AbstractField(type=T.Name)


class NullQual(FooNode):
    enum_node = True
    qualifier = True


class VarDecl(Decl):
    is_null = Field(type=T.NullQual)
    name = Field(type=T.Name)
    type_expr = Field(type=T.Name)
    value = Field(type=T.Expr)


class ParamSpec(FooNode):
    name = Field(type=T.Name)
    type_expr = Field(type=T.Name)
    default_expr = Field(type=T.Expr)


class FunDecl(Decl):
    name = Field(type=T.Name)
    args = Field(type=T.ParamSpec.list)
    return_type = Field(type=T.Name)
    body = Field(type=T.Stmt.list)


@abstract
class Stmt(FooNode):
    pass


class ExprStmt(Stmt):
    expr = Field(type=T.Expr)


class AssignStmt(Stmt):
    names = Field(type=T.Name.list)
    expr = Field(type=T.Expr)


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class DottedName(Expr):
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.Name)


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


class SyncNode(FooNode):
    """
    Node used to test the token synchronization during unparsing: there are
    both pre (kwA) and post (kwF) tokens for the node itself (SyncNode), an
    inter token between the two fields (kwC) and pre/post tokens (kwD and kwE)
    for child_2.
    """
    child_1 = Field(type=T.Name)
    child_2 = Field(type=T.Name)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=[
        GPRMain("main.adb", ["config.json", "example.txt"]),
        GPRMain(
            "main.adb", ["-r", "param_spec", "config.json", "param_spec.txt"]
        ),
        GPRMain("main.adb", ["token_sync_config_1.json", "token_sync.txt"]),
        GPRMain("main.adb", ["token_sync_config_2.json", "token_sync.txt"]),
        "invalid_config.adb",
        "commands.adb",
        "default_config.adb",
    ],
    types_from_lkt=True,
    generate_unparser=True,
    default_unparsing_config="default_cfg.json",
)
print("Done")
