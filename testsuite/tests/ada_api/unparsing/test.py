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
    is_null = Field(type=T.NullQual)
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


mains = [
    GPRMain("main.adb", ["config.json", "example.txt"]),
    GPRMain(
        "main.adb", ["-r", "param_spec", "config.json", "param_spec.txt"]
    ),
    "invalid_config.adb",
    "commands.adb",
    "default_config.adb",
]

for source in [
    "block_comments_only.txt",
    "block_empty.txt",
    "block_first_empty_line.txt",
    "block_mixed.txt",
    "block_trailing.txt",
    "list_nested_reattach.txt",
    "list_ghost_before_trailing.txt",
    "list_separator.txt",
    "list_separator_trailing.txt",
    "unit_comments_only.txt",
    "unit_empty.txt",
]:
    mains.append(
        GPRMain("main.adb", ["config.json", "trivias/{}".format(source)])
    )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=mains,
    types_from_lkt=True,
    generate_unparser=True,
    default_unparsing_config="default_cfg.json",
)
print("Done")
