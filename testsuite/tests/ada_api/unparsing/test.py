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
    callee = Field(type=T.Name, nullable=True)
    args = Field(type=T.CallArg.list)


class ExampleCall(Call):
    pass


class Number(Expr):
    token_node = True


class Ref(Expr):
    name = Field(type=T.Name)


mains = []


def add_main(config, srcfile, rule=None, sloc=None):
    args = [config, srcfile]
    if rule:
        args += ["-r", rule]
    if sloc:
        args += ["-s", sloc]
    mains.append(GPRMain("main.adb", args))


add_main("config.json", "example.txt")
add_main("config.json", "param_spec.txt", rule="param_spec")

mains += [
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
    add_main("config.json", "trivias/{}".format(source))

add_main("trivias/var_group_config.json", "trivias/var_decls.txt")
add_main(
    "trivias/var_group_config.json",
    "trivias/list_nested_reattach.txt",
    sloc="5:4^1",
)
add_main(
    "trivias/var_group_config.json",
    "trivias/list_nested_reattach.txt",
    sloc="9:4^1",
)

for cfg in ["default.json", "same_as_regular.json", "override.json"]:
    add_main("list_extra_sep/{}".format(cfg), "list_extra_sep/assign.txt")
    add_main("list_extra_sep/{}".format(cfg), "list_extra_sep/params.txt")

for i in ["none", 0, 1, 2]:
    add_main(
        "max_empty_lines_{}.json".format(i),
        "trivias/max_empty_lines.txt",
    )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=mains,
    types_from_lkt=True,
    generate_unparser=True,
    default_unparsing_config="default_cfg.json",
)
print("Done")
