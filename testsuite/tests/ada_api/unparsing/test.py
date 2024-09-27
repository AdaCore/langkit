"""Check that the Unparsing/Prettier API works as expect."""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, AbstractField, Field, abstract
from langkit.expressions import (
    If, Not, PropertyError, Self, String, Var, langkit_property
)

from utils import GPRMain, build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Bool)
    def table_join_needed():
        this_decl = Var(Self.cast(T.AnnotatedDecl)._.decl.cast(T.VarDecl))
        prev_decl = Var(
            Self
            .as_bare_entity
            .previous_sibling
            .cast(T.AnnotatedDecl)
            ._.decl
            .cast(T.VarDecl)
        )
        return (
            Not(prev_decl.is_null)
            & Not(this_decl.is_null)
            & If(
                Self.cast(T.AnnotatedDecl).annotation_names.empty,
                True,
                PropertyError(T.Bool, "predicate bug"),
            )
            & (prev_decl.name.text == String("left"))
            & (this_decl.name.text == String("right"))
        )

    @langkit_property(public=True, return_type=T.Bool)
    def table_join_bad_args(i=T.Int):
        return i == 0

    @langkit_property(public=True, return_type=T.Int)
    def table_join_bad_rtype():
        return 0


@abstract
class Decl(FooNode):
    name = AbstractField(type=T.Name)


@abstract
class BaseBlock(FooNode):
    pass


class Block(BaseBlock):
    items = Field(type=T.Decl.list)


class PlusDecls(Decl.list):
    pass


class PlusBlock(BaseBlock):
    items = Field(type=T.PlusDecls)


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
    body = Field(type=T.FooNode)


class LabeledStmts(FooNode):
    label = Field(type=T.Name)
    stmts = Field(type=T.Stmt.list)


class Stmts(FooNode):
    stmts = Field(type=T.Stmt.list)


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


class AnnotationList(Name.list):
    pass


class AnnotatedDecl(FooNode):
    annotation_names = Field(type=T.AnnotationList)
    decl = Field(type=T.Decl)


mains = []


def add_main(config, srcfile, rule=None, sloc=None, traces=[]):
    args = ["-c", config, srcfile]
    if rule:
        args += ["-r", rule]
    if sloc:
        args += ["-s", sloc]
    for t in traces:
        args += ["-t", t]
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
    "list_nested_reattach_2.txt",
    "list_ghost_before_trailing.txt",
    "list_separator.txt",
    "list_separator_trailing.txt",
    "unit_comments_only.txt",
    "unit_empty.txt",
]:
    add_main("config.json", "trivias/{}".format(source))
add_main("trivias/wrapped_list_item.json", "trivias/wrapped_list_item.txt")

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

# Check that broken groups detection works as expected for
# fill/group/ifBreak/table.
for cfg, src in [
    # Breaking their parent group is not supposed to break their contents.
    #
    # To verify this, run the unparsing on a source that contains a variable
    # declaration and with a unparsing configuration that:
    #
    # 1) puts a "softline" document between the "var" keyword and the
    #    declaration identifier (unparsing this requires a space between these
    #    two keywords).
    # 2) wraps both in a fill/group/ifBreak/table,
    # 3) has a "hardline" outside of that wrapping node.
    #
    # The hardline is supposed to break the parent group, but the breaking
    # behavior is not supposed to reach the nested "softline" document. As a
    # result, the unparsing engine is supposed to add an extra whitespace
    # before the declaration identifier.
    ("fill", "one_var"),
    ("group", "one_var"),
    ("ifbreak", "one_var"),
    ("table", "one_var"),

    # The root document is processed as if it was wrapped in a broken group:
    # "top level" softline documents are supposed to be expanded into line
    # breaks, so in this case we expect the unparsing engine *not* to add a
    # whitespace.
    ("root", "one_var"),

    # Tables must break if they contain more than one element
    ("table", "comment_in_vars"),

    # Documents in a "must_break" table must behave as if they were in a broken
    # group (even if the table itself is not broken, because it has only one
    # element), so "softline" documents must be considered as expanded to new
    # lines (the engine must not add extra whitespaces before the declaration
    # identifiers).
    ("table_must_break", "comment_in_vars"),
    ("table_must_break", "one_var"),

    # A "must_break" table must *not* break its parent if it contains
    # only one element and that element itself does not trigger a break.
    ("table_must_break_not_parent", "one_var"),

    # Items in "fill" documents must consider they live in a broken group even
    # if there are breaking items in the same "fill" document. In the following
    # testcase, "line" after VarDecl nodes will expand to spaces even though
    # the "fill" document contains a "hardline", so the unparsing engine needs
    # to add an extra line break after the comment.
    ("fill_broken", "comment_in_var"),
]:
    add_main(
        "breaking/{}.json".format(cfg),
        "breaking/{}.txt".format(src),
        traces=["final_doc"],
    )

for cfg in ["no_split", "split_all", "split_comments", "split_empty_lines"]:
    add_main(
        "table_align/{}.json".format(cfg),
        "table_align/var_decls.txt",
    )

for cfg in ["missing", "true", "false"]:
    add_main(
        "table_align/sep_before_{}.json".format(cfg),
        "table_align/fun_decls.txt",
    )

add_main(
    "table_align/table_sep_in_field.json",
    "table_align/calls.txt",
)

for cfg in ["inherit", "inherit_null"]:
    add_main(
        "table_align/{}.json".format(cfg),
        "table_align/blocks.txt",
    )

for src in ["big.txt", "error.txt"]:
    add_main("table_join/config.json", "table_join/{}".format(src))


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=mains,
    types_from_lkt=True,
    generate_unparser=True,
    default_unparsing_config="default_cfg.json",
)
print("Done")
