from __future__ import annotations

import abc
import argparse
import dataclasses
import json
import os.path
import subprocess
import sys
import tempfile
from typing import Any, NoReturn

from langkit.diagnostics import Location, print_error
import liblktlang as L


parser = argparse.ArgumentParser(
    description="Translate back and forth between JSON and Lkt syntaxes for"
    " the unparsing configuration"
)
parser.add_argument(
    "--output", "-o", help="Output file. Print on stdout if omitted."
)
parser.add_argument(
    "--to-lkt",
    action="store_true",
    default=True,
    help="Read the input file as JSON and translate it to Lkt.",
)
parser.add_argument(
    "--to-json",
    action="store_false",
    dest="to_lkt",
    help="Read the input file as Lkt and translate it to JSON.",
)
parser.add_argument("input-file", help="Input file.")


class FatalError(Exception):
    @classmethod
    def from_sloc(
        cls,
        filename: str,
        loc: L.Sloc,
        msg: str,
        end_loc: L.Sloc | None = None,
        lkt_unit: L.AnalysisUnit | None = None,
    ) -> NoReturn:
        print_error(
            msg,
            Location(
                file=filename,
                line=loc.line,
                column=loc.column,
                end_line=0 if end_loc is None else end_loc.line,
                end_column=0 if end_loc is None else end_loc.column,
                lkt_unit=lkt_unit,
            ),
        )
        raise cls()

    @classmethod
    def from_node(cls, n: L.LktNode, msg: str) -> NoReturn:
        cls.from_sloc(
            n.unit.filename,
            n.sloc_range.start,
            msg,
            n.sloc_range.end,
            n.unit,
        )


@dataclasses.dataclass(frozen=True)
class Parameter(abc.ABC):
    """
    Base class for a function parameter, used to describe function signatures
    in Lkt code for unparsing configurations: see the ``Signature`` class
    below.
    """

    name: str
    """
    Name for this parameter.
    """


@dataclasses.dataclass(frozen=True)
class Positional(Parameter):
    """
    Parameter that can be passed only as positonal.

    It is mandatory, unless ``optional`` is true.
    """

    optional: bool = False


@dataclasses.dataclass(frozen=True)
class Variadic(Parameter):
    """
    If present in the signature, the function accepts zero or multiple
    arguments.
    """

    pass


@dataclasses.dataclass(frozen=True)
class OptKw(Parameter):
    """
    Optional parameter, always passed as a keyword argument.
    """

    pass


class Signature:
    """
    Describes a function signature in Lkt code for unparsing configurations:
    see the ``Signature`` class below.
    """

    def __init__(self, *params: Parameter):
        self.positionals: list[Positional] = []
        self.variadic: Variadic | None = None
        self.optionals: dict[str, OptKw] = {}

        names: set[str] = set()
        for p in params:
            assert p.name not in names, f"duplicate {p.name!r} parameter"
            names.add(p.name)
            match p:
                case Positional():
                    if not p.optional:
                        assert (
                            not self.positionals
                            or not self.positionals[-1].optional
                        ), (
                            "mandatory positional arguments cannot follow"
                            " optional positional arguments"
                        )
                    self.positionals.append(p)
                case Variadic():
                    assert (
                        self.variadic is None
                    ), "at most one variadic allowed"
                    self.variadic = p
                case OptKw():
                    self.optionals[p.name] = p

    def bind(
        self,
        args: L.ArgumentList,
    ) -> tuple[dict[str, L.Expr], list[L.Expr]]:
        """
        Bind arguments to parameters in this signature.

        Return a mapping from parameter names to the corresponding argument
        expressions, and the list of variadic arguments.
        """
        result = {}
        varargs = []

        remaining_positionals = list(self.positionals)
        param: Parameter
        for a in args:
            if a.f_name:
                try:
                    param = self.optionals[a.f_name.text]
                except KeyError:
                    FatalError.from_node(a.f_name, "unexpected argument")
                result[param.name] = a.f_value
            elif remaining_positionals:
                param = remaining_positionals.pop(0)
                result[param.name] = a.f_value
            elif self.variadic:
                varargs.append(a.f_value)
            else:
                FatalError.from_node(a, "unexpected argument")

        if remaining_positionals:
            first_missing = remaining_positionals[-1]
            if not first_missing.optional:
                FatalError.from_node(
                    args.parent, "missing argument for {first_missing.name}"
                )

        return result, varargs


bubble_up_args = (
    OptKw("bubbleUpLeadingTrivias"),
    OptKw("bubbleUpTrailingTrivias"),
)
dedent_sig = Signature(Variadic("contents"), *bubble_up_args)
signatures = {
    "whitespace": Signature(Positional("length")),
    "align": Signature(Variadic("contents"), OptKw("width"), *bubble_up_args),
    "dedent": dedent_sig,
    "dedentToRoot": dedent_sig,
    "fill": Signature(Variadic("document"), *bubble_up_args),
    "group": Signature(
        Variadic("document"),
        OptKw("shouldBreak"),
        OptKw("id"),
        *bubble_up_args,
    ),
    "ifBreak": Signature(
        Positional("breakContents"),
        Positional("flatContents", optional=True),
        OptKw("groupId"),
    ),
    "indent": dedent_sig,
    "markAsRoot": dedent_sig,
    "innerRoot": dedent_sig,
    "continuationLineIndent": dedent_sig,
    "recurse": Signature(Positional("field")),
    "tableSeparator": Signature(Positional("text")),
}


def to_json(input_file: str) -> str:
    """
    Load a JSON configuration and translate it to Lkt.

    :param input_file: Filename for the JSON file to translate.
    :return: The translated Lkt source code.
    """
    unit = L.AnalysisContext().get_from_file(input_file)
    if unit.diagnostics:
        for d in unit.diagnostics:
            print(unit.format_gnu_diagnostic(d), file=sys.stderr)
        raise FatalError(f"{input_file}: parsing errors")

    node_configs: Any = {}
    result = {"node_configs": node_configs}
    root = unit.root
    assert isinstance(root, L.LangkitRoot)

    def error_from_sloc(loc: L.Sloc, msg: str) -> NoReturn:
        """
        Raise a ``FatalError`` rooted at the given source location.
        """
        FatalError.from_sloc(input_file, loc, msg, lkt_unit=unit)

    def error(n: L.LktNode, msg: str) -> NoReturn:
        """
        Raise a ``FatalError`` rooted at the location of the given node.
        """
        FatalError.from_node(n, msg)

    def validate_decl(full_decl: L.FullDecl) -> L.Decl:
        """
        Ensure that the given declaration has no annotation and return the
        inner declaration.
        """
        if len(full_decl.f_decl_annotations):
            error(full_decl.f_decl_annotations[0], "unexpected annotations")
        return full_decl.f_decl

    def validate_val_decl(decl: L.ValDecl) -> str:
        """
        Ensure that there is no type annotation and return the declaration
        name.
        """
        if decl.f_decl_type:
            error(decl.f_decl_type, "unexpected type")
        return decl.f_syn_name.text

    def reject_traits(decl: L.NamedTypeDecl) -> None:
        """
        Ensure that there is no inherited trait.
        """
        if len(decl.f_traits) != 0:
            error(decl.f_traits[0], "unexpected trait derivation")

    def parse_int(e: L.Expr) -> int:
        """
        Parse an integer literal.
        """
        if isinstance(e, L.NumLit):
            return int(e.text)
        else:
            error(e, "integer literal expected")

    def parse_bool(e: L.Expr) -> bool:
        """
        Parse a boolean literal.
        """
        if isinstance(e, L.RefId):
            if e.text == "true":
                return True
            elif e.text == "false":
                return False
        error(e, "Bool literal expected")

    def parse_str(e: L.Expr) -> str:
        """
        Parse a string literal or regex pattern.
        """
        if isinstance(e, (L.SingleLineStringLit, L.RegexPattern)):
            result = e.p_denoted_value
            if result.has_error:
                error_from_sloc(result.error_sloc, result.error_message)
            return result.value
        error(e, "string literal expected")

    def bubble_up_args(bound: dict[str, L.Expr]) -> dict:
        """
        Process ``bubbleUp*`` arguments (if present).
        """
        result = {}
        for name in ("bubbleUpLeadingTrivias", "bubbleUpTrailingTrivias"):
            if name in bound:
                result[name] = parse_bool(bound[name])
        return result

    def parse_args(e: L.CallExpr, doc: Any) -> None:
        args = []
        kwargs = {}
        for arg in e.f_args:
            value = parse_expression(arg.f_value)
            if arg.f_name:
                arg_name = arg.f_name.text
                if arg_name in kwargs:
                    error(arg.f_name, "argument passed multiple times")
                kwargs[arg_name] = value
            elif kwargs:
                error(
                    arg,
                    "positional argument forbidden after a keyword"
                    " argument",
                )
            else:
                args.append(value)
        if kwargs:
            doc["kwargs"] = kwargs
        if args or not kwargs:
            doc["args"] = args

    @dataclasses.dataclass(frozen=True)
    class MatchPattern:
        """
        Base class for node matching patterns.
        """

        node: L.LktNode

    @dataclasses.dataclass(frozen=True)
    class NodeMatchPattern(MatchPattern):
        """
        Pattern that matches nodes of a given kind.
        """

        type_name: str

    @dataclasses.dataclass(frozen=True)
    class NullMatchPattern(MatchPattern):
        """
        Pattern that matches null nodes.
        """

        pass

    @dataclasses.dataclass(frozen=True)
    class DefaultMatchPattern(MatchPattern):
        """
        Default pattern.
        """

        pass

    def parse_subtemplate(e: L.Expr | list[L.Expr]) -> Any:
        return parse_template_helper(e, expression=False)

    def parse_expression(e: L.Expr | list[L.Expr]) -> Any:
        return parse_template_helper(e, expression=True)

    def parse_pattern(p: L.Pattern) -> Any:
        match p:
            case L.RenamingComplexPattern(
                f_decl=L.BindingValDecl(f_syn_name=L.DefId() as def_id),
                f_pattern=None,
                f_details=L.PatternDetailList() as details,
                f_predicate=None,
            ) if len(details) == 0 and def_id.text == "_":
                return "*"

            case L.ComplexPattern(
                f_decl=None,
                f_pattern=L.NullPattern(),
                f_details=L.PatternDetailList() as details,
                f_predicate=None,
            ) if len(details) == 0:
                return None

            case L.ComplexPattern(
                f_decl=None,
                f_pattern=L.BoolPatternFalse(),
                f_details=L.PatternDetailList() as details,
                f_predicate=None,
            ) if len(details) == 0:
                return False

            case L.ComplexPattern(
                f_decl=None,
                f_pattern=L.BoolPatternTrue(),
                f_details=L.PatternDetailList() as details,
                f_predicate=None,
            ) if len(details) == 0:
                return True

            case L.ComplexPattern(
                f_decl=None,
                f_pattern=L.RegexPattern() as regex,
                f_details=L.PatternDetailList() as details,
                f_predicate=None,
            ) if len(details) == 0:
                return {
                    "kind": "symbol_literal",
                    "value": parse_str(regex),
                }

            case L.ComplexPattern(
                f_decl=None,
                f_pattern=L.TypePattern(
                    f_type_name=L.SimpleTypeRef(
                        f_type_name=L.RefId() as type_name
                    )
                ),
                f_details=L.PatternDetailList() as details,
                f_predicate=None,
            ):
                members = []
                for d in details:
                    match d:
                        case L.FieldPatternDetail(
                            f_id=L.Id() as field_name,
                            f_expected_value=sub_pattern,
                        ):
                            members.append(
                                {
                                    "member": field_name.text,
                                    "pattern": parse_pattern(sub_pattern),
                                }
                            )

                        case L.PropertyPatternDetail(
                            f_call=L.CallExpr(
                                f_name=L.RefId() as property_name
                            ) as call_expr,
                            f_expected_value=sub_pattern,
                        ):
                            m = {
                                "member": property_name.text,
                                "pattern": parse_pattern(sub_pattern),
                            }
                            parse_args(call_expr, m)
                            members.append(m)

                        case _:
                            error(p, "invalid member pattern")

                result = {"kind": "node", "type": type_name.text}
                if members:
                    result["members"] = members
                return result

            case L.NotPattern():
                return {
                    "kind": "not",
                    "pattern": parse_pattern(p.f_sub_pattern),
                }

            case L.OrPattern():
                patterns = []

                def visit(p: L.Pattern) -> None:
                    if isinstance(p, L.OrPattern):
                        visit(p.f_left_sub_pattern)
                        visit(p.f_right_sub_pattern)
                    else:
                        patterns.append(parse_pattern(p))

                visit(p)
                return {"kind": "or", "patterns": patterns}

            case _:
                error(p, "invalid pattern")

    def parse_template_helper(
        e: L.Expr | list[L.Expr],
        expression: bool,
    ) -> Any:
        """
        Translate a template expression to JSON.
        """
        result: Any

        if isinstance(e, list):
            result = [parse_template_helper(item, expression) for item in e]
            return result[0] if len(result) == 1 else result

        elif isinstance(e, L.SingleLineStringLit):
            if expression:
                if not e.p_is_prefixed_string:
                    return {"kind": "string", "value": parse_str(e)}
                elif e.p_prefix == "s":
                    return {"kind": "symbol", "value": parse_str(e)}
                else:
                    error(e, "unexpected string prefix")
            elif e.p_is_prefixed_string:
                error(e, "unexpected string prefix")
            return {"kind": "text", "text": parse_str(e)}

        elif isinstance(e, L.RefId):
            if e.text in (
                "recurse",
                "recurse_flatten",
                "recurse_left",
                "recurse_right",
                "breakParent",
                "line",
                "hardline",
                "hardlineWithoutBreakParent",
                "softline",
                "literalline",
                "flushLineBreaks",
                "trim",
                "whitespace",
                "this_field",
                "this_node",
            ):
                return e.text

        elif isinstance(e, L.BinOp):
            match e.f_op:
                case L.OpEq():
                    op = "="
                case L.OpAnd():
                    op = "and"
                case L.OpOr():
                    op = "or"
                case _:
                    error(e.f_op, "unexpected binary operator")
            return {
                "kind": "bin_op",
                "op": op,
                "lhs": parse_expression(e.f_left),
                "rhs": parse_expression(e.f_right),
            }

        elif isinstance(e, L.CastExpr):
            if e.f_null_cond.p_as_bool:
                error(e.f_null_cond, "unexpected non-cond marker")
            elif e.f_excludes_null.p_as_bool:
                error(e.f_null_cond, "unexpected null-excluding marker")
            elif not isinstance(e.f_dest_type, L.SimpleTypeRef):
                error(e.f_null_cond, "invalid type expression")
            return {
                "kind": "cast",
                "prefix": parse_expression(e.f_expr),
                "type": e.f_dest_type.f_type_name.text,
            }

        elif isinstance(e, L.DotExpr):
            if e.f_null_cond.p_as_bool:
                error(e.f_null_cond, "unexpected non-cond marker")

            prefix = parse_expression(e.f_prefix)
            match e.f_suffix.text:
                case "is_empty":
                    return {"kind": "is_empty", "node": prefix}

                case "symbol":
                    return {"kind": "node_symbol", "node": prefix}

                case "text":
                    return {"kind": "node_text", "node": prefix}

                case member:
                    return {
                        "kind": "eval_member",
                        "prefix": prefix,
                        "member": member,
                    }

        elif isinstance(e, L.CallExpr):
            callee = e.f_name
            if isinstance(callee, L.DotExpr):
                result = {
                    "kind": "eval_member",
                    "prefix": parse_expression(callee.f_prefix),
                    "member": callee.f_suffix.text,
                }
                parse_args(e, result)
                return result
            elif not isinstance(callee, L.RefId):
                error(callee, "identifier expected")
            name = callee.text

            try:
                signature = signatures[name]
            except KeyError:
                error(callee, "invalid template function")
            bound, varargs = signature.bind(e.f_args)

            if name == "whitespace":
                return {
                    "kind": "whitespace",
                    "length": parse_int(bound["length"]),
                }

            elif name == "align":
                width = bound["width"]
                return {
                    "kind": "align",
                    "width": (
                        parse_int(width)
                        if isinstance(width, L.NumLit)
                        else parse_str(width)
                    ),
                    "contents": parse_subtemplate(varargs),
                    **bubble_up_args(bound),
                }

            elif name in ("dedent", "dedentToRoot"):
                return {
                    "kind": name,
                    "contents": parse_subtemplate(varargs),
                    **bubble_up_args(bound),
                }

            elif name == "fill":
                return {
                    "kind": "fill",
                    "document": parse_subtemplate(varargs),
                    **bubble_up_args(bound),
                }

            elif name == "group":
                result = {
                    "kind": "group",
                    "document": parse_subtemplate(varargs),
                }
                if "shouldBreak" in bound:
                    result["shouldBreak"] = parse_bool(bound["shouldBreak"])
                if "id" in bound:
                    result["id"] = parse_str(bound["id"])
                result.update(**bubble_up_args(bound))
                return result

            elif name == "ifBreak":
                result = {
                    "kind": "ifBreak",
                    "breakContents": parse_subtemplate(bound["breakContents"]),
                }
                if "flatContents" in bound:
                    result["flatContents"] = parse_subtemplate(
                        bound["flatContents"]
                    )
                if "groupId" in bound:
                    result["groupId"] = parse_str(bound["groupId"])
                return result

            elif name in (
                "indent",
                "markAsRoot",
                "innerRoot",
                "continuationLineIndent",
            ):
                return {
                    "kind": name,
                    "contents": parse_subtemplate(varargs),
                    **bubble_up_args(bound),
                }

            elif name == "recurse":
                field = bound["field"]
                if not isinstance(field, L.RefId):
                    error(field, "field name expected")
                return {"kind": "recurse_field", "field": field.text}

            elif name == "tableSeparator":
                return {
                    "kind": "tableSeparator",
                    "text": parse_str(bound["text"]),
                }

            raise AssertionError(f"unreachable code (call to {name})")

        elif isinstance(e, L.ArrayLiteral):
            if e.f_element_type:
                error(e.f_element_type, "unexpected type")
            result = [
                parse_template_helper(item, expression) for item in e.f_exprs
            ]
            return result[0] if len(result) == 1 else result

        elif isinstance(e, L.IfExpr):
            else_part = parse_subtemplate(e.f_else_expr)
            for alt in reversed(e.f_alternatives):
                else_part = {
                    "kind": "if",
                    "condition": parse_expression(alt.f_cond_expr),
                    "then": parse_subtemplate(alt.f_then_expr),
                    "else": else_part,
                }
            return {
                "kind": "if",
                "condition": parse_expression(e.f_cond_expr),
                "then": parse_subtemplate(e.f_then_expr),
                "else": else_part,
            }

        elif isinstance(e, L.Isa):
            return {
                "kind": "is_a",
                "node": parse_expression(e.f_expr),
                "pattern": parse_pattern(e.f_pattern),
            }

        elif isinstance(e, L.MatchExpr):
            return {
                "kind": "match",
                "node": parse_expression(e.f_match_expr),
                "matchers": [
                    {
                        "pattern": (
                            parse_pattern(branch.f_pattern)
                            if isinstance(branch, L.PatternMatchBranch)
                            else error(branch, "unsupported branch syntax")
                        ),
                        "document": parse_subtemplate(branch.f_expr),
                    }
                    for branch in e.f_branches
                ],
            }

        elif isinstance(e, L.NotExpr):
            return {"kind": "not", "operand": parse_expression(e.f_expr)}

        elif isinstance(e, L.StringLit):
            return e.p_denoted_value

        error(e, "unexpected template")

    if len(root.f_imports) != 0:
        error(root.f_imports[0], "unexpected import")

    # Process top-level declarations (values, structs, classes)
    for full_decl in root.f_decls:
        decl = validate_decl(full_decl)
        if isinstance(decl, L.ValDecl):
            name = validate_val_decl(decl)
            if name == "max_empty_lines":
                result["max_empty_lines"] = parse_int(decl.f_expr)
            else:
                error(decl.f_syn_name, "unknown parameter")

        elif isinstance(decl, L.StructDecl):
            name = decl.f_syn_name.text
            if name != "tokens":
                error(decl.f_syn_name, "unknown struct")
            reject_traits(decl)

            result["token_configs"] = token_configs = {}
            for full_decl in decl.f_decls:
                decl = validate_decl(full_decl)

                if isinstance(decl, L.ValDecl):
                    name = validate_val_decl(decl)
                    expr = decl.f_expr

                    if name == "default":
                        valid = False
                        if isinstance(expr, L.RefId):
                            ref = expr.text
                            if ref in ("lower", "upper", "original"):
                                valid = True
                                token_configs["default"] = ref
                        if not valid:
                            error(expr, "unexpected default")

                    elif name == "formattings":
                        if not (
                            isinstance(expr, L.MatchExpr)
                            and isinstance(expr.f_match_expr, L.RefId)
                            and expr.f_match_expr.text == "token"
                        ):
                            error(expr, "'match token' expected")
                        formattings: Any = {}
                        token_configs["formattings"] = formattings
                        for branch in expr.f_branches:
                            if not isinstance(branch, L.PatternMatchBranch):
                                error(branch, "pattern match branch expected")
                            match branch.f_pattern:
                                case L.ComplexPattern(
                                    f_decl=None,
                                    f_pattern=L.RegexPattern() as lit,
                                    f_details=L.PatternDetailList() as details,
                                    f_predicate=None,
                                ) if len(details) == 0:
                                    formattings.setdefault(
                                        parse_str(lit),
                                        parse_str(branch.f_expr),
                                    )
                                case _:
                                    error(
                                        branch.f_pattern, "unexpected pattern"
                                    )

                    else:
                        error(decl.f_syn_name, "unknown parameter")
                else:
                    error(decl, "unknown parameter")

        elif isinstance(decl, L.ClassDecl):
            if decl.f_syn_base_type:
                error(decl.f_syn_base_type, "unexpected class derivation")
            reject_traits(decl)

            node_cfg: Any = {}
            node_configs[decl.f_syn_name.text] = node_cfg
            for full_decl in decl.f_decls:
                decl = validate_decl(full_decl)

                if isinstance(decl, L.ValDecl):
                    name = validate_val_decl(decl)

                    if name in ("node", "sep", "leading_sep", "trailing_sep"):
                        node_cfg[name] = parse_subtemplate(decl.f_expr)
                    elif name in (
                        "flush_before_children",
                        "independent_lines",
                    ):
                        node_cfg[name] = parse_bool(decl.f_expr)
                    elif not name.startswith("f_"):
                        error(decl.f_syn_name, "unknown parameter")
                    else:
                        node_cfg.setdefault("fields", {})
                        node_cfg["fields"][name] = parse_subtemplate(
                            decl.f_expr
                        )

                elif isinstance(decl, L.StructDecl):
                    if decl.f_syn_name.text != "table":
                        error(decl.f_syn_name, "unexpected struct")
                    reject_traits(decl)
                    table_cfg: Any = {}

                    for full_decl in decl.f_decls:
                        decl = validate_decl(full_decl)
                        if isinstance(decl, L.ValDecl):
                            name = validate_val_decl(decl)
                            expr = decl.f_expr
                            if name == "disabled":
                                table_cfg["disabled"] = parse_bool(expr)

                            elif name == "sep_before":
                                table_cfg["sep_before"] = parse_bool(expr)

                            elif name == "split":
                                split = []
                                if not isinstance(expr, L.ArrayLiteral):
                                    error(expr, "array literal expected")
                                elif expr.f_element_type:
                                    error(
                                        expr.f_element_type, "unexpected type"
                                    )
                                for expr in expr.f_exprs:
                                    if expr.text not in (
                                        "empty_line",
                                        "line_comment",
                                    ):
                                        error(expr, "unexpected split value")
                                    split.append(expr.text)
                                table_cfg["split"] = split

                            elif name == "must_break":
                                table_cfg["must_break"] = parse_bool(expr)

                            else:
                                error(decl.f_syn_name, "unknown parameter")

                        elif isinstance(decl, L.StructDecl):
                            if decl.f_syn_name.text != "join":
                                error(decl.f_syn_name, "unexpected struct")
                            join_decl = decl
                            reject_traits(decl)
                            join_cfg: Any = {}
                            table_cfg["join"] = join_cfg

                            for full_decl in decl.f_decls:
                                decl = validate_decl(full_decl)
                                if not isinstance(decl, L.ValDecl):
                                    error(decl, "unexpected declaration")
                                name = validate_val_decl(decl)
                                expr = decl.f_expr
                                if name == "predicate":
                                    if not isinstance(expr, L.RefId):
                                        error(expr, "predicate name expected")
                                    join_cfg["predicate"] = expr.text
                                elif name == "template":
                                    join_cfg["template"] = parse_subtemplate(
                                        expr
                                    )
                                else:
                                    error(decl.f_syn_name, "unknown parameter")

                            if "predicate" not in join_cfg:
                                error(join_decl, "missing predicate")
                        else:
                            error(decl, "unexpected declaration")

                    disabled = table_cfg.pop("disabled", False)
                    if disabled and table_cfg:
                        error(decl, "unexpected params for disabled table")
                    node_cfg["table"] = None if disabled else table_cfg

                else:
                    error(decl, "unexpected declaration")

        else:
            error(decl, "unexpected declaration")

    return json.dumps(result, indent=2, sort_keys=True) + "\n"


def to_lkt(input_file: str) -> str:
    """
    Load a Lkt configuration and translate it to JSONJ.

    :param input_file: Filename for the JSON file to translate.
    :return: The translated JSON configuration.
    """
    try:
        with open(input_file) as json_f:
            content = json_f.read()
    except IOError as exc:
        raise FatalError(str(exc))

    try:
        doc = json.loads(content)
    except (json.JSONDecodeError, UnicodeDecodeError) as exc:
        raise FatalError(f"{input_file}: {exc}")
    lines: list[str] = []

    def lkt_lit(value: bool | int | str) -> str:
        """
        Turn a JSON literal to the corresponding Lkt excerpt.
        """
        match value:
            case bool():
                return "true" if value else "false"
            case int():
                return str(value)
            case str():
                return json.dumps(value)
            case _:
                raise AssertionError

    def process_bubble_up(doc: Any) -> None:
        """
        Append ``bubbleUp*`` arguments, if present in ``doc``.
        """
        for name in ("bubbleUpLeadingTrivias", "bubbleUpTrailingTrivias"):
            if name in doc:
                lines.append(f", {name}={lkt_lit(doc[name])}")

    def process_args(doc: Any) -> None:
        """
        Translate call arguments (args and kwargs) to Lkt in ``lines``.
        """
        has_args = False
        if "args" in doc:
            for value in doc["args"]:
                if has_args:
                    lines.append(",")
                else:
                    lines.append("(")
                    has_args = True
                process_template(value)
        if "kwargs" in doc:
            for name, value in doc["kwargs"].items():
                if has_args:
                    lines.append(",")
                else:
                    lines.append("(")
                    has_args = True
                lines.append(f"{name}=")
                process_template(value)
        if has_args:
            lines.append(")")
        elif "args" in doc or "kwargs" in doc:
            lines.append("()")

    def process_pattern(doc: Any) -> None:
        """
        Translate the ``doc`` pattern template to Lkt in ``lines``.
        """
        match doc:
            case None:
                lines.append("null")

            case bool():
                lines.append("true" if doc else "false")

            case "*":
                lines.append("_")

            case {"kind": "symbol_literal", "value": str_value}:
                lines.append(lkt_lit(str_value))

            case {"kind": "node", "type": type_name}:
                lines.append(type_name)
                if "members" in doc:
                    lines.append("(")
                    for i, m in enumerate(doc["members"]):
                        if i > 0:
                            lines.append(",")
                        lines.append(m["member"])
                        process_args(m)
                        lines.append(":")
                        process_pattern(m["pattern"])
                    lines.append(")")

            case {"kind": "not", "pattern": p}:
                lines.append("not")
                process_pattern(p)

            case {"kind": "or", "patterns": patterns}:
                for i, p in enumerate(patterns):
                    if i > 0:
                        lines.append("|")
                    process_pattern(p)

            case _:
                raise FatalError(f"invalid template: {doc}")

    def process_template(doc: Any, unwrap_list: bool = False) -> None:
        """
        Translate the ``doc`` template expression to Lkt in ``lines``.
        """
        match doc:
            case list():
                unwrap_list = unwrap_list and len(doc) > 0
                if not unwrap_list:
                    lines.append("[")
                for i, subdoc in enumerate(doc):
                    if i > 0:
                        lines.append(",")
                    process_template(subdoc)
                if not unwrap_list:
                    lines.append("]")

            case (
                "recurse"
                | "recurse_left"
                | "recurse_right"
                | "recurse_flatten"
                | "breakParent"
                | "line"
                | "hardline"
                | "hardlineWithoutBreakParent"
                | "softline"
                | "literalline"
                | "flushLineBreaks"
                | "trim"
                | "whitespace"
                | "this_field"
                | "this_node"
            ):
                lines.append(doc)

            case {"kind": "whitespace", "length": length}:
                lines.append(f"whitespace({lkt_lit(length)})")

            case {"kind": "align", "width": width, "contents": contents}:
                lines.append("align(")
                process_template(contents, unwrap_list=True)
                lines.append(", width=" + lkt_lit(width))
                process_bubble_up(doc)
                lines.append(")")

            case {
                "kind": ("dedent" | "dedentToRoot") as kind,
                "contents": document,
            }:
                lines.append(f"{kind}(")
                process_template(document, unwrap_list=True)
                process_bubble_up(doc)
                lines.append(")")

            case {"kind": "fill", "document": document}:
                lines.append("fill(")
                process_template(document, unwrap_list=True)
                process_bubble_up(doc)
                lines.append(")")

            case {"kind": "group", "document": document}:
                lines.append("group(")
                process_template(document, unwrap_list=True)
                if "shouldBreak" in doc:
                    lines.append(
                        f", shouldBreak={lkt_lit(doc['shouldBreak'])}"
                    )
                if "id" in doc:
                    lines.append(f", id={lkt_lit(doc['id'])}")
                process_bubble_up(doc)
                lines.append(")")

            case {
                "kind": "ifBreak",
                "breakContents": b_cont,
            }:
                lines.append("ifBreak(")
                process_template(b_cont)
                if "flatContents" in doc:
                    lines.append(",")
                    process_template(doc["flatContents"])
                if "groupId" in doc:
                    lines.append(f", groupId={lkt_lit(doc['groupId'])}")
                lines.append(")")

            case {
                "kind": "if",
                "condition": cond_doc,
                "then": then_doc,
                "else": else_doc,
            }:
                alts: list[tuple[Any, Any]] = []
                while (
                    isinstance(else_doc, dict) and else_doc.get("kind") == "if"
                ):
                    alts.append((else_doc["condition"], else_doc["then"]))
                    else_doc = else_doc["else"]

                lines.append("if")
                process_template(cond_doc)
                lines.append("then")
                process_template(then_doc)
                for cond_doc, then_doc in alts:
                    lines.append("elif")
                    process_template(cond_doc)
                    lines.append("then")
                    process_template(then_doc)
                lines.append("else")
                process_template(else_doc)

            case {"kind": "match", "node": node_doc, "matchers": matchers}:
                lines.append("match")
                process_template(node_doc)
                lines.append("{")
                for m in matchers:
                    lines.append("case")
                    process_pattern(m["pattern"])
                    lines.append("=>")
                    process_template(m["document"])
                lines.append("}")

            case {
                "kind": (
                    "indent"
                    | "markAsRoot"
                    | "innerRoot"
                    | "continuationLineIndent"
                ) as kind,
                "contents": contents,
            }:
                lines.append(f"{kind}(")
                process_template(contents, unwrap_list=True)
                process_bubble_up(doc)
                lines.append(")")

            case {"kind": "recurse_field", "field": field}:
                lines.append(f"recurse({field})")

            case {"kind": "tableSeparator", "text": text}:
                lines.append(f"tableSeparator({lkt_lit(text)})")

            case {"kind": "text", "text": text}:
                lines.append(lkt_lit(text))

            case {"kind": "bin_op", "op": op, "lhs": lhs_doc, "rhs": rhs_doc}:
                process_template(lhs_doc)
                lines.append(
                    {
                        "=": "==",
                        "and": "and",
                        "or": "or",
                    }[op]
                )
                process_template(rhs_doc)

            case {"kind": "cast", "prefix": prefix_doc, "type": type_str}:
                process_template(prefix_doc)
                lines.append(f".as[{type_str}]")

            case {
                "kind": "eval_member",
                "prefix": prefix_doc,
                "member": member,
            }:
                process_template(prefix_doc)
                lines.append(f".{member}")
                has_args = False
                if "args" in doc:
                    for value in doc["args"]:
                        if has_args:
                            lines.append(",")
                        else:
                            lines.append("(")
                            has_args = True
                        process_template(value)
                if "kwargs" in doc:
                    for name, value in doc["kwargs"].items():
                        if has_args:
                            lines.append(",")
                        else:
                            lines.append("(")
                            has_args = True
                        lines.append(f"{name}=")
                        process_template(value)
                if has_args:
                    lines.append(")")
                elif "args" in doc or "kwargs" in doc:
                    lines.append("()")

            case {"kind": "is_a", "node": node_doc, "pattern": pattern_doc}:
                process_template(node_doc)
                lines.append("is")
                process_pattern(pattern_doc)

            case {"kind": "is_empty", "node": node_doc}:
                process_template(node_doc)
                lines.append(".is_empty")

            case {"kind": "node_symbol", "node": node_doc}:
                process_template(node_doc)
                lines.append(".symbol")

            case {"kind": "node_text", "node": node_doc}:
                process_template(node_doc)
                lines.append(".text")

            case {"kind": "not", "operand": operand_doc}:
                lines.append("not")
                process_template(operand_doc)

            case {"kind": "string", "value": str_val}:
                lines.append(lkt_lit(str_val))

            case {"kind": "symbol", "value": str_val}:
                lines.append("s" + lkt_lit(str_val))

            case _:
                raise FatalError(f"invalid template: {doc}")

    # First turn the whole JSON configuration to Lkt into ``lines``

    max_empty_lines = doc.get("max_empty_lines")
    if max_empty_lines is not None:
        lines.append(f"val max_empty_lines = {max_empty_lines}")
        lines.append("")

    token_configs = doc.get("token_configs")
    if token_configs:
        lines.append("struct tokens {")

        default = token_configs.get("default")
        if default:
            lines.append(f"val default = {default}")

        formattings = token_configs.get("formattings")
        if formattings:
            lines.append("val formattings = match token {")
            for before, after in formattings.items():
                lines.append(f"case {lkt_lit(before)} => {lkt_lit(after)}")
            lines.append("}")

        lines.append("}")
        lines.append("")

    node_configs = doc.get("node_configs")
    if node_configs:
        for node, config in node_configs.items():
            lines.append(f"class {node} {{")

            for name in ("node", "sep", "leading_sep", "trailing_sep"):
                template = config.get(name)
                if template:
                    lines.append(f"val {name}=")
                    process_template(template)

            for name in ("flush_before_children", "independent_lines"):
                if name in config:
                    lines.append(f"val {name}={lkt_lit(config[name])}")

            fields_cfg = config.get("fields", {})
            for field, template in fields_cfg.items():
                lines.append(f"val {field}=")
                process_template(template)

            if "table" in config:
                table_cfg = config["table"]
                lines.append("struct table {")
                if table_cfg is None:
                    lines.append("val disabled = true")
                else:
                    if "sep_before" in table_cfg:
                        lines.append(
                            "val sep_before ="
                            f" {lkt_lit(table_cfg['sep_before'])}"
                        )
                    if "split" in table_cfg:
                        lines.append(
                            f"val split = [{', '.join(table_cfg['split'])}]"
                        )
                    if "must_break" in table_cfg:
                        lines.append(
                            "val must_break ="
                            f" {lkt_lit(table_cfg['must_break'])}"
                        )

                    join_cfg = table_cfg.get("join")
                    if join_cfg:
                        lines.append("struct join {")
                        lines.append(
                            f"val predicate = {join_cfg['predicate']}"
                        )
                        if "template" in join_cfg:
                            lines.append("val template =")
                            process_template(join_cfg["template"])
                        lines.append("}")
                lines.append("}")

            lines.append("}")
            lines.append("")

    # Write it in a temporary file and then reformat it using lkt_unparse
    with tempfile.NamedTemporaryFile(
        mode="w", prefix="unparsing2lkt", suffix=".lkt", delete=False
    ) as f:
        try:
            for line in lines:
                print(line, file=f)
            f.close()
            p = subprocess.run(
                ["lkt_unparse", f.name],
                encoding="utf-8",
                stdout=subprocess.PIPE,
                stdin=subprocess.DEVNULL,
            )
            if p.returncode != 0:
                print("lkt_unparse failed:", file=sys.stderr)
                print(p.stdout, file=sys.stderr)
                print("Its input was:", file=sys.stderr)
                lineno_max = len(str(len(lines)))
                for i, line in enumerate(lines, 1):
                    print(
                        f"{str(i).rjust(lineno_max)} |", line, file=sys.stderr
                    )
                raise FatalError(
                    f"lkt_unparse failed with status code {p.returncode}"
                )
            return p.stdout
        finally:
            os.remove(f.name)


def main(argv: list[str] | None = None) -> int:
    args = parser.parse_args(argv)

    try:
        input_file = getattr(args, "input-file")
        output = to_lkt(input_file) if args.to_lkt else to_json(input_file)

        if args.output:
            with open(args.output, "w") as f:
                f.write(output)
        else:
            sys.stdout.write(output)
        return 0

    except FatalError as exc:
        print(str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
