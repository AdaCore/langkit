from __future__ import annotations

import dataclasses
from typing import Type, TypeVar

from langkit.compile_context import CompileCtx
from langkit.diagnostics import Location, diagnostic_context, error
from langkit.frontend.utils import lkt_context

import liblktlang as L


def same_node(left: L.LktNode, right: L.LktNode) -> bool:
    """
    Return whether ``left`` and ``right`` designate the same node, regardless
    of generic instantiation information.

    .. todo:: This should probably belong to Liblktlang, in one form or
       another. See use cases.
    """
    return left.unit == right.unit and left.sloc_range == right.sloc_range


def check_no_decoding_error(
    node: L.LktNode,
    result: L.DecodedCharValue | L.DecodedStringValue
) -> None:
    """
    If ``result`` has an error, stop with the corresponding diagnostic.
    """
    if result.has_error:
        filename = node.unit.filename
        line = result.error_sloc.line
        column = result.error_sloc.column
        with diagnostic_context(Location(filename, line, column)):
            error(result.error_message)


def denoted_str(strlit: L.StringLit | L.TokenLit | L.TokenPatternLit) -> str:
    """
    Return the string value that this literal denotes.
    """
    result = strlit.p_denoted_value
    check_no_decoding_error(strlit, result)
    return result.value


def denoted_char(charlit: L.CharLit) -> str:
    """
    Return the character value that this literal denotes.
    """
    result = charlit.p_denoted_value
    check_no_decoding_error(charlit, result)
    return result.value


@dataclasses.dataclass
class StaticValue:
    """
    Value known at compile time.
    """

    @classmethod
    def kind(cls) -> str:
        """
        Kind for this value (bool, int, string, ...).
        """
        # Since this is an abstract class method, this should not be reachable,
        # but mypy complains about derived class not correctly overriding it,
        # so we cannot use ABC helpers here.
        raise AssertionError


@dataclasses.dataclass
class StaticBool(StaticValue):
    value: bool

    @classmethod
    def kind(cls) -> str:
        return "bool"


@dataclasses.dataclass
class StaticString(StaticValue):
    value: str

    @classmethod
    def kind(cls) -> str:
        return "string"


@dataclasses.dataclass
class StaticPattern(StaticValue):
    value: str

    @classmethod
    def kind(cls) -> str:
        return "pattern"


AnyStaticValue = TypeVar("AnyStaticValue", bound=StaticValue)


def generic_parse_static(
    ctx: CompileCtx,
    expr: L.Expr,
    expected: Type[AnyStaticValue],
) -> AnyStaticValue:
    """
    Parse ``expr`` as a static value. If its kind does not match the
    ``expected`` type, emit a user diagnostic and abort. Return the value
    otherwise.
    """

    top_level_expr = expr

    def parse(expr: L.Expr) -> StaticValue:
        match expr:
            case L.RefId():
                match expr.text:
                    case "false":
                        return StaticBool(False)
                    case "true":
                        return StaticBool(True)

            case L.PatternSingleLineStringLit() | L.TokenPatternLit():
                return StaticPattern(denoted_str(expr))

            case L.StringLit() | L.TokenLit():
                return StaticString(denoted_str(expr))

            case L.TokenPatternConcat():
                lhs = parse(expr.f_left)
                rhs = parse(expr.f_right)

                # The grammar is supposed to ensure that lhs is a token pattern
                # concatenation itself and that rhs is a token pattern literal,
                # so we cannot get anything but static patterns here.
                assert isinstance(lhs, StaticPattern)
                assert isinstance(rhs, StaticPattern)

                return StaticPattern(lhs.value + rhs.value)

            case L.BinOp():
                lhs = parse(expr.f_left)
                rhs = parse(expr.f_right)

                if isinstance(expr.f_op, L.OpAmp):
                    match lhs:
                        case StaticString():
                            with lkt_context(expr.f_right):
                                if not isinstance(rhs, StaticString):
                                    error(
                                        f"{lhs.kind()} expected, got"
                                        f" {rhs.kind()}"
                                    )
                            return StaticString(lhs.value + rhs.value)
                        case StaticPattern():
                            with lkt_context(expr.f_right):
                                if not isinstance(rhs, StaticPattern):
                                    error(
                                        f"{lhs.kind()} expected, got"
                                        f" {rhs.kind()}"
                                    )
                            return StaticPattern(lhs.value + rhs.value)
                        case _:
                            with lkt_context(expr.f_left):
                                error(
                                    f"string or pattern expected, got"
                                    f" {lhs.kind()}"
                                )

        # Report non-static expressions at the top level so that we can provide
        # the expected type in the error message: typing for static expressions
        # is exclusively bottom-up except for the top level expression thanks
        # to the "expected" argument.
        with lkt_context(top_level_expr):
            error(f"static {expected.kind()} value expected")

    result = parse(expr)
    if not isinstance(result, expected):
        with lkt_context(expr):
            error(f"{expected.kind()} expected, got {result.kind()}")
    return result


def parse_static_bool(ctx: CompileCtx, expr: L.Expr) -> bool:
    """
    Return the bool value that this expression denotes.
    """
    return generic_parse_static(ctx, expr, StaticBool).value


def parse_static_str(ctx: CompileCtx, expr: L.Expr) -> str:
    """
    Return the string value that this expression denotes.
    """
    return generic_parse_static(ctx, expr, StaticString).value


def parse_static_pattern(ctx: CompileCtx, expr: L.Expr) -> str:
    """
    Return the pattern value that this expression denotes.
    """
    return generic_parse_static(ctx, expr, StaticPattern).value
