from __future__ import annotations

import abc
from contextlib import contextmanager
import dataclasses
import inspect
from itertools import count
import os.path
from typing import (
    Any as _Any,
    Callable,
    ClassVar,
    Iterator,
    Sequence,
    TYPE_CHECKING,
    cast,
)


import funcy

from langkit.common import ascii_repr
from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    Argument,
    ArrayType,
    CompiledType,
    EntityType,
    EnumValue,
    MemberNames,
    NoCompiledType,
    T,
    UserField,
    gdb_helper,
    get_context,
)
from langkit.diagnostics import (
    DiagnosticContext,
    Location,
    WarningSet,
    check_source_language,
    error,
)
from langkit.documentation import RstCommentChecker
from langkit.expressions.utils import assign_var
from langkit.generic_interface import InterfaceMethodProfile
import langkit.names
import langkit.names as names
import langkit.utils
from langkit.utils import assert_type


if TYPE_CHECKING:
    import liblktlang as L

    from langkit.expressions.logic import PredicateErrorDiagnosticTemplate


def expr_or_null(
    expr: Expr,
    default_expr: Expr | None,
    error_location: Location | L.LktNode,
    context_name: str,
    use_case_name: str,
) -> tuple[Expr, Expr]:
    """
    If ``default_expr`` is not None, construct it and unify its type with the
    type of ``expr``. Otherwise, check that ``expr`` has a nullable type and
    build a null expression for it. Return the conversion of ``expr`` and
    ``default_expr`` to the unified type.

    :param expr: Initial expression.
    :param expr: Default expression.
    :param context_name: Used for error message. Name of the expression that
        uses ``expr``.
    :param use_case_name: User for error message. Name for what
        ``default_expr`` is used.
    """
    de: Expr
    if default_expr is None:
        check_source_language(
            expr.type.null_allowed,
            "{} should have a default value provided, in cases where the type"
            " of the provided {} (here {}) does not have a default null"
            " value.".format(
                context_name.capitalize(), use_case_name, expr.type.lkt_name
            ),
            location=error_location,
        )
        de = NullExpr(None, expr.type)
    else:
        de = default_expr

    return expr.unify(de, error_location, context_name)


def match_default_values(
    left: Expr | None,
    right: Expr | None,
) -> bool:
    """
    Return whether the given optional default values are identical.
    """
    if left is None or right is None:
        return left == right
    else:
        assert isinstance(left, Expr), left
        assert isinstance(right, Expr), right
        return left.ir_dump == right.ir_dump


TypePredicate = Callable[[CompiledType], bool]


def maybe_cast(
    error_location: Location | L.LktNode,
    expr: Expr,
    expected_type: CompiledType,
    custom_msg: str | None = None,
) -> Expr:
    """
    Check that the type of ``expr`` matches ``expected_type``: if not, emit an
    error located at ``error_location``, potentially with the given custom
    error message.

    Return ``expr`` itself if it has the expected type. Otherwise, if its
    type is a subtype of ``expected_type``, return a wrapping expression for
    the conversion.
    """
    if not custom_msg:
        custom_msg = "Expected type {expected}, got {expr_type}"

    check_source_language(
        expr.type.matches(expected_type),
        custom_msg.format(
            expected=expected_type.lkt_name,
            expr_type=expr.type.lkt_name,
        ),
        location=error_location,
    )

    # If the type matches expectation but is incompatible in the generated
    # code, generate a conversion. This is needed for the various ASTNodeType
    # instances.
    if expected_type != expr.type:
        from langkit.expressions import CastExpr

        assert isinstance(expected_type, (ASTNodeType, EntityType))
        expr = CastExpr(None, expr, expected_type)

    return expr


@dataclasses.dataclass(frozen=True)
class ExprDebugInfo:
    """
    Debugging information for a Lkt expression.
    """

    label: str
    """
    Human-readable description of the Lkt expression.
    """

    location: Location
    """
    Location for the expression in the Lkt source code.
    """


class Expr:
    """
    Expression intermediate representation (lowered from Lkt, to be lowered to
    Ada).

    Code generation happens in two steps:

    * render_pre, which yields a list of statements to "prepare" the value the
      expression produces;
    * render_expr, which yields an expression that evaluates to this value.

    Subclasses must override the _render_pre method to implement the first
    step and override the _render_expr method to implement the second one.
    This base classe provides wrappers to these method, these create a local
    variable and make it contain the resulting value.
    """

    static_type: CompiledType | None = None
    """
    If subclasses redefine this, then the type property will return this
    static type value.
    """

    expr_count = iter(count(1))
    """
    Generator of unique identifiers for expressions in GDB helpers. See
    render_pre.
    """

    location: Location
    _result_var: LocalVars.LocalVar | None

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        result_var_name: str | names.Name | None = None,
        skippable_refcount: bool = False,
    ):
        """
        :param debug_info: Concise description of the Lkt expression that this
            expression implements. None for other expressions.
        :param result_var_name: If provided, create a local variable using this
            as a base name to hold the result of this expression.  In this
            case, the "type" property must be ready.
        :param skippable_refcount: If True, this expression can omit having a
            result variable even though its result is ref-counted.  This makes
            it possible to simplify the generated code.
        """
        self.debug_info = debug_info

        if result_var_name:
            self._result_var = PropertyDef.get().vars.create(
                Location.builtin, result_var_name, self.type
            )
        else:
            self._result_var = None

        self.skippable_refcount = skippable_refcount

        self._render_pre_called = False
        """
        Safety guard: except for variables, it is highly suspicious for
        render_pre to be called more than once for a given expression.  This
        will happen if we start using such an expression multiple times in the
        expression tree.
        """

    @property
    def result_var(self) -> LocalVars.LocalVar | None:
        """
        Return the local variable used to store the result of this expression,
        if any. Note that if the result is not null, the caller can assume that
        the "render_expr" method only returns the result variable name.
        """
        return self._result_var

    def create_result_var(self, name: str) -> VariableExpr:
        """
        If this property already has a result variable, return it as a
        expression. Otherwise, create one and return it.

        :param name: Camel with underscores-formatted name for the result
            variable.
        """
        assert not self._render_pre_called, (
            "Trying to create a result variable while the expression has been"
            " rendered"
        )

        # If this is already a variable, we don't need to create another
        # variable to hold the same value: just return it.
        if isinstance(self, VariableExpr):
            return self

        # Otherwise, create a result variable if it does not exist yet
        elif not self._result_var:
            self._result_var = PropertyDef.get().vars.create(
                Location.builtin, name, self.type
            )

        result_var = self.result_var
        assert result_var is not None
        return result_var.ref_expr

    def render_pre(self) -> str:
        """
        Render initial statements that might be needed to the expression.
        """
        assert (
            not self._render_pre_called
        ), "{}.render_pre can be called only once".format(type(self).__name__)
        self._render_pre_called = not isinstance(
            self, (BindableLiteralExpr, VariableExpr)
        )

        assert (
            self.skippable_refcount
            or self.type is T.NoCompiledType
            or not self.type.is_refcounted
            or self._result_var
        ), (
            "Expr instances that return ref-counted values must store their"
            " result in a local variable (this {} does not).".format(self)
        )

        pre = self._render_pre()
        expr = str(self._render_expr())

        # Some expressions build their result directly inside the result
        # variable, and thus their _render_pre() method will only return the
        # name of the result variable. In such cases, there is no need to
        # add a tautological assignment (X:=X), which would hamper generated
        # code reading anyway.
        if self.result_var and expr != str(self.result_var.codegen_name):
            result = "{}\n{} := {};".format(
                pre, self.result_var.codegen_name.camel_with_underscores, expr
            )
        else:
            result = pre

        # If this expression materializes the computation of an Lkt expression
        # and its result is stored in a variable, make it visible to the GDB
        # helpers.
        prop = PropertyDef.get_or_none()
        if prop and self.debug_info and self.result_var:
            unique_id = str(next(self.expr_count))

            result = "{}\n{}\n{}".format(
                gdb_helper(
                    "expr-start",
                    unique_id,
                    self.debug_info.label,
                    self.result_var.codegen_name.camel_with_underscores,
                    gdb_loc(self.debug_info.location),
                ),
                result,
                gdb_helper("expr-done", unique_id),
            )

        return result

    def render_expr(self) -> str:
        """
        Render the expression itself.
        """
        return (
            self.result_var.codegen_name.camel_with_underscores
            if self.result_var
            else self._render_expr()
        )

    def _render_pre(self) -> str:
        """
        Per-expression kind implementation for render_pre. The default
        implementation returns no statement.
        """
        return ""

    @abc.abstractmethod
    def _render_expr(self) -> str:
        """
        Per-expression kind implementation for render_expr. To be overriden in
        subclasses.

        Note that the returned expression must be idempotent: each evaluation
        must return the exact same result for the exact same context.
        """
        ...

    def render(self) -> str:
        """
        Render both the initial statements and the expression itself. This is
        basically a wrapper that calls render_pre and render_expr in turn.
        """
        return "{}\n{}".format(self.render_pre(), self.render_expr())

    @property
    def type(self) -> CompiledType:
        """
        Returns the type of the expression.
        """
        if not self.static_type:
            raise NotImplementedError(
                "{} must redefine the type property, or to fill the"
                " static_type class field".format(self)
            )
        return self.static_type

    @property
    def ir_dump(self) -> str:
        """
        Return a textual representation of this expression tree.
        """
        return "\n".join(self._ir_dump(self.subexprs))

    @classmethod
    def _ir_dump(cls, json_like: object) -> list[str]:
        """
        Helper for "ir_dump". Return text representation as a list of lines.
        """
        max_cols = 72
        result = []

        def one_line_subdumps(subdumps: list[list[str]]) -> bool:
            """
            Return whether all dumps in "subdumps" are one-line long.
            """
            return all(len(d) == 1 for d in subdumps)

        # Adopt a specific dump format depending on the type of "json_like".
        # In each case below, first try to return a one-line dump that fits in
        # the column limit. If it does not, fall back to a multi-line dump.

        if isinstance(json_like, list):
            subdumps = [cls._ir_dump(elt) for elt in json_like]

            # One-line format: [A, B, ...]
            if one_line_subdumps(subdumps):
                one_liner = "[{}]".format(", ".join(d[0] for d in subdumps))
                if len(one_liner) <= max_cols:
                    return [one_liner]

            # Multi-line format::
            #
            #     * Aaaaaa...
            #     | aaaa
            #     * Bbbbbbbbbbb
            #     ...
            for elt in json_like:
                subdump = cls._ir_dump(elt)
                result.append("*  {}".format(subdump[0]))
                result.extend("|  {}".format(line) for line in subdump[1:])

        elif isinstance(json_like, dict):
            keys = sorted(json_like)
            subdumps = [cls._ir_dump(json_like[key]) for key in keys]
            items = zip(keys, subdumps)

            # One-line format: {A=a, B=b, ...}
            if one_line_subdumps(subdumps):
                one_liner = "{{{}}}".format(
                    ", ".join("{}={}".format(key, d[0]) for key, d in items)
                )
                if len(one_liner) <= max_cols:
                    return [one_liner]

            # Multi-line format::
            #
            #     A: aaaa
            #     B:
            #     |  bbbbbbbb...
            #     |  bbbbb
            for key, d in zip(keys, subdumps):
                if len(d) == 1 and len(d[0]) <= max_cols:
                    result.append("{}: {}".format(key, d[0]))
                else:
                    result.append("{}:".format(key))
                    result.extend("|  {}".format(line) for line in d)

        elif isinstance(json_like, Expr):
            class_name = getattr(
                json_like, "pretty_class_name", type(json_like).__name__
            )
            subdump = cls._ir_dump(json_like.subexprs)

            # One-line format: ExpressionName(...)
            if len(subdump) == 1:
                one_liner = "{}{}".format(class_name, subdump[0])
                if len(one_liner) <= max_cols:
                    return [one_liner]

            # Multi-line format::
            #
            #     ExpressionName(
            #     |  ...
            #     )
            result.append("{}(".format(class_name))
            result.extend("|  {}".format(line) for line in subdump)
            result.append(")")

        elif isinstance(json_like, CompiledType):
            return cls._ir_dump(json_like.name)

        elif isinstance(json_like, names.Name):
            result.append(json_like.camel_with_underscores)

        else:
            result.append(str(json_like))

        return result

    @property
    def subexprs(self) -> object:
        """
        A JSON-like datastructure to describe this expression.

        Leaves of this datastructure are: strings, CompiledType instances,
        AbtsractNodeData instances and Expr instances (for operands). This is
        used both for expression tree traversal and for IR dump.

        Subclasses must override this property if they have operands.
        """
        return []

    def flat_subexprs(
        self,
        filter: Callable[[object], bool] = lambda expr: isinstance(expr, Expr),
    ) -> list[object]:
        """
        Wrapper around "subexprs" to return a flat list of items matching
        "filter". By default, get all ``Expr`` instances.

        :param filter: Predicate to test whether a subexpression should be
            returned.
        """

        def explore(values: object) -> list[object]:
            if values is None:
                return []
            elif isinstance(values, (list, tuple)):
                return funcy.lmapcat(explore, values)
            elif isinstance(values, dict):
                return funcy.lmapcat(explore, values.values())
            elif filter(values):
                return [values]
            else:
                return []

        return explore(self.subexprs)

    def flat_actual_subexprs(self) -> list[Expr]:
        return cast(list[Expr], self.flat_subexprs())

    @property
    def bindings(self) -> list[VariableExpr]:
        """
        Return the list of variables defined in "self", including in subexprs.

        Subclasses must override the "_bindings" method.
        """
        # Do a copy to avoid mutating the expression own's data structures
        result = list(self._bindings())
        for expr in self.flat_actual_subexprs():
            result.extend(expr.bindings)
        return result

    def _bindings(self) -> list[VariableExpr]:
        """
        Return the list of variables "self" defines.

        Subclasses must override this method if they define variables.
        """
        return []

    def destructure_entity(
        self,
    ) -> tuple[SavedExpr, EvalMemberExpr, EvalMemberExpr]:
        """
        Must be called only on expressions that evaluate to entities.  Return
        3 expressions:

          1. A SavedExpr wrapper for self, so its result can be used multiple
             times.
          2. An expression that evaluates the entity node.
          3. An expression that evaluates the entity info.

        The SavedExpr (1) must be evaluated before any of (2) and (3) are
        evaluated themselves.
        """
        from langkit.expressions.structs import EvalMemberExpr

        assert self.type.is_entity_type
        fields = self.type.get_abstract_node_data_dict()
        saved = SavedExpr(None, "Saved", self)
        return (
            saved,
            EvalMemberExpr(None, saved.result_var_expr, fields["node"], []),
            EvalMemberExpr(None, saved.result_var_expr, fields["info"], []),
        )

    def unified_expr(self, t: CompiledType) -> Expr:
        """
        If ``self``'s type does not have exactly the given type, return a cast
        expr from it.
        """
        from langkit.expressions import CastExpr

        if self.type == t:
            return self

        assert isinstance(t, (ASTNodeType, EntityType))
        return CastExpr(None, self, t)

    def unify(
        self,
        expr: Expr,
        error_location: Location | L.LktNode,
        context_name: str,
    ) -> tuple[Expr, Expr]:
        """
        Try to unify the type of `self` and of `expr`, and return a couple of
        expressions for both that convert their results to this type. Emit a
        user diagnostic using `context_name` if both have mismatching types.

        :param expr: Expression to convert with `self`.
        :param error_location: Location for the user diagnostic, if emitted.
        :param context_name: User for error message. Name of the expression
            that uses `self` and `expr`.
        """
        rtype = self.type.unify(
            expr.type,
            error_location,
            f"Mismatching types in {context_name}: {self.type.lkt_name} and"
            f" {expr.type.lkt_name}",
        )
        return (self.unified_expr(rtype), expr.unified_expr(rtype))


class VariableExpr(Expr):
    """
    Expression that is just a reference to an already computed value.
    """

    pretty_class_name = "Var"

    def __init__(
        self,
        type: CompiledType,
        name: names.Name,
        local_var: LocalVars.LocalVar | None = None,
    ):
        """
        Create a variable reference expression.

        :param type: Type for the referenced variable.
        :param name: Name of the referenced variable.
        :param local_var: The corresponding local variable, if there is one.
        """
        self.static_type = assert_type(type, CompiledType)
        self.name = name
        self.local_var = local_var
        self._ignored = False

        super().__init__(None, skippable_refcount=True)

    @property
    def result_var(self) -> LocalVars.LocalVar | None:
        return self.local_var

    def _render_expr(self) -> str:
        return self.name.camel_with_underscores

    @property
    def source_name(self) -> str | None:
        """
        If it comes from the language specification, return the original
        source name for this variable. Return None otherwise.
        """
        return self.local_var.spec_name if self.local_var else None

    @property
    def ignored(self) -> bool:
        """
        If this comes from the language specification, return whether it is
        supposed to be ignored. Return False otherwise.
        """
        return self._ignored

    def set_ignored(self) -> None:
        """
        Ignore this variable in the context of the unused bindings warning
        machinery.
        """
        self._ignored = True

    def __repr__(self) -> str:
        src_name = self.source_name
        return "<VariableExpr {}{}>".format(
            self.name.lower, " ({})".format(src_name) if src_name else ""
        )

    @property
    def subexprs(self) -> dict:
        result = {"name": self.name.lower}
        if self.source_name:
            result["source-name"] = self.source_name
        return result


class ErrorExpr(Expr):
    """
    Expression that just raises an error.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        expr_type: CompiledType,
        exception_name: names.Name,
        message: str | None = None,
    ):
        """
        :param expr_type: Placeholder type for this expression, as if this
            expression would return a value.
        :param exception_name: Name of the Ada exception to raise.
        :param message: Optional error message.
        """
        self.static_type = expr_type
        self.exception_name = exception_name
        self.message = message
        super().__init__(None, skippable_refcount=True)

    def _render_expr(self) -> str:
        result = "raise {}".format(self.exception_name)
        if self.message:
            result += " with {}".format(ascii_repr(self.message))
        return result

    def __repr__(self) -> str:
        return "<ErrorExpr {} with {}>".format(
            self.exception_name, repr(self.message)
        )


class UnreachableExpr(ErrorExpr):
    """
    Placeholder expression for unreachable code.
    """

    def __init__(self, expr_type: CompiledType):
        super().__init__(
            None,
            expr_type,
            names.Name("Program_Error"),
            "Executing supposedly unreachable code",
        )


class LiteralExpr(Expr):
    """
    Expression for literals of any type.

    The pecularity of literals is that they are not required to live in local
    variables. Because of this, if the type at hand is ref-counted, then the
    literal must be a ref-counting "insensitive" value, for instance a null
    value or an Ada aggregate.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        template: str,
        expr_type: CompiledType | None,
        operands: list[Expr] | None = None,
    ):
        """
        :param template: String template for the expression. Rendering will
            interpolate it with the operands' render_expr methods evaluation.
        :param expr_type: The return type of the expression.
        :param operand: Operands for this expression.
        """
        self.static_type = expr_type
        self.template = template
        self.operands = list(operands) if operands else []

        super().__init__(debug_info, skippable_refcount=True)

    def _render_pre(self) -> str:
        return "\n".join(o.render_pre() for o in self.operands)

    def _render_expr(self) -> str:
        return self.template.format(*[o.render_expr() for o in self.operands])

    @property
    def subexprs(self) -> dict:
        return {
            "0-type": self.static_type,
            "1-template": self.template,
            "2-operands": self.operands,
        }

    def __repr__(self) -> str:
        return "<LiteralExpr {} ({})>".format(
            self.template,
            self.static_type.name.camel if self.static_type else "<no type>",
        )


class BindableLiteralExpr(LiteralExpr):
    """
    Expression for literals that can be expressed in all bindings.
    """

    @abc.abstractmethod
    def render_private_ada_constant(self) -> str:
        """
        Assuming this expression is a valid constant, return Ada code to
        materialize it in the private API ($.Implementation).
        """
        ...

    @abc.abstractmethod
    def render_public_ada_constant(self) -> str:
        """
        Assuming this expression is a valid constant, return Ada code to
        materialize it in the public API ($.Analysis).
        """
        ...

    @abc.abstractmethod
    def render_python_constant(self) -> str:
        """
        Assuming this expression is a valid constant, return Python code to
        materialize it in the generated binding.
        """
        ...

    @abc.abstractmethod
    def render_java_constant(self) -> str:
        """
        Assuming this expression is a valid constant, return Java code to
        materialize it in the generated binding.
        """
        ...

    @abc.abstractmethod
    def render_introspection_constant(self) -> str:
        """
        Assuming this expression is a valid constant, return Ada code to
        materialize it in the introspection API.
        """
        ...

    @abc.abstractmethod
    def render_ocaml_constant(self) -> str:
        """
        Assuming this expression is a valid constant, return ocaml code to
        materialize it in the generated binding.
        """
        ...


class BooleanLiteralExpr(BindableLiteralExpr):

    def __init__(self, debug_info: ExprDebugInfo | None, value: bool):
        self.value = value
        super().__init__(debug_info, str(value), T.Bool)

    def render_private_ada_constant(self) -> str:
        return str(self.value)

    def render_public_ada_constant(self) -> str:
        return str(self.value)

    def render_python_constant(self) -> str:
        return str(self.value)

    def render_java_constant(self) -> str:
        return str(self.value).lower()

    def render_ocaml_constant(self) -> str:
        return str(self.value).lower()

    def render_introspection_constant(self) -> str:
        return "Create_Boolean ({})".format(self.value)


class IntegerLiteralExpr(BindableLiteralExpr):

    def __init__(self, debug_info: ExprDebugInfo | None, value: int):
        self.value = value
        super().__init__(debug_info, str(value), T.Int)

    def render_private_ada_constant(self) -> str:
        return str(self.value)

    def render_public_ada_constant(self) -> str:
        return str(self.value)

    def render_python_constant(self) -> str:
        return str(self.value)

    def render_java_constant(self) -> str:
        return str(self.value)

    def render_ocaml_constant(self) -> str:
        return str(self.value)

    def render_introspection_constant(self) -> str:
        return "Create_Integer ({})".format(self.value)


class CharacterLiteralExpr(BindableLiteralExpr):

    def __init__(self, debug_info: ExprDebugInfo | None, value: str):
        self.value = value
        assert len(self.value) == 1

        self.ada_value = "Character_Type'Val ({})".format(ord(self.value))

        super().__init__(debug_info, self.ada_value, T.Char)

    def render_private_ada_constant(self) -> str:
        return self.ada_value

    def render_public_ada_constant(self) -> str:
        return self.ada_value

    def render_python_constant(self) -> str:
        # Stick to ASCII in generated sources, so that Python2 interpreters do
        # not emit warnings when processing the generated Python code.
        char = self.value
        num = ord(char)

        # Escape metacharacters
        if char in ("'", "\\"):
            char = "\\" + char

        # Forward other printable ASCII codepoints as-is
        elif 32 <= num <= 127:
            pass

        # Use the appropriate escape sequence otherwise
        elif num < 2**8:
            char = "\\x{:02x}".format(num)
        elif num < 2**16:
            char = "\\u{:04x}".format(num)
        else:
            char = "\\U{:08x}".format(num)

        return "'{}'".format(char)

    def render_java_constant(self) -> str:
        return f"new Char({format(ord(self.value))})"

    def render_ocaml_constant(self) -> str:
        # In OCaml bindings, a character is represented as a utf-8 string, not
        # as char since OCaml char cannot represent unicode characters.
        return "Character.chr {}".format(ord(self.value))

    def render_introspection_constant(self) -> str:
        return "Create_Character ({})".format(self.ada_value)


class EnumLiteralExpr(BindableLiteralExpr):

    def __init__(self, debug_info: ExprDebugInfo | None, value: EnumValue):
        self.value = value
        super().__init__(
            debug_info, self.render_private_ada_constant(), self.value.type
        )

    def render_private_ada_constant(self) -> str:
        return self.value.ada_name

    def render_public_ada_constant(self) -> str:
        return self.value.ada_name

    def render_python_constant(self) -> str:
        return "{}.{}".format(self.value.type.py_helper, self.value.name.lower)

    def render_java_constant(self) -> str:
        return "{}.{}".format(self.type.api_name.camel, self.value.name.upper)

    def render_ocaml_constant(self) -> str:
        ocaml_api = get_context().ocaml_api_settings
        return "{}.{}".format(
            ocaml_api.module_name(self.type), self.value.name.camel
        )

    def render_introspection_constant(self) -> str:
        return "Create_{} ({})".format(
            self.type.api_name, self.render_private_ada_constant()
        )


class NullExpr(BindableLiteralExpr):
    """
    Expression for the null expression corresponding to some type.
    """

    def __init__(self, debug_info: ExprDebugInfo | None, type: CompiledType):
        super().__init__(debug_info, type.nullexpr, type)

    def render_private_ada_constant(self) -> str:
        return self._render_expr()

    def render_public_ada_constant(self) -> str:
        # First, handle all types that 1) have different types in the public
        # and internal Ada APIs and that 2) can have default values.
        if self.type.is_entity_type:
            return "No_{}".format(self.type.api_name.camel_with_underscores)

        # For all other cases, make sure that the internal type is the one
        # exposed in the public Ada API.
        else:
            assert (
                self.type.api_name == self.type.name
            ), "Cannot generate a public Ada constant for type {}".format(
                self.type.lkt_name
            )
            return self._render_expr()

    def render_python_constant(self) -> str:
        return "None" if self.type.is_entity_type else self.type.py_nullexpr

    def render_java_constant(self) -> str:
        t: ASTNodeType
        if isinstance(self.type, ASTNodeType):
            t = self.type
        elif isinstance(self.type, EntityType):
            assert isinstance(self.type.element_type, ASTNodeType)
            t = self.type.element_type
        else:
            raise AssertionError(
                f"cannot generate Java null constant for {t.lkt_name}"
            )
        return f"{t.kwless_raw_name.camel}.NONE"

    def render_ocaml_constant(self) -> str:
        return "None"

    def render_introspection_constant(self) -> str:
        # Create_Node takes the internal root entity type
        return "Create_Node ({})".format(T.root_node.entity.nullexpr)


class InitializationStateLiteralExpr(BindableLiteralExpr):
    """
    Expression for ``InitializationState`` literals.

    Note that this DSL construct is meant to be internal, to provide default
    values for lazy field initialization state fields.
    """

    def __init__(self, debug_info: ExprDebugInfo | None, value: str):
        """
        :param value: Corresponding ``Initialization_State`` Ada literal.
        """
        self.value = value
        super().__init__(debug_info, value, T.InitializationState)

    def render_private_ada_constant(self) -> str:
        return self.value

    # InitializationState is not exposed in public APIs, so there is no
    # need to actually implement the render_* methods for them.

    def render_public_ada_constant(self) -> str:
        raise AssertionError

    def render_python_constant(self) -> str:
        raise AssertionError

    def render_java_constant(self) -> str:
        raise AssertionError

    def render_introspection_constant(self) -> str:
        raise AssertionError

    def render_ocaml_constant(self) -> str:
        raise AssertionError


class UncheckedCastExpr(Expr):
    """
    Expression for unchecked casts.

    These casts will not raise a Property_Error if they fail. We must use them
    in code generation only when we know they cannot fail.
    """

    def __init__(self, expr: Expr, dest_type: CompiledType):
        self.expr = expr
        self.dest_type = dest_type
        self.static_type = dest_type
        super().__init__(debug_info=None)

    def _render_pre(self) -> str:
        return self.expr.render_pre()

    def _render_expr(self) -> str:
        if self.dest_type.is_ast_node:
            # All node values are subtypes of the same access, so no explicit
            # conversion needed in the generated Ada code.
            return self.expr.render_expr()
        return "{} ({})".format(self.dest_type.name, self.expr.render_expr())

    @property
    def subexprs(self) -> dict:
        return {"0-type": self.dest_type, "1-expr": self.expr}

    def __repr__(self) -> str:
        return "<UncheckedCastExpr {}>".format(
            self.dest_type.name.camel_with_underscores
        )


class ComputingExpr(Expr):
    """
    Base class for expressions that do computations.

    These expressions are the only ones visible for GDB helpers. As such, they
    are required to store their result into a result variable, and thus
    subclasses only need to override the "_render_pre" method, which is
    supposed to initialize the result variable with the expression evaluation.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        result_var_name: str | names.Name | None = None,
    ):
        super().__init__(debug_info, result_var_name)

    # ComputingExpr always define a result variable: override result_var so
    # that typing can assume that is the case.

    @property
    def result_var(self) -> LocalVars.LocalVar:
        result = super().result_var
        assert result is not None
        return result

    def _render_expr(self) -> str:
        return self.result_var.codegen_name.camel_with_underscores


class SavedExpr(Expr):
    """
    Wrapper expression that will make sure we have a result variable for the
    input expression. This makes it easier to re-use the result of an
    expression multiple times, as we forbid tree sharing.

    If the input expression has no result variable, we create one for it,
    otherwise we re-use it.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        result_var_name: str | names.Name | None,
        expr: Expr,
    ):
        self.expr = expr
        self.static_type = expr.type

        if expr.result_var:
            self.exposed_result_var = expr.result_var
            result_var_name = None

        super().__init__(debug_info, result_var_name, skippable_refcount=True)

        if result_var_name:
            assert self._result_var
            self.exposed_result_var = self._result_var

    @property
    def result_var(self) -> LocalVars.LocalVar:
        """
        Return the LocalVar instance corresponding to the result of this
        expression.
        """
        return self.exposed_result_var

    @property
    def result_var_expr(self) -> VariableExpr:
        """
        Return a reference to the variable that contains the result of this
        expression.
        """
        return self.result_var.ref_expr

    def _render_pre(self) -> str:
        result = [self.expr.render_pre()]
        if self._result_var:
            result.append(
                assign_var(self._result_var.ref_expr, self.expr.render_expr())
            )
        return "\n".join(result)

    def _render_expr(self) -> str:
        return self.exposed_result_var.codegen_name.camel_with_underscores

    @property
    def subexprs(self) -> dict:
        return {"expr": self.expr}

    def __repr__(self) -> str:
        return "<SavedExpr>"


class SequenceExpr(Expr):
    """
    Expression to evaluate a first expression, then a second one.

    The result of this compound expression is the result of the second one.
    This makes it easier to express computations where an expression needs to
    be repeated multiple times later on (as we forbid tree sharing).
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        pre_expr: Expr,
        post_expr: Expr,
    ):
        """
        This expression will evaluate `pre_expr`, then `post_expr`, and will
        then return the result of `post_expr`.
        """
        self.pre_expr = pre_expr
        self.post_expr = post_expr
        self.static_type = post_expr.type

        # This expression completely delegates the work of managing the result
        # value to `post_expr`, so we can safely avoid all ref-counting
        # activity here.
        super().__init__(debug_info, skippable_refcount=True)

    def _render_pre(self) -> str:
        return "{}\n{}".format(
            self.pre_expr.render_pre(), self.post_expr.render_pre()
        )

    def _render_expr(self) -> str:
        return self.post_expr.render_expr()

    @property
    def subexprs(self) -> dict:
        return {"0-pre": self.pre_expr, "1-post": self.post_expr}

    def __repr__(self) -> str:
        return "<SequenceExpr>"

    class _ForwardExpr(Expr):
        def __init__(self, dest_var: VariableExpr, expr: Expr):
            self.dest_var = dest_var
            self.expr = expr
            self.static_type = dest_var.type
            super().__init__(None)

        def _render_pre(self) -> str:
            result = [self.expr.render_pre()]

            # If the destination variable comes from the sources, emit debug
            # info for it: the end of our inner expression is its definition
            # point.
            if self.dest_var.source_name:
                result.append(gdb_bind_var(self.dest_var))

            result.append(assign_var(self.dest_var, self.expr.render_expr()))
            return "\n".join(result)

        def _render_expr(self) -> str:
            return self.dest_var.render_expr()

        @property
        def subexprs(self) -> dict:
            return {"0-var": self.dest_var, "1-expr": self.expr}

        def __repr__(self) -> str:
            return "<ForwardExpr {}>".format(self.dest_var)

    @classmethod
    def make_forward(
        cls,
        debug_info: ExprDebugInfo | None,
        dest_var: VariableExpr,
        pre_expr: Expr,
        post_expr: Expr,
    ) -> Expr:
        """
        Create a sequence expression that:

          * evaluates `pre_expr`;
          * forward its value to `dest_var`;
          * evaluates `post_expr` and return its value.

        :param dest_var: Variable to forward `pre_expr` to.
        :param pre_expr: First expression to evaluate.
        :param post_expr: Second expression to evaluate.
        """
        assert pre_expr.type.matches(dest_var.type)

        return cls(debug_info, cls._ForwardExpr(dest_var, pre_expr), post_expr)


class DynamicVariable:
    """
    Declaration for a dynamic variable.
    """

    @dataclasses.dataclass(frozen=True)
    class Argument:
        """
        Declaration of a dynamic variable as a property argument.
        """

        location: Location
        """
        Location where the dynamic variable is declared as a property argument.
        """

        dynvar: DynamicVariable
        """
        Dynamic variable to use as a property argument.
        """

        local_var: LocalVars.LocalVar
        """
        Local variable that materializes the dynamic variable binding made
        during the property call.
        """

        default_value: BindableLiteralExpr | None
        """
        Default value that is bound to this dynamic variable when calling the
        property, if there is one.
        """

    class BindingToken:
        """
        Dummy class used in ``DynamicVariable.push_binding`` and
        ``DynamicVariable.pop_binding``.
        """

    def __init__(
        self,
        location: Location,
        name: str,
        type: CompiledType | None = None,
        doc: str | None = None,
    ):
        """
        Create a dynamic variable.

        These are implemented as optional arguments in properties.

        :param name: Name for this dynamic variable as defined in the language
            spec (i.e. lower case).
        :param type: Variable type, if known at this point.
        :param doc: User documentation for this variable.
        """
        self.location = location
        self.name = names.Name.from_lower(name)
        if type is not None:
            self.type = type
        self.doc = doc

        self.bindings: list[
            tuple[LocalVars.LocalVar, DynamicVariable.BindingToken]
        ] = []
        """
        Stack of current bindings for this dynamic variable.

        Each time we compile a dynvar binding expression, a
        ``LocalVars.LocalVar`` instance is temporarily pushed: the currently
        active binding is always at the top of this stack.
        """

    @property
    def lkt_name(self) -> str:
        """
        Name of the dynamic variable as it appears in Lkt sources. To be used
        in diagnostics.
        """
        return self.name.lower

    def push_binding(
        self,
        var: LocalVars.LocalVar,
    ) -> DynamicVariable.BindingToken:
        """
        Set ``var`` as the current binding for this dynamic variable.

        Return a token that is needed to unset this binding.
        """
        token = self.BindingToken()
        self.bindings.append((var, token))
        return token

    def pop_binding(self, token: DynamicVariable.BindingToken) -> None:
        """
        Unset the current binding for this dynamic variable. ``token`` is used
        to ensure that this correspond to the expected ``push_binding`` method
        invocation.
        """
        _, top_token = self.bindings[-1]
        assert token == top_token
        self.bindings.pop()

    @contextmanager
    def bind(self, var: LocalVars.LocalVar) -> Iterator[None]:
        """
        Convenience wrapper around the ``push_binding`` and ``pop_binding``
        methods to invoke them at a context manager's boundaries.
        """
        token = self.push_binding(var)
        yield
        self.pop_binding(token)

    @property
    def is_bound(self) -> bool:
        """
        Return whether this dynamic variable is currently bound.
        """
        return bool(self.bindings)

    @property
    def current_binding(self) -> LocalVars.LocalVar:
        """
        Assuming that this dynamic variable is currently bound, return the
        abstract variable that contains its value.
        """
        assert self.bindings
        top_var, _ = self.bindings[-1]
        return top_var

    def __repr__(self) -> str:
        return (
            f"<DynamicVariable {self.name.lower} at"
            f" {self.location.gnu_style_repr(relative=True)}>"
        )

    @staticmethod
    def check_call_bindings(
        error_location: Location | L.LktNode,
        prop: PropertyDef,
        context_msg: str = "",
    ) -> None:
        """
        Ensure all need dynamic vars are bound for a call to ``prop``.

        This emits an error diagnostic if there is at least one dynamic
        variable in ``prop`` that is not currently bound *and* that has no
        default value.

        :param error_location: Location for the error diagnostic.
        :param prop: Property "to call".
        :param context_msg: format string to describe how this property is
            used. This helps formatting the error message. It is formatted with
            "prop", being the name of the property. For instance:

                "In call to {prop}".
        """
        unbound_dynvars = [
            dv_arg.dynvar
            for dv_arg in prop.dynamic_var_args
            if (not dv_arg.dynvar.is_bound and dv_arg.default_value is None)
        ]
        prefix = (
            "{}, some".format(context_msg.format(prop=prop.qualname))
            if context_msg
            else "Some"
        )
        check_source_language(
            not unbound_dynvars,
            "{} dynamic variables need to be bound: {}".format(
                prefix,
                ", ".join(dynvar.lkt_name for dynvar in unbound_dynvars),
            ),
            location=error_location,
        )


class DynamicVariableBindExpr(ComputingExpr):

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        dynvar: DynamicVariable,
        value_var: LocalVars.LocalVar,
        value: Expr,
        to_eval_expr: Expr,
    ):
        self.dynvar = dynvar
        self.value_var = value_var
        self.value = value
        self.to_eval_expr = to_eval_expr
        self.static_type = self.to_eval_expr.type

        super().__init__(debug_info, "Dyn_Var_Bind_Result")

    def _render_pre(self) -> str:
        return "\n".join(
            [
                # First, compute the value to bind
                self.value.render_pre(),
                assign_var(self.value_var.ref_expr, self.value.render_expr()),
                # Then we can compute the nested expression with the bound
                # variable.
                self.to_eval_expr.render_pre(),
                assign_var(
                    self.result_var.ref_expr, self.to_eval_expr.render_expr()
                ),
            ]
        )

    @property
    def subexprs(self) -> dict:
        return {
            "var": self.dynvar,
            "value": self.value,
            "expr": self.to_eval_expr,
        }

    def __repr__(self) -> str:
        return "<DynamicVariableBindExpr>"


def make_node_to_symbol(debug_info: ExprDebugInfo | None, node: Expr) -> Expr:
    return CallExpr(debug_info, "Sym", "Get_Symbol", T.Symbol, [node])


class SymbolLiteralExpr(ComputingExpr):
    """
    Expression that returns a symbol from a string literal.
    """

    def __init__(self, debug_info: ExprDebugInfo | None, name: str):
        self.static_type = T.Symbol
        self.name = name
        get_context().add_symbol_literal(self.name)

        super().__init__(debug_info, "Sym")

    def _render_pre(self) -> str:
        return assign_var(
            self.result_var,
            "Precomputed_Symbol"
            " (Precomputed_Symbol_Table (Self.Unit.Context.Symbols)"
            ", {})".format(get_context().symbol_literals[self.name]),
        )

    @property
    def subexprs(self) -> dict:
        return {"name": self.name}


class BindingScope(ComputingExpr):
    """
    Expression that materializes new bindings.

    This expression is just an annotation: it is useless from a code generation
    point of view. It makes it possible to describe the creation of new
    bindings for some scope.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        expr: Expr,
        bindings: list[VariableExpr],
        scope: LocalVars.Scope | None = None,
    ):
        """
        :param scope: If provided, this BindingScope instance will materialize
            scope entry/finalization in the generated code.
        """
        self.expr = expr
        self.expr_bindings = bindings
        self.static_type = self.expr.type
        self.scope = scope

        # Create a local variable that belong to the outer scope so that at
        # finalization time, our result is still live.
        super().__init__(debug_info, "Scope_Result")

    def _render_pre(self) -> str:
        return render("properties/binding_scope", expr=self)

    @property
    def subexprs(self) -> dict:
        return {"0-bindings": self.expr_bindings, "1-expr": self.expr}

    def _bindings(self) -> list[VariableExpr]:
        return self.expr_bindings

    def __repr__(self) -> str:
        return "<BindingScope ({}): {}>".format(
            ", ".join(repr(b) for b in self.expr_bindings), repr(self.expr)
        )


class LetExpr(ComputingExpr):
    """
    Define bindings in order to evaluate an expression.

    `lambda_fn` is a lambda function that takes one argument per binding. Each
    argument must have a default value that is the expression to compute the
    value associated to the binding. The body for this lambda function is the
    expression to evaluate with the bindings and computes the result of this
    Let expression.

    This is similar to the ``let ... in ...`` constructs in traditional
    functional languages. For instance::

        Let(lambda collection=node.some_property:
            If(collection.length > 0,
               collection.at(0),
               node))
    """

    pretty_class_name = "Let"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        variables: list[tuple[VariableExpr, Expr]],
        expr: Expr,
    ):
        self.variables = variables
        self.expr = expr
        self.static_type = self.expr.type

        # This expression does not create itself the result value: expr does.
        # Hence, relying on expr's result variable to make sure there is no
        # ref-counting issue is fine.
        super().__init__(debug_info, "Let_Result")

    def _render_pre(self) -> str:
        # Start and end a debug info scope around the whole expression so that
        # the bindings we create in this Let expression die when leaving its
        # evaluation in a debugger.
        result = [gdb_helper("scope-start")]

        for var, expr in self.variables:
            result += [
                expr.render_pre(),
                assign_var(var, expr.render_expr()),
                gdb_bind_var(var),
            ]

        result += [
            self.expr.render_pre(),
            assign_var(self.result_var.ref_expr, self.expr.render_expr()),
            gdb_helper("end"),
        ]
        return "\n".join(result)

    @property
    def subexprs(self) -> dict:
        return {
            "vars": {v.name: e for v, e in self.variables},
            "expr": self.expr,
        }

    def _bindings(self) -> list[VariableExpr]:
        return [v for v, _ in self.variables]

    def __repr__(self) -> str:
        return "<Let.Expr (vars: {})>".format(
            ", ".join(v.name.lower for v, _ in self.variables)
        )


class TryExpr(ComputingExpr):
    """
    ``Try`` tries to evaluate the given primary expression. If it raises a
    PropertyError, then either the fallback expression will be evaluated,
    either the Try expression will return the null value for the type of the
    primary expression.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        try_expr: Expr,
        else_expr: Expr,
    ):
        """
        :param try_expr: The expression that may raise.
        :param else_expr: If "try_expr" raises a property, this fallback
            expression is evaluated.
        """
        self.try_expr = try_expr
        self.else_expr = else_expr
        self.static_type = try_expr.type

        super().__init__(debug_info, "Try_Result")

    def _render_pre(self) -> str:
        return render("properties/try_ada", expr=self)

    @property
    def subexprs(self) -> dict:
        return {"0-try": self.try_expr, "1-else": self.else_expr}

    def __repr__(self) -> str:
        return "<Try.Expr>"


class ArrayLiteral:
    """
    Return an array literal that contains `elements`, a list of expressions for
    array components.

    If `element_type` is provided, the type of all components is checked
    against it, otherwise it is inferred from sub-expressions. Because of this,
    `element_type` is mandatory when `elements` is empty.
    """

    @staticmethod
    def construct_static(
        debug_info: ExprDebugInfo | None,
        elements: list[Expr],
        array_type: ArrayType,
    ) -> Expr:
        if len(elements) == 0:
            return CallExpr(
                debug_info,
                "Array_Lit",
                array_type.constructor_name,
                array_type,
                ["Items_Count => 0"],
            )
        else:
            return CallExpr(
                debug_info,
                "Array_Lit",
                array_type.constructor_name,
                array_type,
                [
                    aggregate_expr(
                        array_type,
                        [(str(i), el) for i, el in enumerate(elements, 1)],
                    )
                ],
            )


def gdb_loc(loc: Location | None = None) -> str:
    """
    Return the GDB encoding for a language spec location.
    """
    if loc is None:
        return "None"

    return f"{os.path.basename(loc.file)}:{loc.line}"


def gdb_property_start(prop: PropertyDef) -> str:
    if prop.is_dispatcher:
        return gdb_helper("property-start", prop.debug_name, "dispatcher")
    else:
        return gdb_helper(
            "property-start", prop.debug_name, gdb_loc(prop.location)
        )


def gdb_property_body_start() -> str:
    return gdb_helper("property-body-start")


def gdb_memoization_lookup() -> str:
    return gdb_helper("memoization-lookup")


def gdb_memoization_return() -> str:
    return gdb_helper("memoization-return")


def gdb_scope_start() -> str:
    return gdb_helper("scope-start")


def gdb_property_call_start(prop: PropertyDef) -> str:
    return gdb_helper("property-call-start", prop.debug_name)


def gdb_end() -> str:
    return gdb_helper("end")


def gdb_bind(lkt_name: str, var_name: str) -> str:
    return gdb_helper("bind", lkt_name, var_name)


def gdb_bind_var(var: VariableExpr) -> str:
    """
    Output a GDB helper directive to bind a variable. This does nothing if the
    variable has no source name.
    """
    return (
        gdb_bind(var.source_name, var.name.camel_with_underscores)
        if var.source_name
        else ""
    )


def render(*args: _Any, **kwargs: _Any) -> str:
    return get_context().render_template(
        *args,
        property=PropertyDef.get(),
        assign_var=assign_var,
        gdb_property_start=gdb_property_start,
        gdb_property_body_start=gdb_property_body_start,
        gdb_property_call_start=gdb_property_call_start,
        gdb_memoization_lookup=gdb_memoization_lookup,
        gdb_memoization_return=gdb_memoization_return,
        gdb_scope_start=gdb_scope_start,
        gdb_end=gdb_end,
        gdb_bind=gdb_bind,
        gdb_bind_var=gdb_bind_var,
        **kwargs,
    )


@dataclasses.dataclass
class PropertyClosure:
    """
    Description of the object in generated code used to represent a partially
    evaluated property, currently used by predicate and propagate atoms in
    logic equations.
    """

    @dataclasses.dataclass(frozen=True)
    class PartialArgument:
        """
        Description of a logic predicate argument that is passed for property
        partial evaluation.
        """

        index: int
        """
        0-based index for the partial argument in the closure for this logic
        predicate.
        """

        name: names.Name
        """
        Name for the partial argument in the list of arguments in the property.
        """

        type: CompiledType
        """
        Type for the partial argument.
        """

    id: str
    """
    Identifier for the logic predicate.
    """

    partial_args: tuple[PartialArgument, ...]
    """
    Arguments passed to the property for partial evaluation.
    """

    default_passed_args: int
    """
    Number of arguments passed by default value.
    """


class PropertyDef(AbstractNodeData):
    """
    This is the underlying class that is used to represent properties in the
    DSL. You are not supposed to use it directly, but instead use one of
    Property/AbstractProperty proxy constructors that will ensure the
    consistency of the passed arguments.
    """

    _current_property: ClassVar[PropertyDef | None] = None
    """
    Property that is currently being compiled (see ``PropertyDef.bind``).
    """

    # Overridings for AbstractNodeData class attributes
    is_property = True

    kind_name = "property"

    # Reserved names for arguments in generated subprograms
    self_arg_name = names.Name("Node")
    env_arg_name = names.Name("Bound_Env")
    env_rebinding_name = names.Name("Envs_Rebindings")

    # Collections for these
    reserved_arg_names = (self_arg_name, env_arg_name)
    reserved_arg_lower_names = [n.lower for n in reserved_arg_names]

    dynamic_var_args: list[DynamicVariable.Argument]
    """
    List of dynamic variables declared as arguments for this property.

    This attribute is set either in the ``PropertyDef`` constructor (when the
    ``dynamic_vars`` argument is not None) or when the ``set_dynamic_var_args``
    is called.
    """

    prop_decl: str
    """
    The emitted code for this property declaration.
    """

    prop_def: str
    """
    The emitted code for this property definition.
    """

    _requires_untyped_wrapper: bool
    _uses_envs: bool | None
    _warn_on_unused: bool | None
    activate_tracing: bool
    artificial: bool
    call_memoizable: bool
    called_by_super: bool
    dispatch_table: list[tuple[ASTNodeType, PropertyDef]]
    memoized: bool

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        location: Location,
        type: CompiledType,
        expr: Expr | None = None,
        doc: str | None = None,
        public: bool | None = None,
        abstract: bool = False,
        arguments: list[Argument] | None = None,
        dynamic_vars: list[DynamicVariable.Argument] | None = None,
        memoized: bool = False,
        call_memoizable: bool = False,
        memoize_in_populate: bool = False,
        external: bool = False,
        uses_entity_info: bool | None = None,
        uses_envs: bool | None = None,
        optional_entity_info: bool = False,
        warn_on_unused: bool | None = None,
        call_non_memoizable_because: str | None = None,
        activate_tracing: bool = False,
        dump_ir: bool = False,
        lazy_field: bool | None = None,
        artificial: bool = False,
        access_constructor: (
            Callable[
                [
                    ExprDebugInfo | None,
                    Expr,
                    AbstractNodeData,
                    list[Expr | None],
                ],
                Expr,
            ]
            | None
        ) = None,
        local_vars: LocalVars | None = None,
        final: bool = False,
        has_property_syntax: bool = False,
        implements: Callable[[], InterfaceMethodProfile] | None = None,
    ):
        """
        :param owner: Compiled type that owns this member.
        :param names: Names for this member.
        :param location: Source location of the declaration for this property.
        :param type: Return type for this property.

        :param expr: The expression for the property, if available.

        :param str|None doc: User documentation for this property.
        :param bool|None public: See AbstractNodeData's constructor.
        :param bool abstract: Whether this property is abstract or not. If this
            is True, then expr can be None.

        :param arguments: List of natural arguments for this property, if known
            when creating this property.

        :param type: The optional type annotation for this property. If
            supplied, it will be used to check the validity of inferred types
            for this propery, and eventually for overriding properties in sub
            classes. NOTE: The type is mandatory for abstract base properties
            and for properties that take parameters. If the type itself is not
            available when creating the property, a lambda function that
            returns it is available.
        :type type: CompiledType|langkit.compiled_types.TypeRepo.Defer|None

        :param dynamic_vars: List of dynamic variables that must be bound
            before calling this property, i.e. used as arguments in this
            property.

            If left to None, inherit from the overriden property, or the empty
            list if these is no property to override. Just like `public`, it
            must always be consistent with base classes.

        :param bool memoized: Whether this property must be memoized. Disabled
            by default.

        :param bool call_memoizable: If true, allow memoization for this
            property or its callers even when it is unsafe to do so, for
            instance when using equation resolution constructs, which are
            memoization-unfriendly as they use side-effects. This should be
            used when the side-effect is contained inside the call to this
            property (i.e. when the property is pure from the point of view of
            callers).

        :param bool memoize_in_populate: Whether to memoize the property during
            the populate lexical environment pass. It is disabled by default as
            the hash of lexical environments changes during this pass.

        :param bool external: Whether this property's implementation is
            provided by the language specification. If true, `expr` must be
            None and the implementation must be provided in the
            extensions/nodes/{node_name}/bodies extension file. Note that the
            engines always generate the public declaration part.

        :param bool uses_entity_info: Whether this property requires entity
            information to be passed for Self. If left to None, this will be
            computed using the properties call graph. If false, uses of entity
            info will be rejected. Note that this must be non-None for external
            property, as they escape call graph analysis.

        :param bool|None uses_envs: Whether this property makes a lexical
            environment lookup, or calls a property that does one
            (transitively). If left to None, this will be computed using the
            properties call graph. If false, lookups will be rejected. Note
            that this must be non-None for external property, as they escape
            call graph analysis.

        :param bool optional_entity_info: If `uses_entity_info` is True,
            whether the entity info is optional. This allows properties to be
            called on 1) bare AST nodes, in which case the default entity info
            is passed, and 2) on entities, in which case the entity info from
            the prefix is passed.

        :param bool|None warn_on_unused: Whether to warn on unused or not.
            Defaults to None, which means "unspecified by the user".

        :param bool artificial: Whether this property is artificial: not
            created in the language spec, but still visible for users (unlike
            internal properties).

        :param str|None call_non_memoizable_because: If not-None, makes the use
            of this property in a memoized context impossible. Must be used for
            external properties that do side effects (such as loading an
            analysis unit), as this conflicts with the memoization machinery.

        :param bool activate_tracing: Whether we want to activate tracing for
            this property's execution.

        :param bool dump_ir: If true, dump the body expression tree for this
            property.

        :param lazy_field: Whether the goal of this property is to initialize a
            lazy field. If None, inherit this status from the root property, or
            default to False if this is the root property.

        :param access_constructor: See AbstractNodeData's constructor.

        :param local_vars: Local variables for this property. Start from
            scratch if left to None.

        :param final: If True, this property cannot be overriden. This is
            possible only for concrete properties.

        :param has_property_syntax: Whether calls to this property are expected
            to use the field access syntax (i.e. no argument list).

        :param implements: If provided, callback that returns the generic
            interface method that this member implements.
        """
        # TODO: fix type for public below
        super().__init__(
            owner=owner,
            names=names,
            location=location,
            type=type,
            public=public,  # type: ignore
            abstract=abstract,
            access_constructor=access_constructor,
            final=final,
            implements=implements,
        )

        self._original_is_public: bool
        """
        Original privacy for this property. Can be different from `is_public`
        after properties expansion. Computed right after `is_public` itself in
        the `prepare` pass.
        """

        self.is_dispatcher = False
        """
        Whether this property is just a wrapper that, based on the kind of
        Self, dispatches to specific properties.

        :type: bool
        """

        self.is_artificial_dispatcher = False
        """
        Whether this property is a dispatcher, and that can be considered as
        artificial, i.e. not coming from the sources. The only dispatchers that
        come from sources are property roots that are abstract with no runtime
        check.
        """

        self.is_dispatching_root = False
        """
        Whether this property is the "root static" property after dispatcher
        lowering, i.e. whether it is the property implementation for the node
        that is the root in that property hierarchy.
        """

        self.dispatcher: PropertyDef | None = None
        """
        After property dispatch lowering, this holds a reference to the
        dispatcher that covers ``self``, if ``self`` is part of a property
        dispatching tree.
        """

        self.codegen_name_before_dispatcher: str
        """
        For dispatcher properties, name of the property for code generation
        before it has been re-purposed as a dispatcher (see the
        lower_properties_dispatching pass).
        """

        self.logic_predicates: list[PropertyClosure] = []
        """
        The list of logic predicates to generate.
        """

        self.logic_functors: list[PropertyClosure] = []
        """
        The list of logic functors to generate.
        """

        self.expr = expr

        self.vars = local_vars or LocalVars()

        assert not self.final or not self.abstract

        # If the list of arguments is known, register the arguments
        if arguments:
            for arg in arguments:
                self.append_argument(arg)

        # Likewise for dynamic variables
        self.dynamic_var_args_known = False
        if dynamic_vars is not None:
            self.set_dynamic_var_args(dynamic_vars)

        self._raw_doc: str | None = doc
        self._doc: str | None = doc
        self._doc_location: Location | None = None

        self.memoized = memoized
        self.call_memoizable = call_memoizable
        self.memoize_in_populate = memoize_in_populate

        self.external = external
        self.artificial = artificial

        self.user_external: bool = external and not self.artificial
        """
        Whether this property is external and comes from the DSL. In that case,
        code generation expects its implementation to be in the
        $.Implementation.Extensions unit.
        """

        self._uses_entity_info = uses_entity_info
        self._uses_envs = uses_envs

        self.optional_entity_info = optional_entity_info

        self._requires_untyped_wrapper = False
        self._warn_on_unused = warn_on_unused

        self._call_non_memoizable_because = call_non_memoizable_because

        self._solves_equation = False
        """
        Whether this property uses the ".solve" operation on a logic equation.
        """

        self._gets_logic_var_value = False
        """
        Whether this property uses the ".get_value" operation on a logic
        variable.
        """

        self.activate_tracing = activate_tracing
        self.dump_ir = dump_ir
        self._lazy_field = lazy_field

        self.lazy_state_field: UserField | None = None
        """
        If ``self`` is a lazy field, this is an enum field that tracks
        whether ``self`` was successfully evaluated (or if its initialization
        raised an exception), and thus whether ``lazy_storage_field`` is
        initialized.
        """

        self.lazy_storage_field: UserField | None = None
        """
        If ``self`` is a lazy field, this is the field that stores the result
        of its evaluation.
        """

        self._is_reachable: bool | None = None

        self.called_by_super = False
        """
        Whether this specific property is the target of a Super() call.
        Tracking this matters for unreachable base properties analysis.
        """

        self.predicate_error: PredicateErrorDiagnosticTemplate | None = None
        """
        If not None, the template error message to use when a logic predicate
        that uses this property fails at solve-time.
        """

        self.has_property_syntax = has_property_syntax

        # Create automatic "prefix arguments": node (for all node properties)
        # and self (if entity info is not explicitly disabled for this
        # property, or for non-node properties).
        self.prefix_var: LocalVars.LocalVar

        self.has_node_var = False
        self.node_var: LocalVars.LocalVar

        self.has_self_var = False
        self.self_var: LocalVars.LocalVar

        def create_auto_var(
            codegen_name: str,
            type: CompiledType,
        ) -> LocalVars.LocalVar:
            return self.vars.create(
                location=Location.builtin,
                codegen_name=codegen_name,
                type=type,
                manual_decl=True,
                scope=self.vars.root_scope,
            )

        if isinstance(self.owner, ASTNodeType):
            self.node_var = create_auto_var("Self", self.owner)
            self.has_node_var = True
            if self._uses_entity_info in (None, True):
                self.self_var = create_auto_var("Ent", self.owner.entity)
                self.has_self_var = True
            self.prefix_var = self.node_var
        else:
            self.self_var = create_auto_var("Self", self.owner)
            self.has_self_var = True
            self.prefix_var = self.self_var

    @property
    def node_owner(self) -> ASTNodeType:
        """
        Shortcut to get the owner, asserting that it's a node type.
        """
        assert isinstance(self.owner, ASTNodeType)
        return self.owner

    @property
    def debug_name(self) -> str:
        """
        Return the name for this property to use in debug info.
        """
        return (
            "[dispatcher]{}".format(self.qualname)
            if self.is_dispatcher
            else self.qualname
        )

    @property
    def warn_on_unused(self) -> bool:
        if self._warn_on_unused is not None:
            return self._warn_on_unused
        else:
            return self.base is None or self.base.warn_on_unused

    @property
    def dispatching(self) -> bool:
        """
        Whether this property is dispatching or not.  This is True as soon as
        the property is abstract or the property is overriden in AST node
        subclasses or the property overrides another one.

        This is inferred during the "compute" pass.
        """
        return bool(self.abstract or self.base or self.overridings)

    @property
    def uid(self) -> str:
        """
        Returns a string that uniquely identifies this property.
        """
        return str(self._serial)

    @classmethod
    def get(cls) -> PropertyDef:
        """
        Return the currently bound property. Used by the rendering context to
        get the current property.
        """
        result = cls.get_or_none()
        assert result is not None
        return result

    @classmethod
    def get_or_none(cls) -> PropertyDef | None:
        """
        Return the currently bound property, if there is one. Used by the
        rendering context to get the current property.
        """
        return cls._current_property

    @classmethod
    def get_scope(cls) -> LocalVars.Scope:
        """
        Return the current local variable scope for the currently bound
        property.
        """
        return cls.get().vars.current_scope

    @contextmanager
    def bind(self, bind_dynamic_vars: bool = False) -> Iterator[None]:
        """
        Set the current property to ``self`` as the property currently being
        compiled: ``Property.get()`` and ``Property.get_or_none()`` will return
        it during the lifetime of the returned context manager.

        :param bind_dynamic_vars: Whether to bind dynamic variables.
        """
        assert PropertyDef._current_property is None
        PropertyDef._current_property = self

        # If requested, provide default bindings for self's dynamically bound
        # variables. These binding just redirect to this property's
        # corresponding arguments.
        dynvar_binding_tokens: list[
            tuple[DynamicVariable, DynamicVariable.BindingToken]
        ] = []
        if bind_dynamic_vars:
            dynvar_binding_tokens += [
                (arg.dynvar, arg.dynvar.push_binding(arg.local_var))
                for arg in self.dynamic_var_args
            ]

        try:
            if self.vars is None:
                yield
            else:
                # Make sure temporaries are created in this property's root
                # scope or in its children.
                with self.vars.root_scope.use():
                    yield
        finally:
            for dynvar, token in reversed(dynvar_binding_tokens):
                dynvar.pop_binding(token)
            PropertyDef._current_property = None

    def set_dynamic_var_args(
        self,
        args: list[DynamicVariable.Argument],
    ) -> None:
        """
        Set dynamic variables that are used as arguments for this property.
        """
        assert not self.dynamic_var_args_known
        self.dynamic_var_args = args
        self.dynamic_var_args_known = True

        # Append artificial arguments for each dynamic variable
        for a in args:
            self.append_argument(
                Argument(
                    a.location,
                    a.dynvar.name,
                    a.dynvar.type,
                    is_artificial=True,
                    default_value=a.default_value,
                    local_var=a.local_var,
                )
            )

    def compute_property_attributes(self, context: CompileCtx) -> None:
        """
        Compute various property attributes, notably:
        * Information related to dispatching for properties.
        * Inheritance based information generally, like inheriting return
          type or privacy, consistency of annotations between base property
          and inherited properties.
        * Property overriding completeness checking.

        :type context: langkit.compile_context.CompileCtx
        """
        diag_ctx = DiagnosticContext(self.location)

        if self.abstract:
            # Look for concrete subclasses in self.owner which do not override
            # this property. Abstract nodes can keep inherited properties
            # abstract.
            concrete_types_not_overriding = []

            def find(node: ASTNodeType) -> None:
                # If node overrides this property, all is fine. Obviously, do
                # not check on the very node that defines the abstract
                # property.
                if node != self.owner:
                    for prop in node.get_properties(include_inherited=False):
                        if (
                            prop.names.index == self.names.index
                            and not prop.abstract
                        ):
                            return

                # Otherwise, if it is an abstract node, all is still find, but
                # we need to check its own subclasses...
                if node.abstract:
                    for subcls in node.subclasses:
                        find(subcls)

                # Otherwise, we have identified an illegal concrete subclass
                else:
                    concrete_types_not_overriding.append(node)

            find(assert_type(self.owner, ASTNodeType))
            diag_ctx.check_source_language(
                not concrete_types_not_overriding,
                "Abstract property {} is not overriden in all subclasses."
                " Missing overriding properties on classes: {}".format(
                    self.original_name,
                    ", ".join(
                        [t.lkt_name for t in concrete_types_not_overriding]
                    ),
                ),
            )

        if self.base:
            # Inherit the privacy level or check that it's consistent with the
            # base property.
            if self._is_public is None:
                self._is_public = self.base.is_public
            else:
                diag_ctx.check_source_language(
                    self._is_public == self.base.is_public,
                    "{} is {}, so should be {}".format(
                        self.base.qualname,
                        "public" if self.base.is_public else "private",
                        self.qualname,
                    ),
                )

            # Inherit the "lazy field" status, or check its consistency with
            # the base property.
            if self._lazy_field is None:
                self._lazy_field = self.base.lazy_field
            else:
                diag_ctx.check_source_language(
                    self._lazy_field == self.base.lazy_field,
                    "lazy fields cannot override properties, and conversely",
                )

            # Inherit dynamic variables used as arguments, or check their
            # consistency with the base property.
            if self.dynamic_var_args_known:
                self_dv = self.dynamic_var_args
                base_dv = self.base.dynamic_var_args

                def dv_list(p: PropertyDef) -> list[DynamicVariable]:
                    return [dv_arg.dynvar for dv_arg in p.dynamic_var_args]

                # Check that the set of dynamic variables is the same in this
                # property and in its base property, and that they are in the
                # same order.
                diag_ctx.check_source_language(
                    dv_list(self) == dv_list(self.base),
                    "Requested set of dynamically bound variables is not"
                    " consistent with the property to override: {}".format(
                        self.base.qualname
                    ),
                )

                # Check that default values for these dynamic variables are
                # consistent.
                for self_dv_arg, base_dv_arg in zip(self_dv, base_dv):
                    diag_ctx.check_source_language(
                        match_default_values(
                            self_dv_arg.default_value,
                            base_dv_arg.default_value,
                        ),
                        "Inconsistent default values for dynamic variable"
                        f" '{self_dv_arg.dynvar.lkt_name}",
                    )
            else:
                self.set_dynamic_var_args(self.base.dynamic_var_args)

            # Check the consistency of type annotations
            diag_ctx.check_source_language(
                self.type.matches(self.base.type),
                f"{self.qualname} returns {self.type.lkt_name} whereas it"
                f" overrides {self.base.qualname}, which returns"
                f" {self.base.type.lkt_name}. The former should match the"
                " latter.",
            )

            args = self.natural_arguments
            base_args = self.base.natural_arguments
            diag_ctx.check_source_language(
                len(args) == len(base_args),
                "Derived and base properties don't have the same number"
                " of arguments, base has {}, derived has {}".format(
                    len(base_args), len(args)
                ),
            )

            for i, (arg, base_arg) in enumerate(zip(args, base_args)):
                # Check that argument names and types are consistent with the
                # base property.
                diag_ctx.check_source_language(
                    arg.name == base_arg.name,
                    "Argument #{} does not have the same name here ({}) as in"
                    " base property ({})".format(
                        i + 1, arg.name.lower, base_arg.name.lower
                    ),
                )
                diag_ctx.check_source_language(
                    arg.type == base_arg.type,
                    f'Argument "{arg.lkt_name}" does not have the same type as'
                    f" in base property. Base has {arg.type.lkt_name},"
                    f" derived has {base_arg.type.lkt_name}",
                )

                # First check that the presence of a default argument value is
                # consistent with the base property.
                if arg.default_value is None:
                    diag_ctx.check_source_language(
                        base_arg.default_value is None,
                        'Argument "{}" must have the same default value as in'
                        " base property ({})".format(
                            arg.lkt_name, self.base.qualname
                        ),
                    )
                else:
                    diag_ctx.check_source_language(
                        base_arg.default_value is not None,
                        'Argument "{}" cannot have a default value, to be'
                        " consistent with its base property ({})".format(
                            arg.lkt_name, self.base.qualname
                        ),
                    )

                # Then check that if there is a default value, it is the same
                if arg.default_value is not None:
                    val = arg.default_value
                    base_val = base_arg.default_value
                    diag_ctx.check_source_language(
                        match_default_values(val, base_val),
                        'Argument "{}" does not have the same default value'
                        " ({}) as in base property ({})".format(
                            arg.lkt_name, val, base_val
                        ),
                    )

        else:
            # By default, properties are private, are not lazy fields, and they
            # have no dynamically bound variable.
            self._is_public = bool(self._is_public)
            self._lazy_field = bool(self._lazy_field)
            if not self.dynamic_var_args_known:
                self.set_dynamic_var_args([])

        self._original_is_public = self.is_public

        if self.external:
            diag_ctx.check_source_language(
                self.expr is None,
                "An external property cannot have a DSL implementation",
            )
            diag_ctx.check_source_language(
                not self.abstract, "An external property cannot be abstract"
            )

            diag_ctx.check_source_language(
                self._uses_entity_info is not None,
                "uses_entity_info is required for external properties",
            )
            diag_ctx.check_source_language(
                self._uses_envs is not None,
                "uses_envs is required for external properties",
            )

        else:
            diag_ctx.check_source_language(
                self._uses_entity_info in (None, False),
                "Cannot specify uses_entity_info=True for internal"
                " properties",
            )
            diag_ctx.check_source_language(
                self._uses_envs is None,
                "Cannot explicitly pass uses_envs for internal properties",
            )

        # At this point, we assume the list of argument has reached its final
        # state.

        if self.base:
            args_count = len(self.arguments)
            base_args_count = len(self.base.arguments)
            assert args_count == base_args_count, (
                f"{self.qualname} has {args_count} arguments, whereas its base"
                f" property {self.base.qualname} has {base_args_count} ones"
            )

        if self.lazy_field:
            # Check several invariants for lazy fields. Some are impossible by
            # construction (asserts), others are about checking what the user
            # tried (check_source_language).
            assert not self.external
            assert not self.memoized
            assert not self.dynamic_var_args
            diag_ctx.check_source_language(
                not self.natural_arguments, "Lazy fields cannot have arguments"
            )

            # If this is the root lazy field, create their storage fields: one
            # boolean telling whether the lazy field was evaluated, and the
            # field itself. For other lazy fields, just re-use the root's
            # fields.
            if self.base is None:
                owner = self.owner
                assert isinstance(owner, ASTNodeType)

                # Use the name of the owning node as a prefix for each storage
                # field, to avoid conflict with homonym lazy fields in other
                # nodes: all storage fields end up in the same discriminated
                # record type in the generated Ada code.
                field_name_template = (
                    f"{owner.api_name.lower}_lf_{{}}_{self.original_name}"
                )
                self.lazy_state_field = owner.add_internal_user_field(
                    name=names.Name.from_lower(
                        field_name_template.format("state")
                    ),
                    type=T.InitializationState,
                    default_value=InitializationStateLiteralExpr(
                        None, "Uninitialized"
                    ),
                    doc=f"Initialization state for the {self.qualname} lazy"
                    " field.",
                )

                # Access to the storage field is guarded by the "present flag"
                # field, so it is fine to leave it uninitialized.
                self.lazy_storage_field = owner.add_internal_user_field(
                    name=names.Name.from_lower(
                        field_name_template.format("stg")
                    ),
                    type=self.type,
                    default_value=None,
                    doc=f"Storage for the {self.qualname} lazy field",
                )
            else:
                self.lazy_state_field = self.base.lazy_state_field
                self.lazy_storage_field = self.base.lazy_storage_field

        # Now that all dynamic variables are known for this property, extend
        # its documentation using the docs of its dynamic variables.
        dyn_var_docs = []
        for dv_arg in self.dynamic_var_args:
            dv = dv_arg.dynvar
            if dv.doc:
                name = dv.name.camel_with_underscores
                doc = inspect.cleandoc(dv.doc)
                dyn_var_docs.append(f"``{name}``: {doc}")
        if dyn_var_docs:
            self._doc = (self._raw_doc or "") + "".join(
                f"\n\n{doc}" for doc in dyn_var_docs
            )

    @property
    def original_is_public(self) -> bool:
        return self._original_is_public

    def append_argument(self, a: Argument) -> None:
        """
        Append an argument to this property.
        """
        self.arguments.append(a)

        # If needed, create a local variable instance for this argument so that
        # the property expression can refer to it.
        if not a.has_local_var:
            a.set_local_var(
                self.vars.create(
                    location=a.location,
                    codegen_name=a.name,
                    type=a.type,
                    spec_name=a.lkt_name,
                    manual_decl=True,
                    scope=self.vars.root_scope,
                )
            )

    @property  # type: ignore
    @langkit.utils.memoized
    def entity_info_arg(self) -> LocalVars.LocalVar:
        """
        Return the local variable that contais the entity information passed as
        argument.
        """
        assert self._uses_entity_info
        return self.vars.create(
            location=Location.builtin,
            codegen_name=self.entity_info_name,
            type=T.EntityInfo,
            spec_name=self.entity_info_name.lower,
            manual_decl=True,
            scope=self.vars.root_scope,
        )

    def set_uses_entity_info(self) -> None:
        """
        Set this property as using entity information for Self.

        This triggers the addition of an implicit parameter (Entity_Info).
        """
        check_source_language(
            self._uses_entity_info is not False,
            "Cannot use entity info, as explicitly forbidden",
            location=self.location,
        )
        self._uses_entity_info = True

    @property
    def uses_envs(self) -> bool:
        """
        Return whether the proper evaluation of this property requires
        Populate_Lexical_Env to be called.
        """
        assert self._uses_envs is not None
        return self._uses_envs

    def set_uses_envs(self) -> None:
        """
        Set this property as using lexical environment lookups.

        If this property is public, this will trigger an automatical call to
        Populate_Lexical_Env.
        """
        check_source_language(
            self._uses_envs is not False,
            "Cannot use lexical environments, as explicitly forbidden",
            location=self.location,
        )
        self._uses_envs = True

    @property
    def is_reachable(self) -> bool:
        """
        Return whether this property is considered reachable.
        """
        assert self._is_reachable is not None
        return self._is_reachable

    def set_is_reachable(self, value: bool) -> None:
        """
        Set whether this property is to be considered reachable.
        """
        assert self._is_reachable is None
        self._is_reachable = value

    def require_untyped_wrapper(self) -> None:
        """
        Tag this property as requiring an untyped wrapper function.

        Untyped wrappers take a root entity instead of a node as their first
        formal. Regarding the return type::

          * if the wrapped property returns an entity, the wrapper returns
            the root entity;

          * if the wrapped property returns a node, the wrapper returns the
            root node.

        These wrappers are used as callbacks in lexical environments.
        """
        self._requires_untyped_wrapper = True

    @property
    def requires_untyped_wrapper(self) -> bool:
        return self._requires_untyped_wrapper

    @property
    def untyped_wrapper_rtype(self) -> CompiledType:
        """
        Assuming this property requires an untyped wrapper, return the return
        type of this wrapper.
        """
        assert self.requires_untyped_wrapper
        if self.type.is_entity_type:
            return T.entity
        elif self.type.is_ast_node:
            return T.root_node
        else:
            return self.type

    def set_expr(
        self,
        error_location: Location | L.LktNode,
        expr: Expr,
    ) -> None:
        """
        Assign a body expression to this property.
        """
        assert self.expr is None

        # The type of some internal properties is defined by its expression, so
        # we use a placeholder type when creating the internal property: set
        # the real type after the expression has been constructed.
        if isinstance(self.type, NoCompiledType):
            self.expr = expr
            self.type = expr.type
        else:
            # In order to generate valid Ada code, make sure the body
            # expression evaluates to precisely the return type for this
            # property.
            self.expr = maybe_cast(
                error_location,
                expr,
                self.type,
                (
                    (
                        "expected type {{expected}}, got"
                        " {{expr_type}} instead (expected type comes from"
                        " overridden base property in {base_prop})".format(
                            base_prop=self.base.owner.lkt_name
                        )
                    )
                    if self.base
                    else None
                ),
            )

        if self.dump_ir:
            print(expr.ir_dump)

        # Make sure that all the created local variables are associated to a
        # scope.
        self.vars.check_scopes()

        # Warn on unused bindings
        self.warn_on_unused_bindings()

    def check_overriding_types(self, context: CompileCtx) -> None:
        """
        Check that the return type of this property and the return type of the
        base property that self overrides are the same, if applicable.
        """
        if self.base and self.base.type:
            check_source_language(
                self.type.matches(self.base.type),
                "{} returns {} whereas it overrides {}, which returns {}."
                " The former should match the latter.".format(
                    self.qualname,
                    self.type.lkt_name,
                    self.base.qualname,
                    self.base.type.lkt_name,
                ),
                location=self.location,
            )

    def render_property(self, context: CompileCtx) -> None:
        """
        Render the given property to generated code.
        """
        with self.bind():
            with names.camel_with_underscores:
                self.prop_decl = render("properties/decl_ada")
                self.prop_def = render("properties/def_ada")

                if self.requires_untyped_wrapper:
                    self.untyped_wrapper_decl = render(
                        "properties/untyped_wrapper_decl_ada"
                    )
                    self.untyped_wrapper_def = render(
                        "properties/untyped_wrapper_def_ada"
                    )
                else:
                    self.untyped_wrapper_decl = self.untyped_wrapper_def = ""

    @property
    def doc(self) -> str:
        return self._doc or ""

    @property
    def natural_arguments(self) -> list[Argument]:
        non_art, art = funcy.lsplit_by(
            lambda a: not a.is_artificial, self.arguments
        )
        assert all(
            a.is_artificial for a in art
        ), "All artificial arguments must come after all the other ones"
        return non_art

    @langkit.utils.memoized
    def do_generate_logic_predicate(
        self,
        partial_args: tuple[PropertyClosure.PartialArgument, ...],
        default_passed_args: int,
    ) -> str:
        """
        Helper method, will trigger the emission of a logic predicate object
        for the property for the given partial argument types.

        :param partial_args: Arguments passed to the property for partial
            evaluation.
        :param default_passed_args: Number of arguments passed by default
            value.

        :return: The identifier for the logic predicate, to be used as a prefix
            in code generation for every entity related to it.
        """
        # We use the length of the list as an id for the logic predicate. If
        # the method is called again with the same arg types, the same id
        # will be returned thanks to memoization.
        pred_num = len(self.logic_predicates)

        # This id will uniquely identify both the generic package and the
        # closure data structure.
        with names.camel_with_underscores:
            pred_id = "{}_{}".format(self.names.codegen, pred_num)

        # We can use a list because the method is memoized, eg. this won't
        # be executed twice for the same partial_args_types tuple.
        self.logic_predicates.append(
            PropertyClosure(pred_id, partial_args, default_passed_args)
        )

        return pred_id

    @langkit.utils.memoized
    def do_generate_logic_functor(
        self,
        partial_args: tuple[PropertyClosure.PartialArgument, ...],
        default_passed_args: int,
    ) -> str:
        """
        Generate a logic binder for this convert/combine property.

        If you call this function several times for the same property, only one
        binder will be generated.

        :param arity: Number of entity arguments this property takes ("Self"
            included).

        :return: The identifier for the logic functor, to be used as a prefix
            in code generation for every entity related to it.
        """
        functor_num = len(self.logic_functors)

        # This id will uniquely identify both the generic package and the
        # closure data structure.
        with names.camel_with_underscores:
            functor_id = "{}_{}".format(self.names.codegen, functor_num)

        # Thanks to memoization, we will generate at most one functor for the
        # given arguments, so storing them in a list is fine.
        self.logic_functors.append(
            PropertyClosure(functor_id, partial_args, default_passed_args)
        )

        return functor_id

    def get_concrete_node_types(
        self,
        closure: PropertyClosure,
    ) -> list[CompiledType]:
        """
        Helper for emission of logic closure wrappers. Return the concrete
        node type for leading arguments that correspond to logic variables
        bound by the given closure.
        """
        logic_vars = (
            len(self.arguments)
            - len(closure.partial_args)
            - closure.default_passed_args
        )
        assert self.owner is not None
        return [self.owner] + [a.type for a in self.arguments[:logic_vars]]

    @property
    def memoization_enum(self) -> str:
        """
        Return the enumerator name to materialize references to this property
        in the memoization engine.
        """
        assert self.owner
        return (
            (names.Name("Mmz") + self.owner.name).camel_with_underscores
            + "_"
            + self.names.codegen
        )

    @property
    def reason_for_no_memoization(self) -> str | None:
        """
        Return whether this property is a valid candidate for memoization.

        If it is memoizable, return None, otherwise return a message that
        describes why it is not memoizable.

        This predicate ignores callgraph considerations and focuses on
        characteristics specific to ``self``: whether it contains side-effects
        (equation solving), whether it is external, or abstract. The
        ``CompileCtx.check_memoized`` pass will take care of doing call-graph
        analysis on top of this.
        """
        if self.abstract:
            return (
                "A memoized property cannot be abstract: memoization is"
                " not an inherited behavior"
            )

        if self.external:
            return "An external property cannot be memoized"

        return None

    @property
    def transitive_reason_for_no_memoization(self) -> str | None:
        """
        Determine if there is a reason that this property cannot be memoized.
        If so, this reason is considered to be transitive and will propagate to
        properties that all this one.

        If there is no such reason (i.e. if this property can be memoized,
        assuming that ``self.reason_for_no_memoization`` returns True), return
        None. Otherwise, return the reason as a string.

        As for ``reason_for_no_memoization``, this does not do callgraph
        propagation itself and relies on ``CompileCtx.check_memoized`` to do
        so.
        """
        if self._call_non_memoizable_because:
            return self._call_non_memoizable_because
        elif self._solves_equation:
            return "Cannot memoize equation solving"
        elif self._gets_logic_var_value:
            return "Cannot memoize extracting the value of a logic variable"
        else:
            return None

    def warn_on_unused_bindings(self) -> None:
        """
        Emit warnings for bindings such as variables or arguments, that are not
        used. Also emit warnings for bindings that are used whereas they have
        been tagged as ignored.
        """
        # Mapping to tell for each variable if it is referenced at least once
        assert self.expr is not None
        all_vars = {}
        for v in self.expr.bindings:
            all_vars[v] = False
        for arg in self.natural_arguments:
            all_vars[arg.local_var.ref_expr] = False

        def mark_vars(expr: Expr) -> None:
            if isinstance(expr, BindingScope):
                # BindingScope has bindings themselves as operands, but they
                # must not be considered as uses for this analysis: skip them.
                expr = expr.expr

            if isinstance(expr, VariableExpr):
                all_vars[expr] = True

            for sub in expr.flat_actual_subexprs():
                mark_vars(sub)

        mark_vars(self.expr)
        unused_vars = [
            var
            for var, is_used in all_vars.items()
            if not is_used and not var.ignored
        ]
        wrongly_used_vars = [
            var for var, is_used in all_vars.items() if is_used and var.ignored
        ]

        unused_vars.sort(key=lambda var: var.name)
        wrongly_used_vars.sort(key=lambda var: var.name)

        def format_list(vars: list[VariableExpr]) -> str:
            return ", ".join(var.source_name or var.name.lower for var in vars)

        # TODO: once the Lkt transition is over, emit one warning per unused
        # binding, and attach it to the location of the binding declaration in
        # Lkt code.
        assert self.location
        WarningSet.unused_bindings.warn_if(
            bool(unused_vars),
            "The following bindings are not used: {}".format(
                format_list(unused_vars)
            ),
            location=self.location,
        )
        WarningSet.unused_bindings.warn_if(
            bool(wrongly_used_vars),
            "The following bindings are used even though they are supposed to"
            " be ignored: {}".format(format_list(wrongly_used_vars)),
            location=self.location,
        )

    def warn_on_undocumented_public_property(
        self, context: CompileCtx
    ) -> None:
        del context
        assert self.location
        # For public properties only, warn undocumented ones. Only warn for
        # base properties: no need to repeat for the other ones.
        WarningSet.undocumented_public_properties.warn_if(
            self.is_public and not self.is_overriding and not self.doc,
            "This property is public but it lacks documentation",
            location=self.location,
        )

    @property
    def lazy_field(self) -> bool:
        assert self._lazy_field is not None
        return self._lazy_field

    def check_docstring(self, context: CompileCtx) -> None:
        """
        Property pass function that will check that the docstring for this
        function is correct.
        """
        del context
        RstCommentChecker.check_doc(Location.for_entity_doc(self), self.doc)


def lazy_field(
    owner: CompiledType,
    expr: Expr,
    names: MemberNames,
    location: Location,
    return_type: CompiledType,
    doc: str,
    public: bool | None = None,
    abstract: bool = False,
    warn_on_unused: bool | None = None,
    activate_tracing: bool = False,
    dump_ir: bool = False,
    local_vars: LocalVars | None = None,
) -> PropertyDef:
    """
    Return a decorator to create a lazy field.

    A lazy field is a node field that is initialized on demand, using a
    property expression. The result of that property is stored in the node
    itself, and re-used later on, whenever the field is used.

    Unlike with memoized properties, the cache for the property result is not
    reset when an analysis unit is (re)parsed. This makes lazy fields better
    suited to create synthetic nodes. TODO: eventually we will forbit node
    synthetization in memoized properties.

    See PropertyDef for details about the semantics of arguments.
    """
    return PropertyDef(
        owner=owner,
        names=names,
        location=location,
        type=return_type,
        expr=expr,
        public=public,
        doc=doc,
        abstract=abstract,
        dynamic_vars=None,
        memoized=False,
        call_memoizable=True,
        memoize_in_populate=False,
        external=False,
        # Initializers for lazy fields cannot use entity info (this would be a
        # soundness issue).
        uses_entity_info=False,
        uses_envs=None,
        warn_on_unused=warn_on_unused,
        call_non_memoizable_because=None,
        activate_tracing=activate_tracing,
        dump_ir=dump_ir,
        lazy_field=True,
        local_vars=local_vars,
    )


def aggregate_expr(
    type: ArrayType | str | None,
    assocs: list[tuple[str | names.Name, Expr]],
) -> Expr:
    """
    Create an expression for an Ada aggregate.

    :param type: Type of the aggregate.

        If None, generate a mere Ada aggregate. For instance: ``(A, B, C)``.

        If it's a string, use it as a type name to generate a qualified
        expression. For instance, with ``type='Foo'``: ``Foo'(A, B, C)``.

        Otherwise, use the given ArrayType to generate a qualified expression,
        unless it's NoCompiledType.

        Unless a true ArrayType instance is provided, the result will get the
        NoCompiledType type annotation.

    :param assocs: List of associations for the aggregate.
    """
    result_type: CompiledType
    if type is None or type is T.NoCompiledType:
        meta_template = "({operands})"
        type_name = None
        result_type = T.NoCompiledType
    elif isinstance(type, str):
        meta_template = "{type}'({operands})"
        type_name = type
        result_type = T.NoCompiledType
    else:
        assert isinstance(type, ArrayType)
        result_type = type
        meta_template = "{type}'({operands})"
        type_name = type.array_type_name.camel_with_underscores

    template = meta_template.format(
        type=type_name,
        operands=(
            ", ".join(
                "{} => {{}}".format(
                    n.camel_with_underscores
                    if isinstance(n, names.Name)
                    else n
                )
                for n, _ in assocs
            )
            or "null record"
        ),
    )
    return LiteralExpr(None, template, result_type, [e for _, e in assocs])


class BasicExpr(ComputingExpr):
    """
    A basic expression template, that automatically handles:

    - Passing a list of sub expressions to the constructor, and a type
    - Doing the pre render of those expressions automatically
    - Rendering the sub expressions passed as parameters into the holes that
      are in the template.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        result_var_name: str,
        template: str,
        type: CompiledType | None,
        operands: Sequence[Expr | str],
        requires_incref: bool = True,
    ):
        """
        :param result_var_name: See Expr's constructor.
        :param template: The template string.
        :param type: The return type of the expression.
        :param requires_incref: Whether the computation in `template` returns a
            value that must be inc-ref'd to be stored in the result variable.
        """
        self.template = template
        self.static_type = type
        self.operands = operands
        self.requires_incref = requires_incref
        super().__init__(debug_info, result_var_name)

    def _render_pre(self) -> str:
        expr = self.template.format(
            *[
                (e if isinstance(e, str) else e.render_expr())
                for e in self.operands
            ]
        )
        return "\n".join(
            [e.render_pre() for e in self.operands if not isinstance(e, str)]
            + [
                assign_var(
                    self.result_var.ref_expr, expr, self.requires_incref
                )
            ]
        )

    @property
    def subexprs(self) -> dict:
        return {
            "operands": [op for op in self.operands if isinstance(op, Expr)]
        }


class FieldAccessExpr(BasicExpr):
    """
    Expression for anything that compiles to "{prefix}.{field}" in the
    generated code.

    Note that this automatically generates a null safety check if prefix is
    allowed to be null.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        prefix_expr: Expr,
        field_name: str,
        result_type: CompiledType,
        do_explicit_incref: bool,
    ):
        """
        :param prefix_expr: The prefix corresponding to this expression.
        :param field_name: The name of the field to access.
        :param type: The type of the result.
        :param do_explicit_incref: If True, perform an inc-ref on the result of
            the field access. This must be True for field accesses that do not
            automatically perform it.
        """
        super().__init__(
            debug_info,
            "Fld",
            "{}.{}",
            result_type,
            [NullCheckExpr(prefix_expr), field_name],
            requires_incref=do_explicit_incref,
        )
        self.prefix_expr = prefix_expr
        self.field_name = field_name

    @property
    def subexprs(self) -> dict:
        return {"prefix": self.prefix_expr, "field": self.field_name}

    def __repr__(self) -> str:
        return "<FieldAccessExpr {} ({})>".format(
            self.field_name, self.type.name.camel
        )


class LocalVars:
    """
    Represents the state of local variables in a property definition.
    """

    def __init__(self) -> None:
        self.names: set[names.Name] = set()
        self.local_vars: dict[names.Name, LocalVars.LocalVar] = {}
        self.root_scope = LocalVars.Scope(self, None)
        self.current_scope = self.root_scope

    class Scope:
        """
        Local variables are organized in a traditional scope hierarchy.

        During properties compilation, scopes are created and variables are put
        in a specific scope. This will help memory management: when execution
        goes out of a scope, the ref-count for all the variables is
        decremented.
        """

        COUNT = count(0)

        def __init__(self, vars: LocalVars, parent: LocalVars.Scope | None):
            """
            :param vars: LocalVars instance for this scope.
            :param parent: Parent scope.
            """
            self.index = next(self.COUNT)
            self.vars = vars
            self.parent = parent
            self.sub_scopes: list[LocalVars.Scope] = []
            self.variables: list[LocalVars.LocalVar] = []

        @property
        def name(self) -> names.Name:
            return names.Name("Scope_{}".format(self.index))

        @property
        def finalizer_name(self) -> names.Name:
            """
            Return the name of the finalization procedure for this scope.
            """
            return names.Name("Finalizer") + self.name

        def has_refcounted_vars(self, include_children: bool = False) -> bool:
            """
            Return whether this scope contains at least one variable that
            matters for reference counting.

            :param include_children: Whether to account for children in the
                computation.
            """
            for var in self.variables:
                if var.needs_refcount:
                    return True

            return include_children and any(
                s.has_refcounted_vars(True) for s in self.sub_scopes
            )

        def add(self, var: LocalVars.LocalVar) -> None:
            """
            Associate "var" to this scope. Doing so twice for the same variable
            is an error.

            :param var: Variable to associate.
            """
            assert var._scope is None, (
                "Trying to associate {} to some scope whereas it already has"
                " one".format(var)
            )
            self.variables.append(var)
            var._scope = self

        def push(self) -> LocalVars.Scope:
            """
            Create a new scope that is a child for the current scope, make it
            the current scope and return it.
            """
            result = LocalVars.Scope(self.vars, self)
            self.sub_scopes.append(result)
            self.vars.current_scope = result
            return result

        def pop(self) -> LocalVars.Scope:
            """
            Set the current scope to the parent of the current scope. Return
            this parent scope. Doing so when the current scope is the root one
            is an error.

            :rtype: LocalVars.Scope
            """
            parent = self.vars.current_scope.parent
            assert parent, "Trying to pop the root scope"
            self.vars.current_scope = parent
            return parent

        @contextmanager
        def new_child(self) -> Iterator[LocalVars.Scope]:
            """
            Create a child scope for this block and return a context manager to
            make it the current scope temporarily.
            """
            yield self.push()
            self.pop()

        @contextmanager
        def use(self) -> Iterator[LocalVars.Scope]:
            """
            Return a context manager to make self the current scope
            temporarily.
            """
            old_scope = self.vars.current_scope
            self.vars.current_scope = self
            yield self
            self.vars.current_scope = old_scope

    class LocalVar:
        """
        Represents one local variable in a property definition.
        """

        type: CompiledType
        """
        Type for this local variable.

        TODO (eng/libadalang/langkit#880): As long as typing is done during
        the "construct" pass, the type of some local variables cannot be known
        at ``LocalVar`` instantiation time, so this attribute as well as the
        ``ref_expr`` one are set only when the type is known: see the
        ``set_type`` method.
        """

        ref_expr: VariableExpr
        """
        Expression used to refer to this local variable.

        TODO (eng/libadalang/langkit#880): Set only when its type is known: see
        the above docstring.
        """

        def __init__(
            self,
            location: Location,
            vars: LocalVars,
            codegen_name: names.Name,
            type: CompiledType | None = None,
            spec_name: str | None = None,
            manual_decl: bool = False,
        ):
            """
            :param location: Location for this variable, if it comes from the
                lanugage spec.
            :param vars: The LocalVars instance to which this local variable is
                bound.
            :param codegen_name: The name of this local variable in the
                generated Ada code.
            :param type: The type of this local variable, if known at that
                point.
            :param spec_name: If this local variable materializes a language
                spec entity, name of this entity as found in the langugae spec
                (i.e. lower case).
            :param manual_decl: Whether this variable is declared manually in
                the generated source code, i.e. as part of scope handling. If
                not, it is up to other templates to declare it. For instance,
                when the variable is declared by a ``for`` loop in Ada. When
                this is true, it is also up to the template to handle
                refcounting for this local variable.
            """
            self.location = location
            self.vars = vars
            self.codegen_name = codegen_name
            self.has_type = False
            self.spec_name = spec_name
            if type is not None:
                self.set_type(type)
            self.manual_decl = manual_decl

            self._scope: LocalVars.Scope | None = None
            """
            The scope this variable lives in. During the construct phase, all
            expressions that create local variables must initialize this using
            LocalVars.Scope.add.
            """

        def set_type(self, t: CompiledType) -> None:
            """
            Assuming that the type of this local variable is known late, assign
            it.
            """
            assert not self.has_type
            self.type = t
            self.has_type = True
            self.ref_expr = VariableExpr(
                self.type, self.codegen_name, local_var=self
            )
            if self.spec_name == "_":
                self.ref_expr.set_ignored()

        def consolidate_type(
            self,
            t: CompiledType,
            message: str,
            location: Location,
        ) -> None:
            """
            If this local variable does not have a type yet, set its type to
            ``t``. If it already has one, ensure that it is equal to ``t``, and
            emit a language spec error if not.

            :param t: Type for this local variable.
            :param message: Error message in case of type mismatch.
            :param location: Error location in case of type mismatch.
            """
            if not self.has_type:
                self.set_type(t)
            elif self.type != t:
                error(
                    f"{message}: expected {self.type.lkt_name}, got"
                    f" {t.lkt_name}",
                    location=location,
                )

        @property
        def needs_refcount(self) -> bool:
            """
            Return whether scope finalizers must decrement the refcount for
            this local variable.
            """
            return not self.manual_decl and self.type.is_refcounted

        def render(self) -> str:
            return "{} : {}{};".format(
                self.codegen_name.camel_with_underscores,
                self.type.name.camel_with_underscores,
                (
                    " := {}".format(self.type.nullexpr)
                    if self.type.is_refcounted and not self.type.is_ptr
                    else ""
                ),
            )

        @property
        def name_type_label(self) -> str:
            name_label = self.codegen_name.camel_with_underscores
            paren_items = []
            if self.spec_name:
                paren_items.append(self.spec_name)
            paren_items.append(self.location.gnu_style_repr())
            type_label = (
                self.type.lkt_name if self.has_type else "<unknown type>"
            )
            return f"{name_label} ({', '.join(paren_items)}): {type_label}"

        def __repr__(self) -> str:
            manual_label = " (manual decl)" if self.manual_decl else ""
            return f"<LocalVar {self.name_type_label}{manual_label}>"

    def create(
        self,
        location: Location,
        codegen_name: str | names.Name,
        type: CompiledType | None = None,
        spec_name: str | None = None,
        manual_decl: bool = False,
        scope: LocalVars.Scope | None = None,
    ) -> LocalVars.LocalVar:
        """
        Create a local variable to use in generated code.

        The actual code generation name for the returned variable is
        potentially derived from the passed ``codegen_name`` argument to avoid
        homonyms in a given property.

        The new local variable is automatically associated to the current
        scope.

        :param scope: If provided, add the created local variable to that
            scope. If not, use the current scope.

        See ``LocalVars.LocalVar.__init__`` for the semantic of other
        arguments.
        """
        result = self.create_scopeless(
            location, codegen_name, type, spec_name, manual_decl
        )
        if scope:
            scope.add(result)
        else:
            PropertyDef.get_scope().add(result)
        return result

    def create_scopeless(
        self,
        location: Location,
        codegen_name: str | names.Name,
        type: CompiledType | None = None,
        spec_name: str | None = None,
        manual_decl: bool = False,
    ) -> LocalVars.LocalVar:
        """
        Like ``create`` but do not assign a scope for the new local variable.
        The scope will have to be initialized later.
        """
        name = names.Name.get(codegen_name)

        i = 0
        orig_name = name.base_name
        while name in self.names:
            i += 1
            name = names.Name(f"{orig_name}_{i}")
        ret = LocalVars.LocalVar(
            location, self, name, type, spec_name, manual_decl
        )
        self.local_vars[name] = ret
        self.names.add(name)
        return ret

    def check_scopes(self) -> None:
        """
        Check that all variables are associated to a scope. Raise an
        AssertionError if it is not the case.
        """
        for var in self.local_vars.values():
            assert var._scope, "{} has no scope".format(var)

    @property
    def all_scopes(self) -> list[LocalVars.Scope]:
        """
        Return the list of all scopes in this repository.
        """

        def children(s: LocalVars.Scope) -> list[LocalVars.Scope]:
            return s.sub_scopes

        return funcy.ltree_nodes(self.root_scope, children, children)

    def render(self) -> str:
        return "\n".join(
            lv.render()
            for lv in self.local_vars.values()
            if not lv.manual_decl
        )


class CallExpr(BasicExpr):
    """
    Convenience expression that models a call to a function on the Ada side of
    things. This assumes that for ref-counted types, function calls return a
    new ownership share to the caller.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        result_var_name: str,
        name: names.Name | str,
        type: CompiledType,
        exprs: Sequence[str | Expr],
        shadow_args: list[Expr | AbstractNodeData] = [],
    ):
        """
        :param result_var_name: See Expr's constructor.
        :param name: The name of the procedure to call.
        :param type: The return type of the function call.
        :param exprs: A list of expressions that represents the arguments to
            the function call.
        :param shadow_args: Arguments that do not contribute to code
            generation, but still to be considered for their side effects in
            various analysis (for instance, a property so that it is considered
            called by this expression).
        """
        self.name = (
            name if isinstance(name, str) else name.camel_with_underscores
        )

        args = ", ".join(["{}"] * len(exprs))
        template = f"{self.name} ({args})" if exprs else f"{self.name}"

        self.shadow_args = list(shadow_args)

        super().__init__(
            debug_info,
            result_var_name,
            template,
            type,
            exprs,
            requires_incref=False,
        )

    @property
    def subexprs(self) -> dict:
        return {
            "0-type": self.type,
            "1-name": self.name,
            "2-args": self.operands,
            "3-shadow-args": self.shadow_args,
        }

    def __repr__(self) -> str:
        return f"<CallExpr {self.name}>"


class NullCheckExpr(Expr):
    """
    Expression that raises a PropertyError when the input is a null pointer.
    Just return the input otherwise.

    Note that the check is not performed at all when property checks are
    disabled context-wide.
    """

    def __init__(self, expr: Expr, implicit_deref: bool = False):
        """
        :param expr: Expression to evaluate.
        :param implicit_deref: If expr is an entity, perform the check on the
            embedded AST node instead.
        """
        self.expr = expr
        self.implicit_deref = implicit_deref

        # There is no need for ref-counting handling because this expression
        # only forwards the result of the "expr" operand to the user, without
        # storing it in a local variable.
        super().__init__(None, skippable_refcount=True)

    @property
    def type(self) -> CompiledType:
        return self.expr.type

    def _render_pre(self) -> str:
        return render("properties/null_check_ada", expr=self)

    def _render_expr(self) -> str:
        return self.expr.render_expr()

    @property
    def subexprs(self) -> dict:
        return {"expr": self.expr}

    def __repr__(self) -> str:
        return "<NullCheckExpr>"


class BigIntLiteralExpr(CallExpr):
    """
    Turn an integer value into a big integer one.
    """

    # TODO: the class name is misleading: this is a literal only if the given
    # expression is an integer value. When it is a more complex expression,
    # this is actually a conversion (from the Int type to BigInt). We should
    # rename this once the transition to Lkt is completed.

    def __init__(self, debug_info: ExprDebugInfo | None, expr: Expr | str):
        self.bigint_expr = expr
        super().__init__(
            debug_info, "Big_Int", "Create_Big_Integer", T.BigInt, [expr]
        )

    def __repr__(self) -> str:
        return "<BigInteger.Expr {}>".format(self.bigint_expr)


def make_as_int(
    debug_info: ExprDebugInfo | None,
    expr: Expr,
    prop: PropertyDef,
) -> Expr:
    """
    Return an expression that converts a big integer value (``expr``) to a
    regular integer. This raises a ``PropertyError`` if the big integer is out
    of range.
    """
    return CallExpr(
        debug_info,
        "Small_Int",
        "To_Integer",
        T.Int,
        [prop.node_var.ref_expr, expr],
    )


class UnaryNegExpr(ComputingExpr):
    """
    Unary "-" operator.
    """

    def __init__(self, debug_info: ExprDebugInfo | None, expr: Expr):
        self.expr = expr
        self.static_type = expr.type
        super().__init__(debug_info, "Neg")

    def _render_pre(self) -> str:
        result = [
            self.expr.render_pre(),
            assign_var(
                self.result_var.ref_expr,
                f"-{self.expr.render_expr()}",
                requires_incref=False,
            ),
        ]
        return "\n".join(result)

    @property
    def subexprs(self) -> dict:
        return {"expr": self.expr}


def sloc_info_arg(loc: Location) -> str:
    """
    Return an Ada expression to that, if Adalog debug is not
    enabled at runtime, returns null, or that allocates a String to contain the
    DSL callstack corresponding to the given location.
    """
    return (
        "(if Langkit_Support.Adalog.Debug.Debug"
        ' then New_Unit_String (Node.Unit, "{}")'
        " else null)".format(loc.gnu_style_repr())
    )


if TYPE_CHECKING:
    from langkit.expressions.structs import EvalMemberExpr
