from __future__ import annotations

import argparse
from dataclasses import dataclass, field, replace
import enum
from functools import lru_cache
import os
import os.path as P
import re
import sys
from typing import (
    Any,
    Callable,
    ClassVar,
    Iterable,
    NoReturn,
    Sequence,
    TYPE_CHECKING,
    TextIO,
    TypeVar,
)


from langkit.utils import Colors, assert_type, col


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx
    from langkit.compiled_types import CompiledType
    from langkit.expressions import PropertyDef

    import liblktlang as L


class DiagnosticStyle(enum.Enum):
    """Format for diagnostics that Langkit emits: location and text."""

    default = "default"
    """Human-readable tracebacks."""

    gnu_full = "gnu-full"
    """Standard GNU format with full paths."""

    gnu_base = "gnu-base"
    """Standard GNU format with basenames."""


class Diagnostics:
    """
    Holder class that'll store the language definition source dir. Meant to
    be called by manage before functions depending on knowing the language
    source dir can be called.
    """

    has_pending_error = False

    style = DiagnosticStyle.default
    """
    DiagnosticStyle instance to select the diagnostic representation format.

    :type: DiagnosticStyle
    """

    @classmethod
    def set_style(cls, style: DiagnosticStyle) -> None:
        """
        Set the diagnostic output format.
        """
        cls.style = style


@dataclass(order=True, frozen=True)
class Location:
    """
    Holder for a location in the source code.
    """

    file: str
    """
    Path to the file for this location.
    """

    line: int = field(default=0)
    """
    Line number (1-based). Zero if unspecified.
    """

    column: int = field(default=0)
    """
    Column number (1-based). Zero if unspecified.

    :type: int
    """

    end_line: int = field(default=0)
    end_column: int = field(default=0)
    """
    End line and column numbers. Zero if unspecified.
    """

    # RA22-015 TODO: Remove this "zero if unspecified" business when we get rid
    # of the legacy DSL.

    lkt_unit: L.AnalysisUnit | None = field(default=None)

    def gnu_style_repr(self, relative: bool = True) -> str:
        """
        Return a GNU style representation for this Location, in the form::

            file:line:column

        :param relative: When True, the file path will be relative.
        """
        parts = [P.basename(self.file) if relative else self.file]
        if self.line > 0:
            parts.append(str(self.line))
        if self.column > 0:
            parts.append(str(self.column))
        return ":".join(parts)

    @classmethod
    def from_sloc_range(
        cls, unit: L.AnalysisUnit, sloc: L.SlocRange
    ) -> Location:
        """
        Create a Location based on a LKT SlocRange and AnalysisUnit.
        """
        return cls(
            unit.filename,
            sloc.start.line,
            sloc.start.column,
            sloc.end.line,
            sloc.end.column,
            unit,
        )

    @classmethod
    def from_lkt_node(cls, node: L.LktNode) -> Location:
        """
        Create a Location based on a Lkt node.
        """
        return cls.from_sloc_range(node.unit, node.sloc_range)

    @classmethod
    def from_lkt_node_range(cls, start: L.LktNode, end: L.LktNode) -> Location:
        """
        Create a Location from the range that two Lkt nodes cover.
        """
        unit = start.unit
        start_sloc = start.sloc_range
        end_sloc = end.sloc_range
        return cls(
            unit.filename,
            start_sloc.start.line,
            start_sloc.start.column,
            end_sloc.end.line,
            end_sloc.end.column,
            unit,
        )

    @classmethod
    def from_lkt_node_or_none(cls, node: L.LktNode | None) -> Location | None:
        """
        Create a Location based on a Lkt node. Accept null nodes: return a null
        location in that case.
        """
        return None if node is None else cls.from_lkt_node(node)

    @classmethod
    def from_lkt_tokens(
        cls,
        n: L.LktNode,
        start: L.Token,
        end: L.Token,
    ) -> Location:
        """
        Create a Location from the range that two Lkt tokens cover.
        """
        unit = n.unit
        start_loc = start.sloc_range.start
        end_loc = end.sloc_range.end
        return cls(
            unit.filename,
            start_loc.line,
            start_loc.column,
            end_loc.line,
            end_loc.column,
            unit,
        )

    @classmethod
    def for_entity_doc(
        cls,
        entity: CompiledType | PropertyDef,
    ) -> Location:
        """
        Return the location of the docstring for the given entity, if
        available.
        """
        # Lkt case: we have precise location for the entity doc: just return it
        if entity._doc_location is not None:
            return entity._doc_location

        # Python DSL: the docstring usually starts one line after the
        # declaration's first line ("class" keyword or "def" one), and the
        # docstring first line (just the """ string delimiter) is stripped.
        result = entity.location
        return (
            Location.unknown
            if result is None
            else replace(result, line=result.line + 2)
        )

    @staticmethod
    def resolve(location: Location | L.LktNode) -> Location:
        """
        If "location" is a node from liblktlang, turn it into a Location
        instance. Note that this possible only if liblktlang is available, so
        we know that we'll have a Location instance afterwards.
        """
        if isinstance(location, Location):
            return location
        else:
            import liblktlang as L

            assert isinstance(location, L.LktNode)
            return Location.from_lkt_node(location)

    builtin: ClassVar[Location]
    """
    Special location to designate the abstract source location where builtins
    are defined.
    """

    nowhere: ClassVar[Location]
    """
    Special location to designate no location in particular. Useful for errors
    that do not relate to a specific place in source code.
    """

    unknown: ClassVar[Location]
    """
    Special location to designate an entity that comes from the language spec,
    but at an unknown location.

    TODO (eng/libadalang/langkit#880): this should disappear once the Python
    DSL is retired.
    """


Location.builtin = Location("<builtin>")
Location.nowhere = Location("")
Location.unknown = Location("<unknown>")


class DiagnosticError(Exception):
    pass


class Severity(enum.IntEnum):
    """
    Severity of a diagnostic. For the moment we have two levels, warning and
    error. A warning won't end the compilation process, and error will.
    """

    warning = 1
    error = 2
    non_blocking_error = 3


SEVERITY_COLORS = {
    Severity.warning: Colors.YELLOW,
    Severity.error: Colors.RED,
    Severity.non_blocking_error: Colors.RED,
}


def format_severity(severity: Severity) -> str:
    msg = (
        "Error"
        if severity == Severity.non_blocking_error
        else severity.name.capitalize()
    )
    return col(msg, Colors.BOLD + SEVERITY_COLORS[severity])


def coerce_location(location: Location | L.LktNode) -> Location:
    """
    If given a location, just return it. If given a Lkt node, return its
    location.
    """
    if isinstance(location, Location):
        return location
    else:
        import liblktlang as L

        assert isinstance(location, L.LktNode)
        return Location.from_lkt_node(location)


def get_parsable_location(location: Location | L.LktNode) -> str:
    """
    Returns an error location in the common tool parsable format::

        {file}:{line}:{column}

    Depending on the diagnostic style enabled, `file` will be a base name or a
    full path. Note that this should not be run when `DiagnosticStyle.default`
    is enabled.
    """
    assert Diagnostics.style != DiagnosticStyle.default
    loc = coerce_location(location)
    if loc:
        path = (
            P.abspath(loc.file)
            if Diagnostics.style == DiagnosticStyle.gnu_full
            else P.basename(loc.file)
        )
        return "{}:{}:1".format(path, loc.line)
    else:
        return ""


def error(
    message: str,
    location: Location | L.LktNode,
    ok_for_codegen: bool = False,
) -> NoReturn:
    """
    Shortcut around ``check_source_language``, for fatal errors.
    """
    check_source_language(
        False,
        message,
        location,
        ok_for_codegen=ok_for_codegen,
    )
    # NOTE: The following raise is useless, but is there because mypy is not
    # clever enough to know  that the previous call will never return.
    raise AssertionError("should not happen")


def emit_error(
    message: str,
    location: Location | L.LktNode,
    severity: Severity = Severity.error,
    ok_for_codegen: bool = False,
) -> None:
    """
    Like ``error()``, but not raising an exception.
    """
    check_source_language(
        False,
        message,
        location,
        severity=severity,
        do_raise=False,
        ok_for_codegen=ok_for_codegen,
    )


def non_blocking_error(
    message: str,
    location: Location | L.LktNode,
    ok_for_codegen: bool = False,
) -> None:
    """
    Shortcut around ``check_source_language``, for non-fatal errors.
    """
    check_source_language(
        False,
        message,
        location=location,
        ok_for_codegen=ok_for_codegen,
        severity=Severity.non_blocking_error,
    )


def check_source_language(
    predicate: bool,
    message: str,
    location: Location | L.LktNode,
    severity: Severity = Severity.error,
    do_raise: bool = True,
    ok_for_codegen: bool = False,
) -> None:
    """
    Check predicates related to the user's input in the input language
    definition. Show error messages and eventually terminate if those error
    messages are critical.

    :param predicate: The predicate to check.
    :param message: The base message to display if predicate happens to be
        false.
    :param severity: The severity of the diagnostic.
    :param location: Location associated to the diagnostic to emit.
    :param do_raise: If True, raise a DiagnosticError if predicate happens to
        be false.
    :param ok_for_codegen: If True, allow checks to be performed during
        code generation. This is False by default as it should be an
        exceptional situation: we want, when possible, most checks to be
        performed before we attempt to emit the generated library (for
        --check-only).
    """
    from langkit.compile_context import get_context_or_none

    if not ok_for_codegen:
        ctx = get_context_or_none()
        assert ctx is None or not ctx.emission_started

    severity = assert_type(severity, Severity)
    indent = " " * 4

    if not predicate:
        message_lines = message.splitlines()
        message = "\n".join(
            message_lines[:1] + [indent + line for line in message_lines[1:]]
        )

        location = coerce_location(location)
        if Diagnostics.style != DiagnosticStyle.default:
            print("{}: {}".format(get_parsable_location(location), message))
        else:
            print_error(message, location, severity)

        if severity == Severity.error and do_raise:
            raise DiagnosticError()
        elif severity == Severity.non_blocking_error:
            Diagnostics.has_pending_error = True


@dataclass(frozen=True)
class DiagnosticContext:
    """
    Shortcut to diagnostic-emitting routine calls that all share the same
    location.
    """

    location: Location | L.LktNode

    def error(self, message: str, ok_for_codegen: bool = False) -> NoReturn:
        error(message, self.location, ok_for_codegen)

    def emit_error(
        self,
        message: str,
        severity: Severity = Severity.error,
        ok_for_codegen: bool = False,
    ) -> None:
        emit_error(message, self.location, severity, ok_for_codegen)

    def non_blocking_error(
        self, message: str, ok_for_codegen: bool = False
    ) -> None:
        non_blocking_error(message, self.location, ok_for_codegen)

    def check_source_language(
        self,
        predicate: bool,
        message: str,
        severity: Severity = Severity.error,
        do_raise: bool = True,
        ok_for_codegen: bool = False,
    ) -> None:
        check_source_language(
            predicate,
            message,
            self.location,
            severity,
            do_raise,
            ok_for_codegen,
        )


@dataclass(frozen=True)
class WarningDescriptor:
    """
    Embed information about a class of warnings. Allows to log warning messages
    via the `warn_if` method.
    """

    name: str
    enabled_by_default: bool
    description: str

    @property
    def enabled(self) -> bool:
        """
        Return whether this warning is enabled in the current context.
        """
        from langkit.compile_context import get_context

        return self in get_context().warnings

    def warn_if(
        self,
        predicate: bool,
        message: str,
        location: Location | L.LktNode,
    ) -> None:
        """
        Helper around check_source_language, to raise warnings, depending on
        whether self is enabled or not in the current context.
        """
        check_source_language(
            not self.enabled or not predicate,
            message,
            severity=Severity.warning,
            location=location,
        )


class WarningSet:
    """
    Set of enabled warnings.
    """

    prop_only_entities = WarningDescriptor(
        "prop-only-entities",
        True,
        "Warn about properties that return AST nodes.",
    )
    unused_bindings = WarningDescriptor(
        "unused-bindings",
        True,
        "Warn about bindings (in properties) that are unused, or the ones used"
        " while they are declared as unused.",
    )
    unused_node_type = WarningDescriptor(
        "unused-node-type",
        True,
        "Warn if a node type is not used in the grammar, and is not marked as"
        " abstract nor synthetic.",
    )
    undocumented_public_properties = WarningDescriptor(
        "undocumented-public-properties",
        True,
        "Warn if a public property is left undocumented.",
    )
    undocumented_nodes = WarningDescriptor(
        "undocumented-nodes", True, "Warn if a node is left undocumented."
    )
    imprecise_field_type_annotations = WarningDescriptor(
        "imprecise-field-type-annotations",
        True,
        "Warn about parsing field type annotations that are not as precise as"
        " they could be.",
    )
    available_warnings = [
        prop_only_entities,
        unused_bindings,
        unused_node_type,
        undocumented_public_properties,
        undocumented_nodes,
        imprecise_field_type_annotations,
    ]

    def __init__(self) -> None:
        self.enabled_warnings = {
            w for w in self.available_warnings if w.enabled_by_default
        }

    def __repr__(self) -> str:
        return "<WarningSet [{}]>".format(
            ", ".join(w.name for w in self.enabled_warnings)
        )

    def enable(self, warning: WarningDescriptor | str) -> None:
        """
        Enable the given warning in this WarningSet instance.
        """
        warn = self.lookup(warning) if isinstance(warning, str) else warning
        self.enabled_warnings.add(warn)

    def disable(self, warning: WarningDescriptor | str) -> None:
        """
        Disable the given warning in this WarningSet instance.
        """
        warn = self.lookup(warning) if isinstance(warning, str) else warning
        self.enabled_warnings.discard(warn)

    def clone(self) -> WarningSet:
        """
        Return a copy of this WarningSet instance.
        """
        other = WarningSet()
        other.enabled_warnings = set(self.enabled_warnings)
        return other

    def with_enabled(self, warning: WarningDescriptor | str) -> WarningSet:
        """
        Return a copy of this WarningSet instance where `warning` is enabled.
        """
        other = self.clone()
        other.enable(warning)
        return other

    def with_disabled(self, warning: WarningDescriptor | str) -> WarningSet:
        """
        Return a copy of this WarningSet instance where `warning` is disabled.
        """
        other = self.clone()
        other.disable(warning)
        return other

    def __contains__(self, warning: WarningDescriptor) -> bool:
        """
        Return whether `warning` is enabled:
        """
        return warning in self.enabled_warnings

    def lookup(self, name: str) -> WarningDescriptor:
        """
        Look for the WarningDescriptor whose name is `name`. Raise a ValueError
        if none matches.
        """
        for w in self.available_warnings:
            if w.name == name:
                return w
        else:
            raise ValueError("Invalid warning: {}".format(name))

    @classmethod
    def print_list(
        cls,
        context: CompileCtx,
        out: TextIO = sys.stdout,
        width: int | None = None,
    ) -> None:
        """
        Display the list of available warnings in `f`.

        :param out: File in which the list is displayed.
        :param width: Width of the message. If None, use os.environ['COLUMNS'].
        """
        # Do not import this unless necessary: this module is in the closure of
        # GDB helpers, and we do not want them to transitively import Mako.
        import langkit.documentation

        if width is None:
            try:
                width = int(os.environ["COLUMNS"])
            except (KeyError, ValueError):
                width = 80
        print("List of available warnings:", file=out)
        for w in cls.available_warnings:
            print("", file=out)
            print("* {}:".format(w.name), file=out)
            if w.enabled_by_default:
                print("  [enabled by default]", file=out)
            print(
                langkit.documentation.format_text(
                    context, w.description, 2, width
                ),
                file=out,
            )

    @staticmethod
    def add_args(
        parser: argparse.ArgumentParser,
        dest: str = "warning_activations",
    ) -> None:
        """
        Register --enable-warning/--disable-warnings arguments in ``parser``.

        Parsing these two options will yield a mapping from warning names to a
        boolean that determines whether this warning should be enabled or
        disabled and store that mapping in the "dest" argument namespace
        attribute.
        """

        class Action(argparse.Action):
            def __init__(
                self,
                option_strings: list[str],
                dest: str,
                nargs: int | str | None = None,
                const: object = None,
                default: object = None,
                type: (
                    Callable[[str], object] | argparse.FileType | None
                ) = None,
                choices: Iterable[object] | None = None,
                required: bool = False,
                help: str | None = None,
                metavar: str | None = None,
            ):
                assert isinstance(const, bool)
                super().__init__(
                    option_strings,
                    dest,
                    nargs,
                    const,
                    default,
                    type,
                    choices,
                    required,
                    help,
                    metavar,
                )

            def __call__(
                self,
                parser: argparse.ArgumentParser,
                namespace: argparse.Namespace,
                values: str | Sequence[Any] | None,
                option_string: str | None = None,
            ) -> None:
                assert isinstance(values, str)
                mapping = getattr(namespace, self.dest)
                mapping[values] = self.const

        parser.add_argument(
            "--enable-warning",
            "-W",
            help="Activate a warning by name.",
            dest=dest,
            default={},
            action=Action,
            const=True,
        )
        parser.add_argument(
            "--disable-warning",
            "-w",
            help="Deactivate a warning by name.",
            dest=dest,
            action=Action,
            const=False,
        )


T = TypeVar("T")


def errors_checkpoint() -> None:
    """
    If there was a non-blocking error, exit the compilation process.
    """
    if Diagnostics.has_pending_error:
        Diagnostics.has_pending_error = False
        raise DiagnosticError()


@lru_cache()
def splitted_text(unit: L.AnalysisUnit) -> list[str]:
    """
    Memoized function to get the splitted text of a unit. Used to not have to
    compute this every time.
    """
    return unit.text.splitlines()


def style_diagnostic_message(string: str) -> str:
    """
    Given a diagnostic message containing possible variable references
    surrounded by backticks, style those references.
    """
    return re.sub("`.*?`", lambda m: col(m.group(), Colors.BOLD), string)


def source_listing(
    highlight_sloc: Location,
    lines_after: int = 0,
) -> str | None:
    """
    Create a source listing for an error message, centered around a specific
    sloc, that will be highlighted/careted, as in the following example::

        65 | fun test(): Int = b_inst.fun_call
           |                   ^^^^^^^^^^^^^^^

    :param highlight_sloc: The source location that will allow us
        to create the specific listing.
    :param lines_after: The number of lines to print after the given sloc.
    """

    # Make sure we have at least one line, since locations can refer to "line
    # 1" even for empty source files.
    source_buffer = splitted_text(highlight_sloc.lkt_unit) or [""]

    ret = []

    # These line numbers are 0-based, so that they can be used as indexes in
    # the list of lines.
    line_nb = highlight_sloc.line - 1
    start_offset = highlight_sloc.column - 1
    end_offset = highlight_sloc.end_column - 1

    # If the location is at the file termination, right after a line break,
    # there is no useful line to print.
    if line_nb >= len(source_buffer):
        return None

    # Compute the width of the column needed to print line numbers
    line_nb_width = len(str(highlight_sloc.line + lines_after))

    # Precompute the format string for the listing left column
    prefix_fmt = "{{: >{}}} | ".format(line_nb_width)

    def append_line(line_nb: int | None, line: str) -> None:
        """
        Append a line to the source listing, given a line number and a line.

        :param line_nb: 0-based line number corresponding to ``line``, or None
            if no line number must be printed.
        """
        # Convert the 0-based line number to a 1-based one, which is what users
        # need to see.
        displayed_line_nb = "" if line_nb is None else str(line_nb + 1)

        ret.append(
            col(
                prefix_fmt.format(displayed_line_nb, line),
                Colors.BLUE + Colors.BOLD,
            )
        )
        ret.append(line)
        ret.append("\n")

    # Append the line containing the sloc
    append_line(line_nb, source_buffer[line_nb])

    # Append the line caretting the sloc in the line above
    caret_line = "".join(
        "^" if start_offset <= i < end_offset else " "
        for i in range(len(source_buffer[line_nb]))
    ).rstrip()
    if caret_line:
        append_line(None, col(caret_line, Colors.RED + Colors.BOLD))

    # Append following lines up to ``lines_after`` lines
    for cur_line_nb, cur_line in enumerate(
        source_buffer[
            line_nb + 1 : min(line_nb + lines_after + 1, len(source_buffer))
        ],
        line_nb + 1,
    ):
        append_line(cur_line_nb, cur_line)

    return "".join(ret)


def print_error(
    message: str,
    location: Location | L.LktNode,
    severity: Severity = Severity.error,
) -> None:
    """
    Prints an error.
    """
    if severity == Severity.warning:
        name = "warning"
        color = Colors.YELLOW
    else:
        name = "error"
        color = Colors.RED
    error_marker = col("{}: ".format(name), color + Colors.BOLD)

    location = Location.resolve(location)

    if not location.file:
        print(error_marker + message)
        return

    # Print the basic error (with colors if in tty)
    print(
        "{}: {}{}".format(
            col(location.gnu_style_repr(), Colors.BOLD),
            error_marker,
            style_diagnostic_message(message),
        ),
    )

    # Print the source listing
    if location.lkt_unit is not None and location.line > 0:
        src_lst = source_listing(location)
        if src_lst is not None:
            print(src_lst)


def print_error_from_sem_result(sdiag: L.SolverDiagnostic) -> None:
    """
    Emit an error from an Lkt semantic result.
    """
    non_blocking_error(
        sdiag.message_template.format(*sdiag.args), location=sdiag.location
    )
