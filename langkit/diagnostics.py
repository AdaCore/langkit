from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass, field, replace
import enum
from functools import lru_cache
import os
import os.path as P
import re
import sys
import traceback
from typing import (
    Any, ClassVar, Iterator, NoReturn, TYPE_CHECKING, TextIO, Type, TypeVar
)


liblktlang_available = True
try:
    import liblktlang as L
except (ImportError, OSError):
    # ImportError may be raised if liblktlang is not available.
    #
    # OSError may be raised if liblktlang is available, but the underlying
    # dynamic library is missing dependency libraries. This can occur if the
    # library was built with one version of the compiler, and then the compiler
    # installation gets updated.  liblktlang has stale library links and fails
    # to load.
    #
    # In both scenarios, assume liblktlang is not available and continue.
    liblktlang_available = False


from langkit.utils import Colors, assert_type, col


if TYPE_CHECKING:
    from langkit.compiled_types import CompiledType
    from langkit.expressions import PropertyDef


class DiagnosticStyle(enum.Enum):
    """Format for diagnostics that Langkit emits: location and text."""

    default = 'default'
    """Human-readable tracebacks."""

    gnu_full = 'gnu-full'
    """Standard GNU format with full paths."""

    gnu_base = 'gnu-base'
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

    blacklisted_paths = [P.dirname(P.abspath(__file__))]
    """
    List of blacklisted paths. Add to that list to keep paths out of
    diagnostics.
    """

    blacklisted_frames: dict[str, set[int]] = {}
    """
    Mapping from filename to set of line numbers for all stack frames to
    blacklist from DSL locations.
    """

    @classmethod
    def blacklist_frame(cls, frame: traceback.FrameSummary) -> None:
        """
        Add the given frame to blacklisted ones for DSL locations.
        """
        if isinstance(frame.lineno, int):
            filename = P.normpath(frame.filename)
            lineno_set = cls.blacklisted_frames.setdefault(filename, set())
            lineno_set.add(frame.lineno)

    @classmethod
    def is_langkit_dsl(cls, frame: traceback.FrameSummary) -> bool:
        """
        Return whether to exclude the given frame from locations used to create
        diagnostics.
        """
        # If the path of the file is in the list of blacklisted paths, then
        # it's definitely not part of the language spec.
        python_file = P.normpath(frame.filename)
        if any(path in python_file for path in cls.blacklisted_paths):
            return False

        # Never use blacklisted stack frames to create DSL locations
        try:
            linenos = cls.blacklisted_frames[python_file]
        except KeyError:
            pass
        else:
            if frame.lineno in linenos:
                return False

        # The "manage.py" script is supposed to define settings for the
        # language spec, but is not the language spec itself.
        if "manage.py" in python_file:
            return False

        # Reject Python internals, definitely not part of the language spec
        # neither. For instance: "<frozen importlib._bootstrap>".
        if python_file.startswith("<"):
            return False

        return True

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
    def from_sloc_range(cls,
                        unit: L.AnalysisUnit,
                        sloc: L.SlocRange) -> Location:
        """
        Create a Location based on a LKT SlocRange and AnalysisUnit.
        """
        return cls(
            unit.filename,
            sloc.start.line,
            sloc.start.column,
            sloc.end.line,
            sloc.end.column,
            unit
        )

    @classmethod
    def from_lkt_node(cls, node: L.LktNode) -> Location:
        """
        Create a Location based on a Lkt node.
        """
        return cls.from_sloc_range(node.unit, node.sloc_range)

    @classmethod
    def from_lkt_node_or_none(cls, node: L.LktNode | None) -> Location | None:
        """
        Create a Location based on a Lkt node. Accept null nodes: return a null
        location in that case.
        """
        return None if node is None else cls.from_lkt_node(node)

    @classmethod
    def for_entity_doc(
        cls,
        entity: CompiledType | PropertyDef,
    ) -> Location | None:
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
        if result is not None:
            result = replace(result, line=result.line + 2)
        return result

    @staticmethod
    def resolve(location: Location | L.LktNode) -> Location:
        """
        If "location" is a node from liblktlang, turn it into a Location
        instance. Note that this possible only if liblktlang is available, so
        we know that we'll have a Location instance afterwards.
        """
        if liblktlang_available and isinstance(location, L.LktNode):
            return Location.from_lkt_node(location)
        else:
            assert isinstance(location, Location)
            return location

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


Location.builtin = Location("<builtin>")
Location.nowhere = Location("")


def extract_library_location(
    stack: list[Any] | None = None
) -> Location | None:
    """
    Extract the location of the definition of an entity in the language
    specification from a stack trace. Use `traceback.extract_stack()` if no
    stack is provided.
    """
    stack = stack or traceback.extract_stack()

    # Create Location instances for each stack frame
    locs = [Location(file=t.filename, line=t.lineno)
            for t in stack
            if isinstance(t.lineno, int) and Diagnostics.is_langkit_dsl(t)]

    return locs[-1] if locs else None


context_stack: list[Location | None] = []


@contextmanager
def diagnostic_context(location: Location | None) -> Iterator[None]:
    """
    Context manager to temporarily append a location to the context stack for
    diagnostics.

    :param location: The location for diagnostics.
    """
    if location is not None:
        context_stack.append(location)
        try:
            yield
        finally:
            context_stack.pop()
    else:
        yield


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
    Severity.warning:            Colors.YELLOW,
    Severity.error:              Colors.RED,
    Severity.non_blocking_error: Colors.RED,
}


def format_severity(severity: Severity) -> str:
    msg = ('Error'
           if severity == Severity.non_blocking_error else
           severity.name.capitalize())
    return col(msg, Colors.BOLD + SEVERITY_COLORS[severity])


def get_structured_context() -> list[Location]:
    """
    From the context global structures, return a structured context locations
    list.
    """
    return list(l for l in reversed(context_stack) if l)


def get_current_location() -> Location:
    ctx = get_structured_context()
    assert ctx
    return ctx[0]


def get_parsable_location(location: Location | L.LktNode) -> str:
    """
    Returns an error location in the common tool parsable format::

        {file}:{line}:{column}

    Depending on the diagnostic style enabled, `file` will be a base name or a
    full path. Note that this should not be run when `DiagnosticStyle.default`
    is enabled.

    :rtype: str
    """
    assert Diagnostics.style != DiagnosticStyle.default
    loc = get_current_location()
    if loc:
        path = (P.abspath(loc.file)
                if Diagnostics.style == DiagnosticStyle.gnu_full else
                P.basename(loc.file))
        return "{}:{}:1".format(path, loc.line)
    else:
        return ""


def error(message: str, ok_for_codegen: bool = False) -> NoReturn:
    """
    Shortcut around ``check_source_language``, for fatal errors.
    """
    check_source_language(False, message, ok_for_codegen=ok_for_codegen)
    # NOTE: The following raise is useless, but is there because mypy is not
    # clever enough to know  that the previous call will never return.
    raise AssertionError("should not happen")


def non_blocking_error(message: str, ok_for_codegen: bool = False) -> None:
    """
    Shortcut around ``check_source_language``, for non-fatal errors.
    """
    check_source_language(
        False,
        message,
        ok_for_codegen=ok_for_codegen,
        severity=Severity.non_blocking_error,
    )


def check_source_language(predicate: bool,
                          message: str,
                          severity: Severity = Severity.error,
                          do_raise: bool = True,
                          ok_for_codegen: bool = False) -> None:
    """
    Check predicates related to the user's input in the input language
    definition. Show error messages and eventually terminate if those error
    messages are critical.

    :param predicate: The predicate to check.
    :param message: The base message to display if predicate happens to be
        false.
    :param severity: The severity of the diagnostic.
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
        assert ctx is None or ctx.emitter is None

    severity = assert_type(severity, Severity)
    indent = ' ' * 4

    if not predicate:
        message_lines = message.splitlines()
        message = '\n'.join(
            message_lines[:1] + [indent + line for line in message_lines[1:]]
        )

        location = get_current_location()
        if Diagnostics.style != DiagnosticStyle.default:
            print('{}: {}'.format(get_parsable_location(location), message))
        else:
            print_error(message, location, severity)

        if severity == Severity.error and do_raise:
            raise DiagnosticError()
        elif severity == Severity.non_blocking_error:
            Diagnostics.has_pending_error = True


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

    def warn_if(self, predicate: bool, message: str) -> None:
        """
        Helper around check_source_language, to raise warnings, depending on
        whether self is enabled or not in the current context.
        """
        check_source_language(not self.enabled or not predicate, message,
                              severity=Severity.warning)


class WarningSet:
    """
    Set of enabled warnings.
    """

    prop_only_entities = WarningDescriptor(
        'prop-only-entities', True,
        'Warn about properties that return AST nodes.'
    )
    unused_bindings = WarningDescriptor(
        'unused-bindings', True,
        'Warn about bindings (in properties) that are unused, or the ones used'
        ' while they are declared as unused.'
    )
    unparser_bad_grammar = WarningDescriptor(
        'unparser-bad-grammar', False,
        'Warn if the grammar is not amenable to the automatic generation of an'
        ' unparser.'
    )
    unused_node_type = WarningDescriptor(
        'unused-node-type', True,
        'Warn if a node type is not used in the grammar, and is not marked as'
        ' abstract nor synthetic.'
    )
    undocumented_public_properties = WarningDescriptor(
        'undocumented-public-properties', True,
        'Warn if a public property is left undocumented.'
    )
    undocumented_nodes = WarningDescriptor(
        'undocumented-nodes', True,
        'Warn if a node is left undocumented.'
    )
    imprecise_field_type_annotations = WarningDescriptor(
        'imprecise-field-type-annotations', True,
        'Warn about parsing field type annotations that are not as precise as'
        ' they could be.'
    )
    available_warnings = [
        prop_only_entities, unused_bindings, unparser_bad_grammar,
        unused_node_type, undocumented_public_properties, undocumented_nodes,
        imprecise_field_type_annotations,
    ]

    def __init__(self) -> None:
        self.enabled_warnings = {w for w in self.available_warnings
                                 if w.enabled_by_default}

    def __repr__(self) -> str:
        return '<WarningSet [{}]>'.format(', '.join(
            w.name for w in self.enabled_warnings
        ))

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
            raise ValueError('Invalid warning: {}'.format(name))

    @classmethod
    def print_list(cls,
                   out: TextIO = sys.stdout,
                   width: int | None = None) -> None:
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
                width = int(os.environ['COLUMNS'])
            except (KeyError, ValueError):
                width = 80
        print('List of available warnings:', file=out)
        for w in cls.available_warnings:
            print('', file=out)
            print('* {}:'.format(w.name), file=out)
            if w.enabled_by_default:
                print('  [enabled by default]', file=out)
            print(langkit.documentation.format_text(w.description, 2, width),
                  file=out)


def check_multiple(predicates_and_messages: list[tuple[bool, str]],
                   severity: Severity = Severity.error) -> None:
    """
    Helper around check_source_language, check multiple predicates at once.

    :param predicates_and_messages: List of diagnostic tuples.
    :param severity: The severity of the diagnostics.
    """
    for predicate, message in predicates_and_messages:
        check_source_language(predicate, message, severity)


T = TypeVar('T')


def check_type(obj: Any, typ: Type[T], message: str | None = None) -> T:
    """
    Like utils.assert_type, but produces a client error instead.

    :param obj: The object to check.
    :param typ: The expected type of obj.
    :param str|None message: The base message to display if type check fails.
    """
    try:
        return assert_type(obj, typ)
    except AssertionError as e:
        message = "{}\n{}".format(e.args[0], message) if message else e.args[0]
        error(message)


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
    source_buffer = splitted_text(highlight_sloc.lkt_unit) or ['']

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

        ret.append(col(prefix_fmt.format(displayed_line_nb, line),
                       Colors.BLUE + Colors.BOLD))
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
        source_buffer[line_nb + 1:
                      min(line_nb + lines_after + 1, len(source_buffer))],
        line_nb + 1
    ):
        append_line(cur_line_nb, cur_line)

    return "".join(ret)


def print_error(message: str,
                location: Location | L.LktNode,
                severity: Severity = Severity.error) -> None:
    """
    Prints an error.
    """
    if severity == Severity.warning:
        name = 'warning'
        color = Colors.YELLOW
    else:
        name = 'error'
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


def print_error_from_sem_result(sem_result: L.SemanticResult) -> None:
    """
    Emit an error from an Lkt semantic result.
    """
    with diagnostic_context(
        Location.from_lkt_node(sem_result.node)
    ):
        check_source_language(False,
                              sem_result.error_message,
                              severity=Severity.non_blocking_error)
