from __future__ import annotations

from dataclasses import dataclass, field
import enum
from functools import lru_cache
import os
import os.path as P
import re
import sys
import traceback
from typing import (
    Any, List, NoReturn, Optional as Opt, TextIO, Tuple, Type, TypeVar, Union
)


try:
    import liblktlang as L
except ImportError:
    pass


import langkit.documentation
from langkit.utils import Colors, assert_type, col


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

    @classmethod
    def is_langkit_dsl(cls, python_file: str) -> bool:
        """
        Return wether `python_file` is langkit DSL.
        """
        # We check that the path of the file is not in the list of blacklisted
        # paths.
        python_file = P.normpath(python_file)
        return all(path not in python_file for path in cls.blacklisted_paths)

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

    line: int
    """
    Line number (1-based).
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

    lkt_unit: Opt[L.AnalysisUnit] = field(default=None)

    def gnu_style_repr(self, relative: bool = True) -> str:
        """
        Return a GNU style representation for this Location, in the form::

            file:line:column

        :param relative: When True, the file path will be relative.
        """
        return ":".join([
            P.basename(self.file) if relative else self.file,
            str(self.line),
        ] + ([str(self.column)] if self.column > 0 else []))

    @classmethod
    def from_lkt_node(cls, node: L.LKNode) -> Location:
        """
        Create a Location based on a LKT node.
        """
        return cls(
            node.unit.filename,
            node.sloc_range.start.line,
            node.sloc_range.start.column,
            node.sloc_range.end.line,
            node.sloc_range.end.column,
            node.unit
        )


def extract_library_location(stack: Opt[List[Any]] = None) -> Opt[Location]:
    """
    Extract the location of the definition of an entity in the language
    specification from a stack trace. Use `traceback.extract_stack()` if no
    stack is provided.
    """
    stack = stack or traceback.extract_stack()

    # Create Location instances for each stack frame
    locs = [Location(file=t[0], line=t[1])
            for t in stack
            if Diagnostics.is_langkit_dsl(t[0]) and "manage.py" not in t[0]]

    return locs[-1] if locs else None


context_stack: List[Location] = []


class Context:
    """
    Add context for diagnostics. For the moment this context is constituted
    of a message and a location.
    """

    def __init__(self, location: Location):
        """
        :param Location location: The location associated to the context.
        """
        self.location = location

    def __enter__(self) -> None:
        context_stack.append(self.location)

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None:
        del traceback
        del exc_type
        context_stack.pop()

    def __repr__(self) -> str:
        return '<diagnostics.Context location={}>'.format(self.location)


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


def get_structured_context() -> List[Location]:
    """
    From the context global structures, return a structured context locations
    list.
    """
    return list(reversed(context_stack))


def get_current_location() -> Opt[Location]:
    ctx = get_structured_context()
    return ctx[0] if ctx else None


def get_parsable_location() -> str:
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


def error(message: str) -> NoReturn:
    """
    Shortcut around ``check_source_language``, for fatal errors.
    """
    check_source_language(False, message)
    # NOTE: The following raise is useless, but is there because mypy is not
    # clever enough to know  that the previous call will never return.
    raise AssertionError("should not happen")


def check_source_language(predicate: bool,
                          message: str,
                          severity: Severity = Severity.error,
                          do_raise: bool = True,
                          ok_for_codegen: bool = False) -> None:
    """
    Check predicates related to the user's input in the input language
    definition. Show error messages and eventually terminate if those error
    messages are critical.

    :param bool predicate: The predicate to check.
    :param str message: The base message to display if predicate happens to
        be false.
    :param Severity severity: The severity of the diagnostic.
    :param bool do_raise: If True, raise a DiagnosticError if predicate happens
        to be false.
    :param bool ok_for_codegen: If True, allow checks to be performed during
        code generation. This is False by default as it should be an
        exceptional situation: we want, when possible, most checks to be
        performed before we attempt to emit the generated library (for
        --check-only).
    """
    from langkit.compile_context import get_context

    if not ok_for_codegen:
        ctx = get_context(or_none=True)
        assert ctx is None or ctx.emitter is None

    severity = assert_type(severity, Severity)
    indent = ' ' * 4

    if not predicate:
        message_lines = message.splitlines()
        message = '\n'.join(
            message_lines[:1] + [indent + line for line in message_lines[1:]]
        )

        if Diagnostics.style != DiagnosticStyle.default:
            print('{}: {}'.format(get_parsable_location(), message))
        else:
            print_error(message, get_current_location())

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

    def enable(self, warning: Union[WarningDescriptor, str]) -> None:
        """
        Enable the given warning in this WarningSet instance.
        """
        warn = self.lookup(warning) if isinstance(warning, str) else warning
        self.enabled_warnings.add(warn)

    def disable(self, warning: Union[WarningDescriptor, str]) -> None:
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

    def with_enabled(self,
                     warning: Union[WarningDescriptor, str]) -> WarningSet:
        """
        Return a copy of this WarningSet instance where `warning` is enabled.
        """
        other = self.clone()
        other.enable(warning)
        return other

    def with_disabled(self,
                      warning: Union[WarningDescriptor, str]) -> WarningSet:
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
                   width: Opt[int] = None) -> None:
        """
        Display the list of available warnings in `f`.

        :param out: File in which the list is displayed.
        :param width: Width of the message. If None, use os.environ['COLUMNS'].
        """
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


def check_multiple(predicates_and_messages: List[Tuple[bool, str]],
                   severity: Severity = Severity.error) -> None:
    """
    Helper around check_source_language, check multiple predicates at once.

    :param list[(bool, str)] predicates_and_messages: List of diagnostic
        tuples.
    :param Severity severity: The severity of the diagnostics.
    """
    for predicate, message in predicates_and_messages:
        check_source_language(predicate, message, severity)


T = TypeVar('T')


def check_type(obj: Any, typ: Type[T], message: Opt[str] = None) -> T:
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
def splitted_text(unit: L.AnalysisUnit) -> List[str]:
    """
    Memoized function to get the splitted text of an unit. Used to not have to
    compute this every time.
    """
    return unit.text.splitlines()


def style_diagnostic_message(string: str) -> str:
    """
    Given a diagnostic message containing possible variable references
    surrounded by backticks, style those references.
    """
    return re.sub("`.*?`", lambda m: col(m.group(), Colors.BOLD), string)


def source_listing(highlight_sloc: Location, lines_after: int = 0) -> str:
    """
    Create a source listing for an error message, centered around a specific
    sloc, that will be highlighted/careted, as in the following example::

        65 | fun test(): Int = b_inst.fun_call
           |                   ^^^^^^^^^^^^^^^

    :param highlight_sloc: The source location that will allow us
        to create the specific listing.
    :param lines_after: The number of lines to print after the given sloc.
    """

    source_buffer = splitted_text(highlight_sloc.lkt_unit)

    ret = []

    line_nb = highlight_sloc.line - 1
    start_offset = highlight_sloc.column - 1
    end_offset = highlight_sloc.end_column - 1

    # Compute the width of the column needed to print line numbers
    line_nb_width = len(str(highlight_sloc.line + lines_after))

    # Precompute the format string for the listing left column
    prefix_fmt = "{{: >{}}} | ".format(line_nb_width)

    def append_line(line_nb: Union[int, str], line: str) -> None:
        """
        Append a line to the source listing, given a line number and a line.
        """
        ret.append(col(prefix_fmt.format(line_nb, line),
                       Colors.BLUE + Colors.BOLD))
        ret.append(line)
        ret.append("\n")

    # Append the line containing the sloc
    append_line(line_nb, source_buffer[line_nb])

    # Append the line caretting the sloc in the line above
    caret_line = "".join("^" if start_offset <= i < end_offset else " "
                         for i in range(len(source_buffer[line_nb])))
    append_line("", col(caret_line, Colors.RED + Colors.BOLD))

    # Append following lines up to ``lines_after`` lines
    for line_nb, line in enumerate(
        source_buffer[line_nb + 1:
                      min(line_nb + lines_after + 1, len(source_buffer))],
        line_nb + 1
    ):
        append_line(line_nb, line)

    return "".join(ret)


def print_error(message: str,
                location: Union[Location, L.LKNode, None]) -> None:
    """
    Prints an error.
    """
    error_marker = col(col("error: ", Colors.RED), Colors.BOLD)

    if location is None:
        print(error_marker + message)
        return

    if isinstance(location, L.LKNode):
        location = Location.from_lkt_node(location)

    # Print the basic error (with colors if in tty)
    print(
        "{}: {}{}".format(
            col(location.gnu_style_repr(), Colors.BOLD),
            error_marker,
            style_diagnostic_message(message),
        ),
    )

    # Print the source listing
    if location.lkt_unit is not None:
        print(source_listing(location))


def print_error_from_sem_result(sem_result: L.SemanticResult) -> None:
    """
    Prints an error from an lkt semantic result.
    """
    print_error(sem_result.error_message,
                Location.from_lkt_node(sem_result.node))
