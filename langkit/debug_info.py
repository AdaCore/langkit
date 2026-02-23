"""
Data structures for mapping from generated library source lines to the
properties DSL level.
"""

from __future__ import annotations

import abc
from collections import defaultdict
import inspect
import os.path
import shlex
from typing import Callable, Iterable, Self, TYPE_CHECKING, Type, cast

from langkit.gdb.state import Binding, ExpressionEvaluation


if TYPE_CHECKING:
    from langkit.gdb.context import Context
    from langkit.gdb.state import ScopeState


class ParseError(Exception):
    def __init__(self, line_no: int, message: str):
        super().__init__("line {}: {}".format(line_no, message))


class DebugInfo:
    """
    Holder for all info that maps generated code to the properties DSL level.
    """

    def __init__(self, context: Context | None):
        self.context = context
        """
        Reference to the library-specific context, if in GDB. None otherwise
        (e.g. when parsing debug info outside a debug session).
        """

        self.filenames: set[str] = set()
        """
        Absolute path to bodies for implementation packages, i.e. for any
        generated code that may contain GDB helper directives.
        """

        self.properties: list[Property] = []

        self.properties_by_filename: dict[str, list[Property]] = defaultdict(
            list
        )

        self.properties_dict: dict[str, Property] = {}
        """
        Name-based lookup dictionnary for properties.
        """

    @classmethod
    def parse_from_gdb(cls, context: Context) -> DebugInfo:
        """
        Try to parse the $-implementation.adb source file that GDB found.

        This extracts mapping information from its GDB helpers directives.
        Print error messages on standard output if anything goes wrong, but
        always return a DebugInfo instance anyway.
        """
        import gdb

        result = cls(context)

        # Look for the "$-implementation.adb" file using some symbol that is
        # supposed to be defined there.
        has_unit_sym = gdb.lookup_global_symbol(
            "{}__implementation__is_null".format(context.lib_name)
        )
        if not has_unit_sym:
            return result

        filename = has_unit_sym.symtab.fullname()
        with open(filename, "r") as f:
            result._try_parse(filename, f)

        return result

    @classmethod
    def parse_from_iterable(
        cls, filename: str, lines: Iterable[str]
    ) -> DebugInfo:
        """
        Like parse_from_gdb, but parsing from ``lines``.

        :param filename: Name of the file from which we read the sources.  Used
            for diagnostics purposes.
        :param lines: Iterable that yields all the lines to parse.  This can be
            any iterator: a read file, a list of strings in memory, a custom
            iterator, ...
        """
        result = cls(context=None)
        result._try_parse(filename, lines)
        return result

    def _try_parse(self, filename: str, lines: Iterable[str]) -> None:
        """
        Internal method. Same semantics as parse_from_iterable, but work on an
        existing instance.
        """
        self.filenames.add(filename)
        try:
            self._parse_file(filename, lines)
        except ParseError as exc:
            print("Error while parsing directives in {}:".format(filename))
            print(str(exc))

    def _parse_file(self, filename: str, lines: Iterable[str]) -> None:
        """
        Internal method. Read GDB helpers directives from the "lines" source
        file and fill self according to it. Raise a ParseError if anything goes
        wrong.

        :param filename: Name of the file from which we read the sources.  Used
            for diagnostics purposes.
        :param lines: Iterable that yields all the lines to parse.  This can be
            any iterator: a read file, a list of strings in memory, a custom
            iterator, ...
        """
        scope_stack: list[Scope] = []
        expr_stack: list[ExprStart] = []

        for line_no, line in enumerate(lines, 1):
            line = line.strip()
            if not line.startswith("--#"):
                continue
            line = line[3:].strip()
            args = shlex.split(line)

            try:
                name = args.pop(0)
            except IndexError:
                raise ParseError(line_no, "directive name is missing")

            loc = AdaLocation(filename, line_no)
            d = Directive.parse_dispatch(loc, name, args)

            if isinstance(d, PropertyStart):
                if scope_stack:
                    raise ParseError(
                        line_no,
                        "property-start directive not"
                        " allowed inside another property",
                    )
                p = Property(
                    loc.to_range(None),
                    d.name,
                    d.lkt_sloc,
                    d.is_dispatcher,
                )
                self.properties.append(p)
                self.properties_by_filename[filename].append(p)
                self.properties_dict[p.name] = p
                scope_stack.append(p)

            elif isinstance(
                d, (ScopeStart, PropertyCallStart, MemoizationLookupDirective)
            ):
                if not scope_stack or not isinstance(scope_stack[-1], Scope):
                    raise ParseError(
                        line_no,
                        "{} directive must occur inside a property or a"
                        " property scope".format(d.directive_name),
                    )

                line_range = d.loc.to_range(None)

                new_scope: Scope
                if isinstance(d, MemoizationLookupDirective):
                    new_scope = MemoizationLookup(line_range)
                elif isinstance(d, PropertyCallStart):
                    new_scope = PropertyCall(line_range, d.name)
                else:
                    assert isinstance(d, ScopeStart)
                    new_scope = Scope(line_range)

                scope_stack.append(new_scope)

            elif isinstance(d, End):
                if not scope_stack:
                    raise ParseError(line_no, "no scope to end")
                ended_scope = scope_stack.pop()
                ended_scope.line_range.last_line = d.loc.line_no
                if scope_stack:
                    assert isinstance(scope_stack[-1], Scope)
                    scope_stack[-1].events.append(ended_scope)
                else:
                    assert isinstance(
                        ended_scope, Property
                    ), "Top-level scopes must all be properties"
                    assert not expr_stack, (
                        "Some expressions are not done when leaving property"
                        " {}: {}".format(
                            ended_scope.name,
                            ", ".join(str(e) for e in expr_stack),
                        )
                    )

            elif isinstance(d, BindDirective):
                if not scope_stack:
                    raise ParseError(line_no, "no scope for binding")
                assert isinstance(scope_stack[-1], Scope)
                scope_stack[-1].events.append(
                    Bind(d.loc, d.lkt_name, d.gen_name)
                )

            elif isinstance(d, ExprStartDirective):
                if not scope_stack:
                    raise ParseError(line_no, "no scope for expression")
                start_event = ExprStart(
                    d.loc, d.expr_id, d.expr_repr, d.result_var, d.lkt_sloc
                )
                assert isinstance(scope_stack[-1], Scope)
                scope_stack[-1].events.append(start_event)
                if expr_stack:
                    expr_stack[-1].sub_expr_start.append(start_event)
                expr_stack.append(start_event)

            elif isinstance(d, ExprDoneDirective):
                if not scope_stack:
                    raise ParseError(line_no, "no scope for expression")
                done_event = ExprDone(d.loc, d.expr_id)
                start_event = expr_stack.pop()
                assert (
                    start_event.expr_id == done_event.expr_id
                ), "Mismatching ExprStart/ExprStop events: {} and {}".format(
                    start_event, done_event
                )
                start_event._done_event = done_event
                assert isinstance(scope_stack[-1], Scope)
                scope_stack[-1].events.append(done_event)

            elif isinstance(d, MemoizationReturnDirective):
                if not scope_stack or not isinstance(
                    scope_stack[-1], MemoizationLookup
                ):
                    raise ParseError(
                        line_no,
                        "memoization-result directive must appear inside a"
                        " memoization-lookup scope",
                    )
                scope_stack[-1].events.append(MemoizationReturn(d.loc))

            elif isinstance(d, PropertyBodyStart):
                if not scope_stack or not isinstance(scope_stack[0], Property):
                    raise ParseError(
                        line_no,
                        "property-body-start directive must appear inside a"
                        " property",
                    )
                scope_stack[0].body_start = d.loc

            else:
                raise NotImplementedError("Unknown directive: {}".format(d))

        if scope_stack:
            raise ParseError(
                line_no, "end of scope expected before end of" " file"
            )

    def lookup_property(self, loc: AdaLocation) -> Property | None:
        """
        Look for a property that covers the given source line number. Return
        None if there is no such property.

        :param line_no: Line number to lookup.
        """
        try:
            properties = self.properties_by_filename[loc.filename]
        except KeyError:
            return None

        for p in properties:
            if loc in p.line_range:
                return p
        return None

    def get_property_by_name(self, name: str) -> Property:
        """
        Fetch the property called `name`. Raise an error if not found.

        :param name: Name of the property to fetch.
        """
        return self.properties_dict[name]


class BaseLocation:
    """
    Location of a line in a source file.
    """

    def __init__(self, filename: str, line_no: int):
        self.filename = filename
        self.line_no = line_no

    @classmethod
    def parse(cls, spec: str) -> Self | None:
        """
        If `spec` is "None", return None. Otherwise, create a BaseLocation
        instance out of a string of the form "filename:line_no".
        """
        if spec == "None":
            return None

        try:
            filename, line_no = spec.rsplit(":", 1)
        except ValueError:
            raise ValueError("Invalid DSL location: {}".format(spec))

        try:
            line_no_int = int(line_no)
        except ValueError:
            raise ValueError("Invalid line number: {}".format(line_no))

        return cls(filename, line_no_int)

    def matches(self, other: LktLocation) -> bool:
        """
        Return whether `self` designates a file at least as much specifically
        as `other`. In practice, both must have the same line number and the
        filename from `other` must be a suffix for the one in `self`.
        """
        return self.line_no == other.line_no and self.filename.endswith(
            other.filename
        )

    def is_after(self, other: AdaLocation) -> bool:
        """
        Return whether `self` belongs to the same file as `other` and
        designates a line number greater than `other`'s.
        """
        return self.filename == other.filename and self.line_no > other.line_no

    def __str__(self) -> str:
        return "{}:{}".format(os.path.basename(self.filename), self.line_no)

    def __repr__(self) -> str:
        return f"<{type(self).__name__} {self}>"


class AdaLocation(BaseLocation):
    """
    Location of a line in an Ada source file.
    """

    def to_range(self, last_line: int | None) -> LineRange:
        """
        TODO.
        """
        return LineRange(self.filename, self.line_no, last_line)

    @property
    def gdb_spec(self) -> str:
        """
        Return the GDB spec to designate this location.
        """
        return str(self)


class LktLocation(BaseLocation):
    """
    Location of a line in a Lkt source file.
    """


class LineRange:
    """
    Range of lines in the generated Ada source code.
    """

    def __init__(self, filename: str, first_line: int, last_line: int | None):
        self.filename = filename
        self.first_line = first_line
        self.last_line = last_line

    def __contains__(self, loc: AdaLocation) -> bool:
        assert isinstance(loc, AdaLocation)
        assert isinstance(self.last_line, int)
        return (
            loc.filename == self.filename
            and self.first_line <= loc.line_no <= self.last_line
        )

    @property
    def first_loc(self) -> AdaLocation:
        return AdaLocation(self.filename, self.first_line)

    def __repr__(self) -> str:
        return "<LineRange {}>".format(self)

    def __str__(self) -> str:
        return "{}-{}".format(self.first_line, self.last_line)


class BaseEvent(abc.ABC):
    pass


class Scope(BaseEvent):
    def __init__(self, line_range: LineRange, label: str | None = None):
        self.line_range = line_range
        self.label = label
        self.events: list[BaseEvent] = []

    @property
    def subscopes(self) -> list[Scope]:
        return [e for e in self.events if isinstance(e, Scope)]

    def iter_events(
        self,
        recursive: bool = True,
        filter: Type[BaseEvent] | Callable[[BaseEvent], bool] | None = None,
    ) -> Iterable[BaseEvent]:
        """
        Iterate through all events in this scope and, if `recursive`, all
        sub-scopes. Events are yielded in source order.

        :param filter: If left to None, don't filter the results. If it's a
            class, return only instances of this class. Otherwise, treat it as
            a predicate function and return only items for which the predicate
            returns true.
        """

        def predicate(e: BaseEvent) -> bool:
            if filter is None:
                return True
            elif inspect.isclass(filter):
                assert isinstance(filter, type)
                return isinstance(e, filter)
            else:
                return cast(Callable[[BaseEvent], bool], filter)(e)

        for e in self.events:
            if isinstance(e, Scope):
                if predicate(e):
                    yield e
                if recursive:
                    for sub_e in e.iter_events(filter=filter):
                        yield sub_e
            else:
                if predicate(e):
                    yield e

    def __repr__(self) -> str:
        return "<{}{} {}>".format(
            type(self).__name__,
            " {}".format(self.label) if self.label else "",
            self.line_range,
        )


class PropertyCall(Scope):
    def __init__(self, line_range: LineRange, name: str):
        super().__init__(line_range, name)
        self.name = name

    def property(self, context: Context) -> Property:
        """
        Look for the property that this property call targets.
        """
        return context.debug_info.get_property_by_name(self.name)


class Property(Scope):
    def __init__(
        self,
        line_range: LineRange,
        name: str,
        lkt_sloc: LktLocation | None,
        is_dispatcher: bool,
    ):
        super().__init__(line_range, name)
        self.name = name
        self.lkt_sloc = lkt_sloc
        self.is_dispatcher = is_dispatcher

        self.body_start: AdaLocation | None = None
        """
        Source location where to put breakpoints for the beginning of this
        property, or None if this property has no code.
        """

    @property
    def memoization_lookup(self) -> MemoizationLookup | None:
        """
        Return None if this property is not memoized. Otherwise, return the
        special scope that covers code that is about to return based on a
        memoization cached result. To be used when putting breakpoints at the
        beginning of properties.
        """
        for e in self.events:
            if isinstance(e, MemoizationLookup):
                return e
        return None


class Event(BaseEvent):
    def __init__(self, loc: AdaLocation, entity: str | None = None):
        self.loc = loc
        self.entity = entity

    @abc.abstractmethod
    def apply_on_state(self, scope_state: ScopeState) -> None:
        """
        Modify the input state according to the effect this event has. All
        subclasses must override this.
        """
        ...

    def __repr__(self) -> str:
        return "<Event line {}>".format(self.loc)


class Bind(Event):
    def __init__(self, loc: AdaLocation, lkt_name: str, gen_name: str):
        super().__init__(loc, lkt_name)
        self.lkt_name = lkt_name
        self.gen_name = gen_name

    def apply_on_state(self, scope_state: ScopeState) -> None:
        scope_state.bindings.append(Binding(self.lkt_name, self.gen_name))

    def __repr__(self) -> str:
        return "<Bind {}, {}>".format(self.lkt_name, self.loc)


class ExprStart(Event):
    def __init__(
        self,
        loc: AdaLocation,
        expr_id: str,
        expr_repr: str,
        result_var: str,
        lkt_sloc: LktLocation | None,
    ):
        super().__init__(loc)
        self.expr_id = expr_id
        self.expr_repr = expr_repr
        self.result_var = result_var
        self.lkt_sloc = lkt_sloc

        self.sub_expr_start: list[ExprStart] = []
        """
        List of ExprStart events that occur before the ExprDone corresponding
        to `self`.
        """

        self._done_event: ExprDone | None = None
        """
        Done event corresponding to this ExprStart event.
        """

    @property
    def done_event(self) -> ExprDone:
        """
        Return the ExprDone event that corresponds to `self`.
        """
        assert self._done_event
        return self._done_event

    @property
    def line_range(self) -> LineRange:
        """
        Return the line range that spans from this ExprStart event to the
        corresponding ExprDone one.
        """
        assert self.loc.filename == self.done_event.loc.filename
        return LineRange(
            self.loc.filename, self.loc.line_no, self.done_event.loc.line_no
        )

    def apply_on_state(self, scope_state: ScopeState) -> None:
        assert self.expr_id not in scope_state.expressions
        expr = ExpressionEvaluation(self)
        scope_state.expressions[self.expr_id] = expr
        if scope_state.state.started_expressions:
            scope_state.state.started_expressions[-1].append_sub_expr(expr)
        scope_state.state.started_expressions.append(expr)

    def __repr__(self) -> str:
        return "<ExprStart {}, {}>".format(self.expr_id, self.loc)


class ExprDone(Event):
    def __init__(self, loc: AdaLocation, expr_id: str):
        super().__init__(loc)
        self.expr_id = expr_id

    def apply_on_state(self, scope_state: ScopeState) -> None:
        expr = scope_state.expressions[self.expr_id]

        pop = scope_state.state.started_expressions.pop()
        assert pop == expr, (
            "Expressions are not properly nested: {} is done before {}"
            " is".format(expr, pop)
        )
        expr.set_done(self.loc)

    def __repr__(self) -> str:
        return "<ExprDone {}, {}>".format(self.expr_id, self.loc)


class MemoizationLookup(Scope):
    pass


class MemoizationReturn(Event):
    def apply_on_state(self, scope_state: ScopeState) -> None:
        pass

    def __repr__(self) -> str:
        return "<MemoizationReturn, {}>".format(self.loc)


class Directive:
    """
    Holder for GDB helper directives as parsed from source files.
    """

    name_to_cls: dict[str, Type[Directive]] = {}

    def __init__(self, loc: AdaLocation):
        self.loc = loc

    @property
    def directive_name(self) -> str:
        """
        Return the name of this directive.
        """
        for name, directive_type in self.name_to_cls.items():
            if isinstance(self, directive_type):
                return name
        raise KeyError("Unknown directive: {}".format(self))

    @classmethod
    def parse_dispatch(
        cls, loc: AdaLocation, name: str, args: list[str]
    ) -> Directive:
        """
        Try to parse a GDB helper directive. Raise a ParseError if anything
        goes wrong.

        :param loc: Location at which this directive appears in the generated
            ada code.
        :param name: Name of the directive.
        :param args: Arguments for this directive.
        """
        try:
            subcls = cls.name_to_cls[name]
        except KeyError:
            raise ParseError(loc.line_no, "invalid directive: {}".format(name))
        return subcls.parse(loc, args)

    @classmethod
    @abc.abstractmethod
    def parse(cls, loc: AdaLocation, agrs: list[str]) -> Directive:
        """
        Subclasses must override this to parse the directive.
        """
        ...


class PropertyStart(Directive):
    def __init__(
        self,
        name: str,
        lkt_sloc: LktLocation | None,
        is_dispatcher: bool,
        loc: AdaLocation,
    ):
        super().__init__(loc)
        self.name = name
        self.lkt_sloc = lkt_sloc
        self.is_dispatcher = is_dispatcher

    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> PropertyStart:
        name, info = args
        if info == "dispatcher":
            is_dispatcher = True
            lkt_sloc = None
        else:
            is_dispatcher = False
            lkt_sloc = LktLocation.parse(info)
        return cls(name, lkt_sloc, is_dispatcher, loc)


class PropertyBodyStart(Directive):
    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> PropertyBodyStart:
        assert len(args) == 0
        return cls(loc)


class PropertyCallStart(Directive):
    def __init__(self, name: str, loc: AdaLocation):
        super().__init__(loc)
        self.name = name

    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> PropertyCallStart:
        (name,) = args
        return cls(name, loc)


class MemoizationLookupDirective(Directive):
    @classmethod
    def parse(
        cls, loc: AdaLocation, args: list[str]
    ) -> MemoizationLookupDirective:
        assert len(args) == 0
        return cls(loc)


class MemoizationReturnDirective(Directive):
    @classmethod
    def parse(
        cls, loc: AdaLocation, args: list[str]
    ) -> MemoizationReturnDirective:
        assert len(args) == 0
        return cls(loc)


class ScopeStart(Directive):
    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> ScopeStart:
        return cls(loc)


class BindDirective(Directive):
    def __init__(self, lkt_name: str, gen_name: str, loc: AdaLocation):
        super().__init__(loc)
        self.lkt_name = lkt_name
        self.gen_name = gen_name

    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> BindDirective:
        lkt_name, gen_name = args
        return cls(lkt_name, gen_name, loc)


class End(Directive):
    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> End:
        return cls(loc)


class ExprStartDirective(Directive):
    def __init__(
        self,
        expr_id: str,
        expr_repr: str,
        result_var: str,
        lkt_sloc: LktLocation | None,
        loc: AdaLocation,
    ):
        super().__init__(loc)
        self.expr_id = expr_id
        self.expr_repr = expr_repr
        self.result_var = result_var
        self.lkt_sloc = lkt_sloc

    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> ExprStartDirective:
        expr_id, expr_repr, result_var, lkt_sloc = args
        return cls(
            expr_id,
            f"<{expr_repr} at {lkt_sloc}>",
            result_var,
            LktLocation.parse(lkt_sloc),
            loc,
        )


class ExprDoneDirective(Directive):
    def __init__(self, expr_id: str, loc: AdaLocation):
        super().__init__(loc)
        self.expr_id = expr_id

    @classmethod
    def parse(cls, loc: AdaLocation, args: list[str]) -> ExprDoneDirective:
        (expr_id,) = args
        return cls(expr_id, loc)


Directive.name_to_cls.update(
    {
        "property-start": PropertyStart,
        "property-body-start": PropertyBodyStart,
        "property-call-start": PropertyCallStart,
        "memoization-lookup": MemoizationLookupDirective,
        "memoization-return": MemoizationReturnDirective,
        "scope-start": ScopeStart,
        "bind": BindDirective,
        "end": End,
        "expr-start": ExprStartDirective,
        "expr-done": ExprDoneDirective,
    }
)
