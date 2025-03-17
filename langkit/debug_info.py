"""
Data structures for mapping from generated library source lines to the
properties DSL level.
"""

from __future__ import annotations

import abc
import inspect
import shlex
from typing import Callable, Iterable, TYPE_CHECKING, Type, cast

from langkit.gdb.state import Binding, ExpressionEvaluation


if TYPE_CHECKING:
    from langkit.gdb.context import Context
    from langkit.gdb.state import ScopeState


class ParseError(Exception):
    def __init__(self, line_no: int, message: str):
        super().__init__('line {}: {}'.format(line_no, message))


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

        self.filename: str | None = None
        """
        Absolute path for the "$-implementation.adb" file, or None if we
        haven't found it.
        """

        self.properties: list[Property] = []

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
            '{}__implementation__is_null'.format(context.lib_name)
        )
        if not has_unit_sym:
            return result

        filename = has_unit_sym.symtab.fullname()
        with open(filename, 'r') as f:
            result._try_parse(filename, f)

        return result

    @classmethod
    def parse_from_iterable(cls,
                            filename: str,
                            lines: Iterable[str]) -> DebugInfo:
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
        self.filename = filename
        try:
            self._parse_file(lines)
        except ParseError as exc:
            print('Error while parsing directives in {}:'.format(filename))
            print(str(exc))

    def _parse_file(self, lines: Iterable[str]) -> None:
        """
        Internal method. Read GDB helpers directives from the "lines" source
        file and fill self according to it. Raise a ParseError if anything goes
        wrong.

        :param iter[str] lines: Iterable that yields all the lines to parse.
            This can be any iterator: a read file, a list of strings in memory,
            a custom iterator, ...
        :rtype: None
        """
        self.properties = []
        self.properties_dict = {}
        scope_stack: list[Scope] = []
        expr_stack: list[ExprStart] = []

        for line_no, line in enumerate(lines, 1):
            line = line.strip()
            if not line.startswith('--#'):
                continue
            line = line[3:].strip()
            args = shlex.split(line)

            try:
                name = args.pop(0)
            except IndexError:
                raise ParseError(line_no, 'directive name is missing')

            d = Directive.parse_dispatch(line_no, name, args)

            if isinstance(d, PropertyStart):
                if scope_stack:
                    raise ParseError(line_no, 'property-start directive not'
                                     ' allowed inside another property')
                p = Property(LineRange(d.line_no, None), d.name, d.dsl_sloc,
                             d.is_dispatcher)
                self.properties.append(p)
                self.properties_dict[p.name] = p
                scope_stack.append(p)

            elif isinstance(d, (ScopeStart, PropertyCallStart,
                                MemoizationLookupDirective)):
                if not scope_stack or not isinstance(scope_stack[-1], Scope):
                    raise ParseError(
                        line_no,
                        '{} directive must occur inside a property or a'
                        ' property scope'.format(d.directive_name)
                    )

                line_range = LineRange(d.line_no, None)

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
                    raise ParseError(line_no, 'no scope to end')
                ended_scope = scope_stack.pop()
                ended_scope.line_range.last_line = d.line_no
                if scope_stack:
                    assert isinstance(scope_stack[-1], Scope)
                    scope_stack[-1].events.append(ended_scope)
                else:
                    assert isinstance(ended_scope, Property), (
                        'Top-level scopes must all be properties'
                    )
                    assert not expr_stack, (
                        'Some expressions are not done when leaving property'
                        ' {}: {}'.format(
                            ended_scope.name,
                            ', '.join(str(e) for e in expr_stack)
                        )
                    )

            elif isinstance(d, BindDirective):
                if not scope_stack:
                    raise ParseError(line_no, 'no scope for binding')
                assert isinstance(scope_stack[-1], Scope)
                scope_stack[-1].events.append(Bind(d.line_no, d.dsl_name,
                                                   d.gen_name))

            elif isinstance(d, ExprStartDirective):
                if not scope_stack:
                    raise ParseError(line_no, 'no scope for expression')
                start_event = ExprStart(d.line_no, d.expr_id, d.expr_repr,
                                        d.result_var, d.dsl_sloc)
                assert isinstance(scope_stack[-1], Scope)
                scope_stack[-1].events.append(start_event)
                if expr_stack:
                    expr_stack[-1].sub_expr_start.append(start_event)
                expr_stack.append(start_event)

            elif isinstance(d, ExprDoneDirective):
                if not scope_stack:
                    raise ParseError(line_no, 'no scope for expression')
                done_event = ExprDone(d.line_no, d.expr_id)
                start_event = expr_stack.pop()
                assert start_event.expr_id == done_event.expr_id, (
                    'Mismatching ExprStart/ExprStop events: {} and {}'.format(
                        start_event, done_event
                    )
                )
                start_event._done_event = done_event
                assert isinstance(scope_stack[-1], Scope)
                scope_stack[-1].events.append(done_event)

            elif isinstance(d, MemoizationReturnDirective):
                if (not scope_stack or
                        not isinstance(scope_stack[-1], MemoizationLookup)):
                    raise ParseError(
                        line_no,
                        'memoization-result directive must appear inside a'
                        ' memoization-lookup scope'
                    )
                scope_stack[-1].events.append(MemoizationReturn(d.line_no))

            elif isinstance(d, PropertyBodyStart):
                if not scope_stack or not isinstance(scope_stack[0], Property):
                    raise ParseError(
                        line_no,
                        'property-body-start directive must appear inside a'
                        ' property'
                    )
                scope_stack[0].body_start = d.line_no

            else:
                raise NotImplementedError('Unknown directive: {}'.format(d))

        if scope_stack:
            raise ParseError(line_no, 'end of scope expected before end of'
                                      ' file')

    def lookup_property(self, line_no: int) -> Property | None:
        """
        Look for a property that covers the given source line number. Return
        None if there is no such property.

        :param line_no: Line number to lookup.
        """
        for p in self.properties:
            if line_no < p.line_range.first_line:
                break
            elif line_no in p.line_range:
                return p
        return None

    def get_property_by_name(self, name: str) -> Property:
        """
        Fetch the property called `name`. Raise an error if not found.

        :param str name: Name of the property to fetch.
        :rtype: Property
        """
        return self.properties_dict[name]


class DSLLocation:
    """
    Source location in the DSL.
    """

    def __init__(self, filename: str, line_no: int):
        self.filename = filename
        self.line_no = line_no

    @classmethod
    def parse(cls, dsl_sloc: str) -> DSLLocation | None:
        """
        If `dsl_sloc` is "None", return None. Otherwise, create a DSLLocation
        instance out of a string of the form "filename:line_no".
        """
        if dsl_sloc == 'None':
            return None

        try:
            filename, line_no = dsl_sloc.rsplit(':', 1)
        except ValueError:
            raise ValueError('Invalid DSL location: {}'.format(dsl_sloc))

        try:
            line_no_int = int(line_no)
        except ValueError:
            raise ValueError('Invalid line number: {}'.format(line_no))

        return cls(filename, line_no_int)

    def matches(self, other: DSLLocation) -> bool:
        """
        Return whether `self` designates a file at least as much specifically
        as `other`. In practice, both must have the same line number and the
        filename from `other` must be a suffix for the one in `self`.
        """
        return (self.line_no == other.line_no
                and self.filename.endswith(other.filename))

    def __str__(self) -> str:
        return '{}:{}'.format(self.filename, self.line_no)

    def __repr__(self) -> str:
        return '<DSLLocation {}>'.format(self)


class LineRange:
    """
    Range of lines in the $-implementation.adb source file.
    """

    def __init__(self, first_line: int, last_line: int | None):
        self.first_line = first_line
        self.last_line = last_line

    def __contains__(self, line_no: int) -> bool:
        assert isinstance(line_no, int)
        assert isinstance(self.last_line, int)
        return self.first_line <= line_no <= self.last_line

    def __repr__(self) -> str:
        return '<LineRange {}>'.format(self)

    def __str__(self) -> str:
        return '{}-{}'.format(self.first_line, self.last_line)


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
        return '<{}{} {}>'.format(
            type(self).__name__,
            ' {}'.format(self.label) if self.label else '',
            self.line_range
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
    def __init__(self,
                 line_range: LineRange,
                 name: str,
                 dsl_sloc: DSLLocation | None,
                 is_dispatcher: bool):
        super().__init__(line_range, name)
        self.name = name
        self.dsl_sloc = dsl_sloc
        self.is_dispatcher = is_dispatcher

        self.body_start: int | None = None
        """
        Line number where to put breakpoints for the beginning of this
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
    def __init__(self, line_no: int, entity: str | None = None):
        self.line_no = line_no
        self.entity = entity

    @abc.abstractmethod
    def apply_on_state(self, scope_state: ScopeState) -> None:
        """
        Modify the input state according to the effect this event has. All
        subclasses must override this.
        """
        ...

    def __repr__(self) -> str:
        return '<Event line {}>'.format(self.line_no)


class Bind(Event):
    def __init__(self, line_no: int, dsl_name: str, gen_name: str):
        super().__init__(line_no, dsl_name)
        self.dsl_name = dsl_name
        self.gen_name = gen_name

    def apply_on_state(self, scope_state: ScopeState) -> None:
        scope_state.bindings.append(
            Binding(self.dsl_name, self.gen_name)
        )

    def __repr__(self) -> str:
        return '<Bind {}, line {}>'.format(self.dsl_name, self.line_no)


class ExprStart(Event):
    def __init__(self,
                 line_no: int,
                 expr_id: str,
                 expr_repr: str,
                 result_var: str,
                 dsl_sloc: DSLLocation | None):
        super().__init__(line_no)
        self.expr_id = expr_id
        self.expr_repr = expr_repr
        self.result_var = result_var
        self.dsl_sloc = dsl_sloc

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

        :rtype: ExprDone
        """
        assert self._done_event
        return self._done_event

    @property
    def line_range(self) -> LineRange:
        """
        Return the line range that spans from this ExprStart event to the
        corresponding ExprDone one.

        :rtype: LineRange
        """
        return LineRange(self.line_no, self.done_event.line_no)

    def apply_on_state(self, scope_state: ScopeState) -> None:
        assert self.expr_id not in scope_state.expressions
        expr = ExpressionEvaluation(self)
        scope_state.expressions[self.expr_id] = expr
        if scope_state.state.started_expressions:
            scope_state.state.started_expressions[-1].append_sub_expr(expr)
        scope_state.state.started_expressions.append(expr)

    def __repr__(self) -> str:
        return '<ExprStart {}, line {}>'.format(self.expr_id, self.line_no)


class ExprDone(Event):
    def __init__(self, line_no: int, expr_id: str):
        super().__init__(line_no)
        self.expr_id = expr_id

    def apply_on_state(self, scope_state: ScopeState) -> None:
        expr = scope_state.expressions[self.expr_id]

        pop = scope_state.state.started_expressions.pop()
        assert pop == expr, (
            'Expressions are not properly nested: {} is done before {}'
            ' is'.format(expr, pop)
        )
        expr.set_done(self.line_no)

    def __repr__(self) -> str:
        return '<ExprDone {}, line {}>'.format(self.expr_id, self.line_no)


class MemoizationLookup(Scope):
    pass


class MemoizationReturn(Event):
    def apply_on_state(self, scope_state: ScopeState) -> None:
        pass

    def __repr__(self) -> str:
        return '<MemoizationReturn, line {}>'.format(self.line_no)


class Directive:
    """
    Holder for GDB helper directives as parsed from source files.
    """

    name_to_cls: dict[str, Type[Directive]] = {}

    def __init__(self, line_no: int):
        self.line_no = line_no

    @property
    def directive_name(self) -> str:
        """
        Return the name of this directive.
        """
        for name, directive_type in self.name_to_cls.items():
            if isinstance(self, directive_type):
                return name
        raise KeyError('Unknown directive: {}'.format(self))

    @classmethod
    def parse_dispatch(cls,
                       line_no: int,
                       name: str,
                       args: list[str]) -> Directive:
        """
        Try to parse a GDB helper directive. Raise a ParseError if anything
        goes wrong.

        :param line_no: Line number on which this directive appears in the
            source file.
        :param name: Name of the directive.
        :param args: Arguments for this directive.
        """
        try:
            subcls = cls.name_to_cls[name]
        except KeyError:
            raise ParseError(line_no, 'invalid directive: {}'.format(name))
        return subcls.parse(line_no, args)

    @classmethod
    @abc.abstractmethod
    def parse(cls, line_no: int, agrs: list[str]) -> Directive:
        """
        Subclasses must override this to parse the directive.
        """
        ...


class PropertyStart(Directive):
    def __init__(self,
                 name: str,
                 dsl_sloc: DSLLocation | None,
                 is_dispatcher: bool,
                 line_no: int):
        super().__init__(line_no)
        self.name = name
        self.dsl_sloc = dsl_sloc
        self.is_dispatcher = is_dispatcher

    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> PropertyStart:
        name, info = args
        if info == 'dispatcher':
            is_dispatcher = True
            dsl_sloc = None
        else:
            is_dispatcher = False
            dsl_sloc = DSLLocation.parse(info)
        return cls(name, dsl_sloc, is_dispatcher, line_no)


class PropertyBodyStart(Directive):
    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> PropertyBodyStart:
        assert len(args) == 0
        return cls(line_no)


class PropertyCallStart(Directive):
    def __init__(self, name: str, line_no: int):
        super().__init__(line_no)
        self.name = name

    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> PropertyCallStart:
        name, = args
        return cls(name, line_no)


class MemoizationLookupDirective(Directive):
    @classmethod
    def parse(cls,
              line_no: int,
              args: list[str]) -> MemoizationLookupDirective:
        assert len(args) == 0
        return cls(line_no)


class MemoizationReturnDirective(Directive):
    @classmethod
    def parse(cls,
              line_no: int,
              args: list[str]) -> MemoizationReturnDirective:
        assert len(args) == 0
        return cls(line_no)


class ScopeStart(Directive):
    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> ScopeStart:
        return cls(line_no)


class BindDirective(Directive):
    def __init__(self, dsl_name: str, gen_name: str, line_no: int):
        super().__init__(line_no)
        self.dsl_name = dsl_name
        self.gen_name = gen_name

    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> BindDirective:
        dsl_name, gen_name = args
        return cls(dsl_name, gen_name, line_no)


class End(Directive):
    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> End:
        return cls(line_no)


class ExprStartDirective(Directive):
    def __init__(self,
                 expr_id: str,
                 expr_repr: str,
                 result_var: str,
                 dsl_sloc: DSLLocation | None,
                 line_no: int):
        super().__init__(line_no)
        self.expr_id = expr_id
        self.expr_repr = expr_repr
        self.result_var = result_var
        self.dsl_sloc = dsl_sloc

    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> ExprStartDirective:
        expr_id, expr_repr, result_var, dsl_sloc = args
        return cls(
            expr_id,
            f"<{expr_repr} at {dsl_sloc}>",
            result_var,
            DSLLocation.parse(dsl_sloc),
            line_no,
        )


class ExprDoneDirective(Directive):
    def __init__(self, expr_id: str, line_no: int):
        super().__init__(line_no)
        self.expr_id = expr_id

    @classmethod
    def parse(cls, line_no: int, args: list[str]) -> ExprDoneDirective:
        expr_id, = args
        return cls(expr_id, line_no)


Directive.name_to_cls.update({
    'property-start': PropertyStart,
    'property-body-start': PropertyBodyStart,
    'property-call-start': PropertyCallStart,
    'memoization-lookup': MemoizationLookupDirective,
    'memoization-return': MemoizationReturnDirective,
    'scope-start': ScopeStart,
    'bind': BindDirective,
    'end': End,
    'expr-start': ExprStartDirective,
    'expr-done': ExprDoneDirective,
})
