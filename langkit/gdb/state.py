from __future__ import annotations

from collections import OrderedDict
from enum import Enum
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    import gdb

    from langkit.debug_info import (
        AdaLocation,
        ExprDone,
        ExprStart,
        LktLocation,
        Property,
        Scope,
    )
    from langkit.gdb.context import Context


def gen_code_loc(context: Context, frame: gdb.Frame) -> AdaLocation | None:
    """
    If the given frame is in generated Ada code covered by GDB directives,
    rturn currently executed location. Return None otherwise.
    """
    from langkit.debug_info import AdaLocation

    current_func = frame.function()
    if current_func is None:
        return None

    current_symtab = current_func.symtab
    if current_symtab is None:
        return None

    current_file = current_symtab.fullname()
    if current_file not in context.debug_info.filenames:
        return None

    return AdaLocation(current_file, frame.find_sal().line)


class State:
    """
    Holder for the execution state of a property.
    """

    def __init__(self, frame: gdb.Frame, loc: AdaLocation, prop: Property):
        self.frame = frame
        """
        The GDB frame from which this state was decoded.
        """

        self.property = prop
        """
        The property currently running.
        """

        self.scopes: list[ScopeState] = []
        """
        The stack of scope states describing the current execution state. The
        first item is the scope for the property itself. The following items
        are the nested scopes currently activated. The last item is the most
        nested scope.
        """

        self.started_expressions: list[ExpressionEvaluation] = []
        """
        Stack of expressions that are being evaluated.
        """

        self.loc = loc
        """
        The location in the generated source code where execution was when this
        state was decoded.
        """

    @property
    def in_memoization_lookup(self) -> bool:
        """
        Return whether execution is inside a memoization handler, about to
        return a cached result.
        """
        from langkit.debug_info import MemoizationLookup

        innermost = self.innermost_scope
        return innermost is not None and isinstance(
            innermost.scope, MemoizationLookup
        )

    @property
    def property_scope(self) -> ScopeState:
        """
        Return the ScopeState associated to the running property.
        """
        return self.scopes[0]

    @property
    def innermost_scope(self) -> ScopeState:
        """
        Return the ScopeState associated to the innermost activated scope.
        """
        return self.scopes[-1]

    def lookup_current_expr(
        self,
    ) -> tuple[ScopeState | None, ExpressionEvaluation | None]:
        """
        Return the innermost currently evaluating expression and its scope
        state. Return (None, None) if there is no evaluating expression.
        """
        for scope_state in reversed(self.scopes):
            for e in reversed(scope_state.expressions.values()):
                if e.is_started:
                    return scope_state, e
        return (None, None)

    def lookup_expr(self, expr_id: str) -> ExpressionEvaluation | None:
        """
        Look for an expression evaluation matching the given ID.
        """
        for scope in self.scopes:
            try:
                return scope.expressions[expr_id]
            except KeyError:
                pass
        return None

    @classmethod
    def decode(cls, context: Context, frame: gdb.Frame) -> State | None:
        """
        Decode the execution state from the given GDB frame. Return None if no
        property is running in this frame.
        """
        from langkit.debug_info import Event, PropertyCall, Scope

        loc = gen_code_loc(context, frame)

        # First, look for the currently running property
        prop = context.debug_info.lookup_property(loc) if loc else None
        if prop is None:
            return None
        assert loc is not None

        # Create the result, add the property root scope
        result = cls(frame, loc, prop)
        root_scope_state = ScopeState(result, None, prop)
        result.scopes.append(root_scope_state)

        def build_scope_state(scope_state: ScopeState) -> None:
            assert loc is not None
            for event in scope_state.scope.events:

                if isinstance(event, Event):
                    if event.loc.is_after(loc):
                        break
                    event.apply_on_state(scope_state)

                elif isinstance(event, Scope):
                    if event.line_range.first_loc.is_after(loc):
                        break
                    elif loc in event.line_range:
                        sub_scope_state = ScopeState(
                            result, scope_state, event
                        )
                        result.scopes.append(sub_scope_state)
                        build_scope_state(sub_scope_state)
                        break

                elif isinstance(event, PropertyCall):
                    if loc in event.line_range:
                        scope_state.called_property = event.property(context)

        build_scope_state(root_scope_state)
        return result


class ScopeState:
    """
    Holder for the execution state of a specific scope in a property.
    """

    def __init__(self, state: State, parent: ScopeState | None, scope: Scope):
        self.state = state
        self.parent = parent

        self.scope = scope
        """
        The scope of interest.
        """

        self.bindings: list[Binding] = []
        """
        Bindings that are live in this state.
        """

        self.expressions: dict[str, ExpressionEvaluation] = OrderedDict()
        """
        Expressions that are currently being evaluated or that are evaluated in
        this state, indexed by unique ids.
        """

        self.called_property: Property | None = None
        """
        Property that is currently being called, if any.
        """

    def sorted_expressions(
        self,
    ) -> tuple[list[ExpressionEvaluation], ExpressionEvaluation | None]:
        """
        Return a tuple, whose first element is the list of already evaluated
        expressions in this scope, sorted by line of done, and second element
        is the currently evaluating expression.
        """

        done_exprs = []
        last_started = None
        for e in self.expressions.values():
            if e.is_started:
                last_started = e
            elif e.is_done:
                done_exprs.append(e)

        # Sort expressions whose evaluation is completed by "done location"
        # so that users see them in the order they saw evaluation
        # happening.
        def key(e: ExpressionEvaluation) -> int:
            assert e.done_at_line is not None
            return e.done_at_line

        done_exprs.sort(key=key)

        return done_exprs, last_started


class Binding:
    """
    Describe the mapping between a DSL-level variable and an Ada one in the
    generated code.
    """

    def __init__(self, lkt_name: str, gen_name: str):
        self.lkt_name = lkt_name
        """
        Name of the variable in the DSL.
        """

        self.gen_name = gen_name
        """
        Name of the variable in the Ada generated code.
        """


class EvalState(Enum):
    """
    Evaluation state for an expression.
    """

    started = "started"
    """
    Evaluation has started but not yet completed.
    """

    done = "done"
    """
    Evaluation has completed. The expression result is available for use in
    the result variable, if there is one.
    """


class ExpressionEvaluation:
    """
    Describe the state of evaluation of an expression.
    """

    def __init__(self, start_event: ExprStart):
        self.start_event = start_event

        self.parent_expr: ExpressionEvaluation | None = None
        self.sub_exprs: list[ExpressionEvaluation] = []

        self.state: EvalState = EvalState.started
        self.done_at_line: int | None = None

    @property
    def expr_id(self) -> str:
        return self.start_event.expr_id

    @property
    def expr_repr(self) -> str:
        return self.start_event.expr_repr

    @property
    def result_var(self) -> str:
        return self.start_event.result_var

    @property
    def lkt_sloc(self) -> LktLocation | None:
        return self.start_event.lkt_sloc

    @property
    def done_event(self) -> ExprDone:
        return self.start_event.done_event

    def set_done(self, loc: AdaLocation) -> None:
        assert loc.filename == self.start_event.loc.filename
        self.state = EvalState.done
        self.done_at_line = loc.line_no

    @property
    def is_started(self) -> bool:
        return self.state == EvalState.started

    @property
    def is_done(self) -> bool:
        return self.state == EvalState.done

    def append_sub_expr(self, expr: ExpressionEvaluation) -> None:
        """
        Append `expr` to the list of sub-expressions for `self`. Also set
        `self` as the parent of `expr`.
        """
        assert expr.parent_expr is None
        self.sub_exprs.append(expr)
        expr.parent_expr = self

    def read(self, frame: gdb.Frame) -> gdb.Value:
        """
        Read the value of this expression in the given GDB frame.

        This is valid iff this expression is done.
        """
        assert self.is_done
        return frame.read_var(self.result_var.lower())

    def __repr__(self) -> str:
        return "<ExpressionEvaluation {}, {}>".format(
            self.expr_id, self.lkt_sloc
        )
