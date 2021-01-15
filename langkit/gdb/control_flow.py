from __future__ import annotations

from typing import Iterable, List, Optional, cast

import gdb

from langkit.gdb.breakpoints import BreakpointGroup
from langkit.gdb.context import Context
from langkit.gdb.debug_info import (
    BaseEvent, ExprDone, ExprStart, MemoizationReturn, Property, PropertyCall,
    Scope
)
from langkit.gdb.utils import expr_repr


def scope_start_line_nos(scope: Scope,
                         from_line_no: Optional[int] = None) -> List[int]:
    """
    Return line numbers for all entry points that are relevant (for users) for
    the given `scope`. Return an empty list if we could find no relevant
    location to break on.

    :param from_line_no: If given, don't consider line numbers lower than or
        equal to `from_line_no`.
    """
    candidates: List[int] = []

    # Consider the first line for this scope's root expression, if any
    events = cast(Iterable[ExprStart], scope.iter_events(filter=ExprStart))
    try:
        line_no = next(iter(events)).line_no
    except StopIteration:
        # If there is no root expression, just use the first scope line. It's
        # degraded mode because users are interested in expressions rather than
        # scopes.
        candidates.append(scope.line_range.first_line)
    else:
        candidates.append(line_no)

    # Consider memoization return points for properties
    if isinstance(scope, Property):
        lookup_scope = scope.memoization_lookup
        if lookup_scope:
            ls_events = cast(List[MemoizationReturn], lookup_scope)
            candidates.extend(m.line_no for m in ls_events)

    # Filter candidates if needed with `from_line_no`
    if from_line_no:
        candidates = [l for l in candidates if from_line_no < l]

    return candidates


def break_scope_start(
    context: Context,
    scope: Scope,
    from_line_no: Optional[int] = None,
) -> Optional[BreakpointGroup]:
    """
    Create a breakpoint group for all entry points that are relevant (for
    users) for the given `scope`. Return None if we could find no relevant
    location to break on, otherwise return the breakpoint group.

    :param from_line_no: If given, don't consider line numbers lower than or
        equal to `from_line_no`.
    """
    candidates = scope_start_line_nos(scope, from_line_no)
    return BreakpointGroup(context, candidates) if candidates else None


def go_next(context: Context) -> None:
    """
    Continue execution until reaching another expression.
    """

    state = context.decode_state()
    if not state:
        print('Selected frame is not in a property.')
        return

    # If execution reached the part of the code where the property is about to
    # return a cached result, just let it return.
    if state.in_memoization_lookup:
        gdb.execute('finish')
        return

    scope_state, current_expr = state.lookup_current_expr()

    if current_expr is None:
        # There are only two possible causes for no currently evaluating
        # expressions: either the property just started (root expression
        # evaluation is ahead), either it is about to return (root expr.  eval.
        # is behind).
        bp_group = break_scope_start(context, state.property_scope.scope,
                                     from_line_no=state.line_no)

        if bp_group:
            # The first expression is ahead: resume execution until we reach
            # it.
            gdb.execute('continue')
        else:
            gdb.execute('finish')

    else:
        # Depending on the control flow behavior of the currently running
        # expression, the next step can be either its parent expression or any
        # of its sub-expressions.
        next_slocs_candidates = []

        # First look for the point where the current expression terminates its
        # evaluation.
        next_slocs_candidates.append(current_expr.done_event.line_no)

        # Now look for the starting point for all sub-expressions
        for subexpr in current_expr.start_event.sub_expr_start:
            next_slocs_candidates.append(subexpr.line_no)

        BreakpointGroup(context, next_slocs_candidates)
        gdb.execute('continue')

    new_current_expr = None
    new_expr = None

    new_state = context.decode_state()
    if new_state:
        _, new_current_expr = new_state.lookup_current_expr()
        if current_expr:
            new_expr = new_state.lookup_expr(current_expr.expr_id)

    # If we just finished the evaluation of an expression, display its value
    if new_expr and new_expr.is_done:
        assert new_state is not None
        print('{} evaluated to {}'.format(
            expr_repr(new_expr),
            new_expr.read(new_state.frame)
        ))

    # Display the expression of most interest, if any
    if new_current_expr:
        print('')
        print('Now evaluating {}'.format(expr_repr(new_current_expr)))


def go_out(context: Context) -> None:
    """
    Continue execution until the end of the evaluation of the current
    sub-expression.
    """

    # Look for the expression that is being evaluated currently
    state = context.decode_state()
    if state is None:
        print('Selected frame is not in a property.')
        return

    scope_state, current_expr = state.lookup_current_expr()
    if not current_expr:
        print('Not evaluating any expression currently')
        return
    assert scope_state is not None

    # Look for the point in the generated library where its evaluation will be
    # done.
    until_line_no = None
    for e in scope_state.scope.events:
        if isinstance(e, ExprDone) and e.expr_id == current_expr.expr_id:
            until_line_no = e.line_no
    if until_line_no is None:
        print('ERROR: cannot find the end of evaluation for expression {}.'
              ' Code generation may have a bug.'.format(current_expr))
        return

    # Now go there! When we land in the expected place, also be useful and
    # display the value we got.
    gdb.execute('until {}'.format(until_line_no))
    frame = gdb.selected_frame()
    new_state = context.decode_state(frame)
    assert new_state is not None
    new_expr = new_state.lookup_expr(current_expr.expr_id)
    if new_state:
        _, new_current_expr = new_state.lookup_current_expr()
    else:
        new_current_expr = None

    # Do some sanity checks first...

    def error(msg: str) -> None:
        print('ERROR: {}: something went wrong...'.format(msg))

    if new_state is None or new_state.property != state.property:
        return error('we landed in another property')
    if new_expr is None:
        return error('cannot find back the same expression')
    if not new_expr.is_done:
        return error('the expression is not evaluated yet')

    print('')
    print('{} evaluated to: {}'.format(expr_repr(current_expr),
                                       new_expr.read(new_state.frame)))
    if new_current_expr:
        print('')
        print('Now evaluating {}'.format(expr_repr(new_current_expr)))


def go_step_inside(context: Context) -> None:
    """
    If execution is about to call a property, step inside it. Traverse
    dispatch properties in order to land directly in the dispatched property.
    """

    def continue_until(line_no: int, hide_output: bool) -> None:
        dest_spec = '{}:{}'.format(context.debug_info.filename,
                                   line_no)
        gdb.Breakpoint(dest_spec, internal=True, temporary=True)
        gdb.execute('continue', to_string=hide_output)

    # First, look for a property call in the current execution state
    state = context.decode_state()
    if not state:
        print('Selected frame is not in a property.')
        return
    scope_state, current_expr = state.lookup_current_expr()
    target = scope_state.called_property if scope_state else None

    # If we are not inside a property call already, look for all property
    # calls in the current expression.
    if target is None and scope_state is not None:
        assert current_expr is not None

        # Look for property calls that fall under the following line range...
        expr_range = current_expr.start_event.line_range

        # ... and that *don't* fall under these (i.e. exclude calls for nested
        # expressions).
        filter_ranges = [expr.line_range
                         for expr in current_expr.start_event.sub_expr_start]

        def filter(e: BaseEvent) -> bool:
            if not isinstance(e, PropertyCall):
                return False
            line_no = e.line_range.first_line
            if line_no not in expr_range:
                return False
            for fr in filter_ranges:
                if line_no in fr:
                    return False
            return True

        targets = [cast(PropertyCall, event).property(context)
                   for event in scope_state.scope.iter_events()
                   if filter(event)]
        if len(targets) == 1:
            target = targets[0]

    # Still no call target in sight? Just behave like the "next" commmand
    if not target:
        go_next(context)
        return

    # If the target is not a dispatcher, put a temporary breakpoint on the
    # first line in its body and continue to reach it.
    if not target.is_dispatcher:
        bp_group = break_scope_start(context, target)
        if bp_group:
            gdb.execute('continue')
        else:
            go_next(context)
        return

    def frame_signature(frame: gdb.Frame) -> str:
        return str(frame.function())

    # The target is a dispatcher. Look for all property calls it contains,
    # create a breakpoint group for them and continue.
    line_nos = []
    for call in target.iter_events(filter=PropertyCall):
        assert isinstance(call, PropertyCall)
        try:
            prop = call.property(context)
        except KeyError:
            # This happens when the called property is actually a stub
            # (abstract runtime check). No need to put a breakpoint, there.
            pass
        else:
            line_nos.extend(scope_start_line_nos(prop))
    BreakpointGroup(context, line_nos)
    gdb.execute('continue')
