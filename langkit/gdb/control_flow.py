from __future__ import absolute_import, division, print_function

import gdb

from langkit.gdb.breakpoints import BreakpointGroup
from langkit.gdb.debug_info import ExprDone, ExprStart
from langkit.gdb.utils import expr_repr


def scope_start_location(context, scope, allow_degraded_mode=True):
    """
    Return the line number in the generated source where to put a breakpoint
    for the beginning of a scope.

    :type context: langkit.gdb.context.Context
    :type scope: Scope

    :param bool allow_degraded_mode: By default, use the sloc of the root
        expression. If there is not and this is true, fallback on the first
        line of `scope`, otherwise return None.

        This last case is degraded mode, as the first line of the root
        expression takes us closer to the interesting bits in source code.

    :rtype: int|None
    """
    # First look for the root expression in this scope
    events = scope.iter_events(filter=ExprStart)
    try:
        return next(iter(events)).line_no
    except StopIteration:
        pass

    # Otherwise just return the first line of this scope
    return scope.line_range.first_line if allow_degraded_mode else None


def go_next(context):
    """
    Continue execution until reaching another expression.
    """

    state = context.decode_state()
    if not state:
        print('Selected frame is not in a property.')
        return

    scope_state, current_expr = state.lookup_current_expr()

    if current_expr is None:
        # There are only two possible causes for no currently evaluating
        # expressions: either the property just started (root expression
        # evaluation is ahead), either it is about to return (root expr.  eval.
        # is behind).
        line_no = scope_start_location(context, state.property_scope.scope,
                                       allow_degraded_mode=False)

        if line_no and state.line_no < line_no:
            # The first expression is ahead: resume execution until we reach
            # it.
            gdb.execute('until {}'.format(line_no))
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
        print('{} evaluated to {}'.format(
            expr_repr(new_expr),
            new_expr.read(new_state.frame)
        ))

    # Display the expression of most interest, if any
    if new_current_expr:
        print('')
        print('Now evaluating {}'.format(expr_repr(new_current_expr)))


def go_out(context):
    """
    Continue execution until the end of the evaluation of the current
    sub-expression.
    """

    # Look for the expression that is being evaluated currently
    state = context.decode_state()
    if not state:
        print('Selected frame is not in a property.')

    scope_state, current_expr = state.lookup_current_expr()
    if not current_expr:
        print('Not evaluating any expression currently')
        return

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
    new_expr = new_state.lookup_expr(current_expr.expr_id)
    if new_state:
        _, new_current_expr = new_state.lookup_current_expr()
    else:
        new_current_expr = None

    # Do some sanity checks first...

    def error(msg):
        print('ERROR: {}: something went wrong...'.format(msg))

    if new_state.property != state.property:
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
