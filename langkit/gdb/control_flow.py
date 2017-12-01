from __future__ import absolute_import, division, print_function

import gdb

from langkit.gdb.breakpoints import BreakpointGroup
from langkit.gdb.debug_info import ExprDone, ExprStart, PropertyCall, Scope
from langkit.gdb.utils import expr_repr


def scope_start_location(context, scope, from_line_no=None):
    """
    Return the line number in the generated source where to put a breakpoint
    for the beginning of a scope.

    :type context: langkit.gdb.context.Context
    :type scope: Scope

    :param int|None from_line_no: If given, don't consider line numbers lower
        than or equal to `from_line_no`.

    :rtype: int|None
    """
    def accepted(line_no):
        return from_line_no is None or from_line_no < line_no

    # First look for the root expression in this scope
    events = scope.iter_events(filter=ExprStart)
    try:
        line_no = next(iter(events)).line_no
    except StopIteration:
        pass
    else:
        if accepted(line_no):
            return line_no

    # Otherwise just return the first line of this scope
    line_no = scope.line_range.first_line
    if accepted(line_no):
        return line_no


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
                                       from_line_no=state.line_no)

        if line_no:
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


def go_step_inside(context):
    """
    If execution is about to call a property, step inside it. Traverse
    dispatch properties in order to land directly in the dispatched property.
    """

    def continue_until(line_no, hide_output):
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

    # If we are not inside a  property call already, look for all property
    # calls in the current expression.
    if not target and scope_state:
        # Look for property calls that fall under the following line range...
        expr_range = current_expr.start_event.line_range

        # ... and that *don't* fall under these (i.e. exclude calls for nested
        # expressions).
        filter_ranges = [expr.line_range
                         for expr in current_expr.start_event.sub_expr_start]

        def filter(e):
            if not isinstance(e, PropertyCall):
                return False
            line_no = event.line_range.first_line
            if line_no not in expr_range:
                return False
            for fr in filter_ranges:
                if line_no in fr:
                    return False
            return True

        targets = [event.property(context)
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
        line_no = scope_start_location(context, target)
        if line_no:
            continue_until(line_no, False)
        else:
            go_next(context)
        return

    # The target is a dispatcher. These have only one first-level scope, so:
    # continue to its first-level scope.
    outer_scopes = list(target.iter_events(recursive=False, filter=Scope))
    if len(outer_scopes) != 1:
        print('ERROR: dispatcher {} has none or multiple first-level'
              ' scopes'.format(target))
        return
    outer_scope = outer_scopes[0]
    continue_until(outer_scope.line_range.first_line, True)

    # Step until we reach a nested scope, so that we let the dispatch occur
    while True:
        state = context.decode_state()
        if not state or state.property != target:
            print('ERROR: landed somewhere else that in {}'.format(target))
            return
        inner_scope = state.innermost_scope.scope

        if inner_scope != outer_scope:
            break
        gdb.execute('next')

    # We now reached the matcher that contains the call to the dispatched
    # property: find it and follow the call.
    targets = [call.property(context)
               for call in inner_scope.iter_events(filter=PropertyCall)
               if call.line_range.first_line in inner_scope.line_range]
    target = targets[0] if len(targets) == 1 else None
    continue_until(scope_start_location(context, target), False)
