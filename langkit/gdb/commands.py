from __future__ import absolute_import, division, print_function

from collections import namedtuple
from functools import partial
import itertools
from StringIO import StringIO
import sys

import gdb

from langkit.gdb.breakpoints import BreakpointGroup
from langkit.gdb.debug_info import DSLLocation, ExprDone, ExprStart, Scope
from langkit.gdb.utils import expr_repr, name_repr, prop_repr
from langkit.utils import no_colors


def get_input(prompt):
    """
    Wrapper around raw_input/input depending on the Python version.
    """
    input_fn = input if sys.version_info.major > 2 else raw_input
    return input_fn(prompt)


class BaseCommand(gdb.Command):
    """
    Factorize common code for our commands.
    """

    def __init__(self, context, basename, command_class,
                 completer_class=gdb.COMPLETE_NONE):
        kwargs = {'name': '{}{}'.format(context.prefix, basename),
                  'command_class': command_class}
        if completer_class is not None:
            kwargs['completer_class'] = completer_class
        super(BaseCommand, self).__init__(**kwargs)
        self.context = context


class StateCommand(BaseCommand):
    """Display the state of the currently running property.

This command may be followed by a "/X" flag, where X is one or several of:

    * f: display the full image of values (no ellipsis);
    * s: to print the name of the Ada variables that hold DSL values.
"""

    def __init__(self, context):
        super(StateCommand, self).__init__(context, 'state', gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        printer = StatePrinter(self.context)

        if arg:
            if not arg.startswith('/'):
                print('Invalid argument')
                return

            for c in arg[1:]:
                if c == 'f':
                    printer.with_ellipsis = False
                elif c == 's':
                    printer.with_locs = True
                else:
                    print('Invalid flag: {}'.format(repr(c)))
                    return

        printer.run()


class StatePrinter(object):
    """
    Helper class to embed code to display the state of the currently running
    property.
    """

    ellipsis_limit = 50

    def __init__(self, context):
        self.context = context

        self.frame = gdb.selected_frame()
        self.state = self.context.decode_state(self.frame)

        self.with_ellipsis = True
        self.with_locs = False
        self.sio = StringIO()

    def _render(self):
        """
        Internal render method for the state printer.
        """

        # We rebind print to print to our StringIO instance for the scope of
        # this method.
        prn = partial(print, file=self.sio)

        if self.state is None:
            prn('Selected frame is not in a property.')
            return

        prn('Running {}'.format(prop_repr(self.state.property)))
        prn('from {}'.format(self.state.property.dsl_sloc))

        for scope_state in self.state.scopes:
            is_first = [True]

            def print_info(strn):
                if is_first[0]:
                    prn('')
                    is_first[0] = False
                prn(strn)

            for b in scope_state.bindings:
                print_info('{}{} = {}'.format(
                    name_repr(b),
                    self.loc_image(b.gen_name),
                    self.value_image(b.gen_name)
                ))

            done_exprs, last_started = scope_state.sorted_expressions()

            for e in done_exprs:
                print_info('{}{} -> {}'.format(
                    expr_repr(e),
                    self.loc_image(e.result_var),
                    self.value_image(e.result_var)
                ))

            if last_started:
                print_info('Currently evaluating {}'.format(
                    expr_repr(last_started)
                ))
                if last_started.dsl_sloc:
                    print_info('from {}'.format(last_started.dsl_sloc))

    def run(self):
        """
        Output the state to stdout.
        """
        self._render()
        print(self.sio.getvalue())

    def render(self):
        """
        Return the state as a string.

        :rtype: str
        """
        with no_colors():
            self._render()
        return self.sio.getvalue()

    def loc_image(self, var_name):
        """
        If `self.with_locs`, return the name of the Ada variable that holds the
        DSL value.

        :rtype: str
        """
        return ' ({})'.format(var_name) if self.with_locs else ''

    def value_image(self, var_name):
        """
        Return the image of the value contained in the `var_name` variable.

        :rtype: str
        """
        # Switching to lower-case is required since GDB ignores case
        # insentivity for Ada from the Python API.
        value = str(self.frame.read_var(var_name.lower()))
        if self.with_ellipsis and len(value) > self.ellipsis_limit:
            value = '{}...'.format(value[:self.ellipsis_limit])
        return value


class BreakCommand(BaseCommand):
    """Put a breakpoint on a property. One of the following forms is allowed:

    * A case-insensitive property qualified name; for instance::
          break MyNode.p_property

    * A DSL source location; for instance, in spec.py, line 128::
          break spec.py:128

In both cases, one can pass an expression to make the breakpoint conditional.
For instance::

    break MyNode.p_property if $match("<Node XXX>", self)
"""

    def __init__(self, context):
        super(BreakCommand, self).__init__(context, 'break',
                                           gdb.COMMAND_BREAKPOINTS, None)

    def complete(self, text, word):
        """
        Try to complete `word`.

        Assuming `word` is the start of a property qualified name, return all
        possible completions. This is case insensitive, for user convenience.
        """
        prefix = word.lower()
        result = [prop.name for prop in self.context.debug_info.properties
                  if prop.name.lower().startswith(prefix)]
        return result

    def invoke(self, arg, from_tty):
        argv = arg.strip().split(None, 2)

        spec = None
        cond = None

        if len(argv) == 0:
            print('Breakpoint specification missing')
            return

        elif len(argv) == 1:
            spec,  = argv

        elif len(argv) == 3:
            spec, if_kwd, cond = argv
            if if_kwd != 'if':
                print('Invalid arguments (second arg should be "if")')
                return

        else:
            print('Invalid number of arguments')
            return

        bp = (self.break_on_dsl_sloc(spec)
              if ':' in spec else
              self.break_on_property(spec))
        if cond:
            try:
                bp.condition = cond
            except gdb.error as exc:
                print(exc)
                return

    def break_on_property(self, qualname):
        """
        Try to put a breakpoint on a property whose qualified name is
        `qualname`. Display a message for the user if that is not possible.
        """
        lower_prop = qualname.lower()

        for prop in self.context.debug_info.properties:
            if prop.name.lower() == lower_prop:
                break
        else:
            print('No such property: {}'.format(qualname))
            return

        scopes = prop.subscopes
        if not scopes:
            print('{} has no code'.format(prop.name))
            return

        # Break on the first line of the property's first inner scope so that
        # we skip the prologue (all variable declarations).
        return gdb.Breakpoint('{}:{}'.format(
            self.context.debug_info.filename,
            scopes[0].line_range.first_line
        ))

    def break_on_dsl_sloc(self, dsl_sloc):
        """
        Try to put a breakpoint on code that maps to the given DSL source
        location. Display a message for the user if that is not possible.
        """
        dsl_sloc = DSLLocation.parse(dsl_sloc)

        Match = namedtuple('Match', 'prop dsl_sloc line_no')
        matches = []

        def process_scope(prop, scope):
            for e in scope.events:
                if isinstance(e, Scope):
                    process_scope(prop, e)
                elif (isinstance(e, ExprStart)
                      and e.dsl_sloc
                      and e.dsl_sloc.matches(dsl_sloc)):
                    matches.append(Match(prop, e.dsl_sloc, e.line_no))

        for prop in self.context.debug_info.properties:
            process_scope(prop, prop)

        if not matches:
            print('No match for {}'.format(dsl_sloc))
            return

        elif len(matches) == 1:
            m,  = matches

        else:
            print('Multiple matches for {}:'.format(dsl_sloc))

            def idx_fmt(i):
                return '[{}] '.format(i)

            idx_width = len(idx_fmt(len(matches)))
            for i, m in enumerate(matches, 1):
                print('{}In {}, {}'.format(
                    idx_fmt(i).rjust(idx_width),
                    m.prop.name, m.dsl_sloc
                ))
                print('{}at {}:{}'.format(' ' * idx_width,
                                          self.context.debug_info.filename,
                                          m.line_no))

            print('Please chose one of the above locations [default=1]:')
            try:
                choice = get_input('> ')
            except EOFError:
                print('Aborting: no breakpoint created')
                return

            if not choice:
                choice = 1
            else:
                try:
                    choice = int(choice)
                except ValueError:
                    print('Invalid index choice: {}'.format(choice))
                    return

                if choice < 1 or choice > len(matches):
                    print('Choice must be in range {}-{}'.format(
                        1, len(matches)
                    ))
                    return

            m = matches[choice]

        return gdb.Breakpoint('{}:{}'.format(
            self.context.debug_info.filename, m.line_no
        ))


class NextCommand(BaseCommand):
    """Continue execution until reaching another expression."""

    def __init__(self, context):
        super(NextCommand, self).__init__(context, 'next', gdb.COMMAND_RUNNING)

    def invoke(self, arg, from_tty):
        if arg:
            print('This command takes no argument')
            return

        state = self.context.decode_state()
        if not state:
            print('Selected frame is not in a property.')
            return

        scope_state, current_expr = state.lookup_current_expr()

        if current_expr is None:
            # There are only two possible causes for no currently evaluating
            # expressions: either the property just started (root expression
            # evaluation is ahead), either it is about to return (root expr.
            # eval. is behind).
            events = itertools.ifilter(
                lambda e: isinstance(e, ExprStart),
                state.property_scope.scope.iter_events()
            )
            try:
                root_expr = next(iter(events))
            except StopIteration:
                # This function has no computing expression: just finish it.
                # For instance, this happens when properties return a constant
                # boolean.
                root_expr = None

            if root_expr and state.line_no < root_expr.line_no:
                # The first expression is ahead: resume execution until we
                # reach it.
                gdb.execute('until {}'.format(root_expr.line_no))
            else:
                gdb.execute('finish')

        else:
            # Depending on the control flow behavior of the currently running
            # expression, the next step can be either its parent expression or
            # any of its sub-expressions.
            next_slocs_candidates = []

            # First look for the point where the current expression terminates
            # its evaluation.
            next_slocs_candidates.append(current_expr.done_event.line_no)

            # Now look for the starting point for all sub-expressions
            for subexpr in current_expr.start_event.sub_expr_start:
                next_slocs_candidates.append(subexpr.line_no)

            BreakpointGroup(self.context, next_slocs_candidates)
            gdb.execute('continue')

        new_current_expr = None
        new_expr = None

        new_state = self.context.decode_state()
        if new_state:
            _, new_current_expr = new_state.lookup_current_expr()
            if current_expr:
                new_expr = new_state.lookup_expr(current_expr.expr_id)

        # If we just finished the evaluation of an expression, display its
        # value.
        if new_expr and new_expr.is_done:
            print('{} evaluated to {}'.format(
                expr_repr(new_expr),
                new_expr.read(new_state.frame)
            ))

        # Display the expression of most interest, if any
        if new_current_expr:
            print('')
            print('Now evaluating {}'.format(expr_repr(new_current_expr)))


class OutCommand(BaseCommand):
    """Continue execution until the end of the evaluation of the current
sub-expression.
    """

    def __init__(self, context):
        super(OutCommand, self).__init__(context, 'out', gdb.COMMAND_RUNNING)

    def invoke(self, arg, from_tty):
        if arg:
            print('This command takes no argument')
            return

        # Look for the expression that is being evaluated currently
        state = self.context.decode_state()
        if not state:
            print('Selected frame is not in a property.')

        scope_state, current_expr = state.lookup_current_expr()
        if not current_expr:
            print('Not evaluating any expression currently')
            return

        # Look for the point in the generated library where its evaluation will
        # be done.
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
        new_state = self.context.decode_state(frame)
        new_expr = self.lookup_expr(new_state, current_expr.expr_id)
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

    def lookup_expr(self, state, expr_id):
        """
        Look for the ExpressionEvaluation instance in `state` whose id is
        `expr_id`.

        :rtype: langkit.gdb.state.ExpressionEvaluation|None
        """
        for scope_state in state.scopes:
            for e in scope_state.expressions.values():
                if e.expr_id == expr_id:
                    return e
