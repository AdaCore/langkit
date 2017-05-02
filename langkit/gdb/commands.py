from __future__ import absolute_import, division, print_function

import gdb

from langkit.gdb.state import State


class BaseCommand(gdb.Command):
    """
    Factorize common code for our commands.
    """

    def __init__(self, context, basename, command_class,
                 completer_class=gdb.COMPLETE_NONE):
        super(BaseCommand, self).__init__(
            '{}{}'.format(context.prefix, basename),
            command_class, completer_class
        )
        self.context = context


class StateCommand(BaseCommand):
    """Display the state of the currently running property.

This command may be followed by a "/X" flag, where X is one or several of:

    * s: to print the name of the Ada variables that hold DSL values.
"""

    def __init__(self, context):
        super(StateCommand, self).__init__(context, 'state', gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        with_locs = False

        if arg:
            if not arg.startswith('/'):
                print('Invalid argument')
                return

            for c in arg[1:]:
                if c == 's':
                    with_locs = True
                else:
                    print('Invalid flag: {}'.format(repr(c)))

        frame = gdb.selected_frame()
        state = State.decode(self.context, frame)
        if state is None:
            print('Selected frame is not in a property.')
            return

        print('Running {}'.format(state.property.name))
        print('from {}'.format(state.property.dsl_sloc))

        for scope_state in state.scopes:
            is_first = [True]

            def print_info(*args, **kwargs):
                if is_first[0]:
                    print('')
                    is_first[0] = False
                print(*args, **kwargs)

            for b in scope_state.bindings:
                # Read the value associated to this binding. Switching
                # to lower-case is required since GDB ignores case
                # insentivity for Ada from the Python API.
                value = frame.read_var(b.gen_name.lower())
                print_info('{}{} = {}'.format(
                    b.dsl_name,
                    ' ({})'.format(b.gen_name) if with_locs else '',
                    value
                ))

            last_started = None
            for e in scope_state.expressions.values():
                if e.is_started:
                    last_started = e
                elif e.is_done:
                    print_info('{} -> {}'.format(
                        e.expr_repr,
                        frame.read_var(e.result_var.lower())
                    ))
            if last_started:
                print_info('Currently evaluating {}'.format(
                    last_started.expr_repr
                ))
                if last_started.expr_loc:
                    print_info('from {}'.format(last_started.expr_loc))


class BreakCommand(BaseCommand):
    """Put a breakpoint on a property. Takes a case-insensitive property
qualified name. For instance::

    *break MyNode.p_property
"""

    def __init__(self, context):
        super(BreakCommand, self).__init__(context, 'break',
                                           gdb.COMMAND_BREAKPOINTS)

    def invoke(self, arg, from_tty):
        lower_prop = arg.strip().lower()
        if not lower_prop:
            print('Missing breakpoint specification')
            return

        for prop in self.context.debug_info.properties:
            if prop.name.lower() == lower_prop:
                break
        else:
            print('No such property: {}'.format(arg.strip()))
            return

        scopes = prop.subscopes
        if not scopes:
            print('{} has no code'.format(prop.name))
            return

        # Break on the first line of the property's first inner scope so that
        # we skip the prologue (all variable declarations).
        gdb.Breakpoint('{}:{}'.format(
            self.context.debug_info.filename,
            scopes[0].line_range.first_line
        ))
