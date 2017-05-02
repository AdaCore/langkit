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
        self.state = State.decode(self.context, self.frame)

        self.with_ellipsis = True
        self.with_locs = False

    def run(self):
        if self.state is None:
            print('Selected frame is not in a property.')
            return

        print('Running {}'.format(self.state.property.name))
        print('from {}'.format(self.state.property.dsl_sloc))

        for scope_state in self.state.scopes:
            is_first = [True]

            def print_info(*args, **kwargs):
                if is_first[0]:
                    print('')
                    is_first[0] = False
                print(*args, **kwargs)

            for b in scope_state.bindings:
                print_info('{}{} = {}'.format(
                    b.dsl_name,
                    ' ({})'.format(b.gen_name) if self.with_locs else '',
                    self.value_image(b.gen_name)
                ))

            last_started = None
            for e in scope_state.expressions.values():
                if e.is_started:
                    last_started = e
                elif e.is_done:
                    print_info('{} -> {}'.format(
                        e.expr_repr,
                        self.value_image(e.result_var)
                    ))
            if last_started:
                print_info('Currently evaluating {}'.format(
                    last_started.expr_repr
                ))
                if last_started.expr_loc:
                    print_info('from {}'.format(last_started.expr_loc))

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
