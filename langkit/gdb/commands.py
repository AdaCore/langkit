from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb


def current_line_no(context):
    """
    If the current frame is in the $-analysis.adb file, return its currently
    executed line number. Return None otherwise.

    :rtype: int|None
    """
    frame = gdb.selected_frame()
    current_file = frame.function().symtab.fullname()
    if current_file == context.line_map.filename:
        return frame.find_sal().line


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
    """Display the state of the currently running property."""

    def __init__(self, context):
        super(StateCommand, self).__init__(context, 'state', gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        line_no = current_line_no(self.context)
        p = (self.context.line_map.lookup_property(line_no)
             if line_no else None)
        if p is None:
            print('Selected frame is not in a property.')
            return

        print('Running {}'.format(p.name))
        print('from {}'.format(p.dsl_sloc))


class BreakCommand(BaseCommand):
    """But a breakpoint on a property. Takes a case-insensitive property
qualified name. For instance::

    *break MyNode.p_property
"""

    def __init__(self, context):
        super(BreakCommand, self).__init__(context, 'break',
                                           gdb.COMMAND_BREAKPOINTS)

    def invoke(self, arg, from_tty):
        lower_prop = arg.strip().lower()
        for prop in self.context.line_map.properties:
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
            self.context.line_map.filename,
            scopes[0].line_range.first_line
        ))
