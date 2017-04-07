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


class StateCommand(gdb.Command):
    """Display the state of the currently running property."""

    def __init__(self, context):
        super(StateCommand, self).__init__(
            '{}state'.format(context.prefix),
            gdb.COMMAND_DATA, gdb.COMPLETE_NONE
        )
        self.context = context

    def invoke(self, arg, from_tty):
        line_no = current_line_no(self.context)
        p = (self.context.line_map.lookup_property(line_no)
             if line_no else None)
        if p is None:
            print('Selected frame is not in a property.')
            return

        print('Running {}'.format(p.name))
