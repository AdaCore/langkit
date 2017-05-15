from __future__ import absolute_import, division, print_function

import gdb


class BreakpointGroup(object):
    """
    List of breakpoints to be considered as a single temporary one.

    This is useful to implement high-level control-flow primitive. If any
    breakpoint is hit or if the inferior stops/exits, we remove all
    breakpoints.
    """

    def __init__(self, context, line_nos):
        self.context = context
        self.breakpoints = [_Breakpoint(context, l)
                            for l in line_nos]

        self._event_callback = lambda _: self.cleanup()
        gdb.events.stop.connect(self._event_callback)
        gdb.events.exited.connect(self._event_callback)

    def cleanup(self):
        """
        Remove all our breakpoints and unregister our GDB event handlers.
        """
        for bp in self.breakpoints:
            bp.delete()
        gdb.events.stop.disconnect(self._event_callback)
        gdb.events.exited.disconnect(self._event_callback)


class _Breakpoint(gdb.Breakpoint):
    """
    Helper for BreakpointGroup's internal breakpoints that stop the inferior
    when hit.
    """

    def __init__(self, context, line_no):
        super(_Breakpoint, self).__init__(
            '{}:{}'.format(context.debug_info.filename, line_no),
            internal=True
        )

    def stop(self):
        return True
