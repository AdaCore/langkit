from typing import List, TYPE_CHECKING

import gdb

from langkit.gdb.context import Context


if TYPE_CHECKING:
    # TODO (V603-004): gdb.events is automatically imported, but importing it
    # manually does not work (yet we need it for proper type checking).
    import gdb.events


class BreakpointGroup:
    """
    List of breakpoints to be considered as a single temporary one.

    This is useful to implement high-level control-flow primitive. If any
    breakpoint is hit or if the inferior stops/exits, we remove all
    breakpoints.
    """

    def __init__(self, context: Context, line_nos: List[int]):
        self.context = context
        self.breakpoints = [_Breakpoint(context, l)
                            for l in line_nos]

        self._event_callback = lambda _: self.cleanup()
        gdb.events.stop.connect(self._event_callback)
        gdb.events.exited.connect(self._event_callback)

    def cleanup(self) -> None:
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

    def __init__(self, context: Context, line_no: int):
        super().__init__(
            '{}:{}'.format(context.debug_info.filename, line_no),
            internal=True
        )

    def stop(self) -> bool:
        return True
