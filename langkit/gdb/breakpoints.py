from __future__ import annotations

import dataclasses
from typing import TYPE_CHECKING

import gdb

from langkit.debug_info import AdaLocation
from langkit.gdb.context import Context


if TYPE_CHECKING:
    # TODO (V603-004): gdb.events is automatically imported, but importing it
    # manually does not work (yet we need it for proper type checking).
    import gdb.events


@dataclasses.dataclass
class FrameSignature:
    """
    Signature for a GDB frame.

    ``gdb.Frame`` instances are valid only as long as the corresponding stack
    frame exists. This dataclass provides a signature for frames, that survive
    their frames. We use them for equality, to check if a frame at some point
    is the same frame that existed earlier.
    """

    index: int
    """
    0-based index of the frame: the oldest frame (for the "main" subprogram)
    has index 0, then the frame that comes next (the subprogram called by
    "main") has index 1, etc.

    This index is the same as ``gdb.Frame.level``, but with the opposite
    ordering.
    """

    @classmethod
    def from_frame(cls, frame: gdb.Frame | None = None) -> FrameSignature:
        """
        Compute the signature of the given frame (or the currently selected
        frame, if None is passed).
        """
        index = 0
        f: gdb.Frame | None = frame or gdb.selected_frame()
        while f is not None:
            index += 1
            f = f.older()
        return cls(index)


class BreakpointGroup:
    """
    List of breakpoints to be considered as a single temporary one.

    This is useful to implement high-level control-flow primitive. If any
    breakpoint is hit or if the inferior stops/exits, we remove all
    breakpoints.
    """

    def __init__(
        self,
        context: Context,
        locs: list[AdaLocation],
        same_call: bool = False,
    ):
        """
        :param locs: Locations for all the breakpoints to create in this group.
        :param same_call: Whether breakpoints must trigger in the same call
            frame as the currently selected frame.
        """
        frame_sig = FrameSignature.from_frame() if same_call else None
        self.context = context
        self.breakpoints = [_Breakpoint(context, l, frame_sig) for l in locs]

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

    def __init__(
        self,
        context: Context,
        loc: AdaLocation,
        frame_sig: FrameSignature | None,
    ):
        super().__init__(loc.gdb_spec, internal=True)
        self.frame_sig = frame_sig

    def stop(self) -> bool:
        # Stop the inferior if no frame signature was given, or if the selected
        # frame matches it.
        return (
            self.frame_sig is None
            or FrameSignature.from_frame() == self.frame_sig
        )
