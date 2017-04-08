from __future__ import (absolute_import, division, print_function,
                        unicode_literals)


def analysis_line_no(context, frame):
    """
    If the given frame is in the $-analysis.adb file, return its currently
    executed line number. Return None otherwise.

    :type frame: gdb.Frame
    :rtype: int|None
    """
    current_file = frame.function().symtab.fullname()
    return (frame.find_sal().line
            if current_file == context.line_map.filename else None)


class State(object):
    """
    Holder for the execution state of a property.
    """

    def __init__(self, prop):
        self.property = prop
        """
        :type: langkit.gdb.line_map.Property
        The property currently running.
        """

    @classmethod
    def decode(cls, context, frame):
        """
        Decode the execution state from the given GDB frame. Return None if no
        property is running in this frame.

        :type frame: gdb.Frame
        """
        line_no = analysis_line_no(context, frame)
        prop = context.line_map.lookup_property(line_no) if line_no else None
        if prop is None:
            return None

        return cls(prop)
