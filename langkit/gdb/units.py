from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

try:
    from gnatdbg.strings import UnboundedStringPrinter
except ImportError:
    fetch_unbounded_string = None
else:
    fetch_unbounded_string = (
        lambda value: UnboundedStringPrinter(value).get_string_value()
    )


class AnalysisUnit(object):
    """
    Helper to deal with analysis units.
    """

    def __init__(self, value):
        self.value = value

    @property
    def filename(self):
        return (eval(fetch_unbounded_string(self.value['file_name']))
                if fetch_unbounded_string else None)
