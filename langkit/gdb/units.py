from __future__ import absolute_import, division, print_function

try:
    from gnatdbg.strings import UnboundedString
except ImportError:
    fetch_unbounded_string = None
else:
    fetch_unbounded_string = (
        lambda value: UnboundedString(value).get_string()
    )


class AnalysisUnit(object):
    """
    Helper to deal with analysis units.
    """

    def __init__(self, value):
        self.value = value

    @property
    def filename(self):
        return (fetch_unbounded_string(self.value['file_name'])
                if fetch_unbounded_string else None)
