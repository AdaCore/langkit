import gdb

from langkit.gdb.utils import dereference_fat_array_ptr


class AnalysisUnit:
    """
    Helper to deal with analysis units.
    """

    def __init__(self, value: gdb.Value):
        self.value = value

    @property
    def filename(self) -> str:
        virtual_file = self.value['filename']
        vf_record = virtual_file['value'].dereference()
        v = dereference_fat_array_ptr(vf_record['full'])
        return v.string()
