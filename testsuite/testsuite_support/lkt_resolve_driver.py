from __future__ import absolute_import, division, print_function

from os import path as P
from testsuite_support.python_driver import PythonDriver


class LktResolveDriver(PythonDriver):
    """
    Custom python driver that will automatically run 'lkt_resolve.py' on the
    CWD's "test.lkt".
    """
    def mandatory_files(self):
        return ['test.lkt']

    def script_and_args(self):
        LKT_LIB_DIR = P.join(self.langkit_root_dir, 'contrib', 'lkt')
        return [P.join(LKT_LIB_DIR, 'lkt_resolve.py'), 'test.lkt']
