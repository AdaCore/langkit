from os import path as P

from drivers.python_driver import PythonDriver


class LktResolveDriver(PythonDriver):
    """
    Custom Python driver that will automatically run the "lkt_resolve.py"
    script on the testcase's "test.lkt" file.
    """

    @property
    def mandatory_files(self):
        return ['test.lkt']

    @property
    def script_and_args(self):
        LKT_LIB_DIR = P.join(self.langkit_root_dir, 'contrib', 'lkt')
        return [P.join(LKT_LIB_DIR, 'lkt_resolve.py'), 'test.lkt']
