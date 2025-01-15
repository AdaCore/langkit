import os
import os.path

from drivers.python_driver import PythonDriver


class LktCompileDriver(PythonDriver):
    """
    Driver that compiles all Lkt sources in the current directory up to code
    emission and prints error messages (if any). If there is a "test.py" script
    in the test directory, also execute it at the end of the driver execution.
    """

    @property
    def mandatory_files(self):
        return []

    @property
    def script_and_args(self):
        return [os.path.join(self.support_dir, "lkt_compile.py")]
