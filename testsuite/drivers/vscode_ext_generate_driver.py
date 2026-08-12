import os
import os.path

from drivers.python_driver import PythonDriver


class VSCodeExtGenerateDriver(PythonDriver):
    """
    Driver that generate a VS code extension for the provided configuration and
    prints error messages (if any).

    If no other configuration is provided, this driver requires a "test.lkt"
    file in the case directory that will be used as Lkt entry point.

    If there is a "test.py" script in the test directory, also execute it at
    the end of the driver execution.
    """

    @property
    def script_and_args(self):
        return [os.path.join(self.support_dir, "vscode_ext_generate.py")]
