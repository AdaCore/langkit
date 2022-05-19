import os
import os.path

from drivers.python_driver import PythonDriver


class LktCompileDriver(PythonDriver):
    """
    Test script to compile all Lkt sources in the current directory up to code
    emission and to print error messages.
    """

    @property
    def mandatory_files(self):
        return []

    @property
    def script_and_args(self):
        result = [os.path.join(self.support_dir, "lkt_compile.py")]
        if self.test_env.get("generate_unparser"):
            result.append("--generate-unparser")
        return result
