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
        result = [os.path.join(self.support_dir, "lkt_compile.py")]
        if self.test_env.get("all_warnings"):
            result.append("--all-warnings")
        if self.test_env.get("generate_unparser"):
            result.append("--generate-unparser")
        return result
