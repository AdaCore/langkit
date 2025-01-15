import os
import os.path

from drivers.python_driver import PythonDriver


class LktBuildAndRunDriver(PythonDriver):
    """
    Driver to generate/build the "test.lkt" language specification in the
    current directory and optionally build and run test programs
    (Ada/C/Python/OCaml/Java) with the generated library.

    In addition to the common "test.yaml" keys supported in the Langkit
    testsuite, this driver accepts keys that match arguments of the
    python_suport/utils.py:build_and_run function.

    The "post_scripts" key is an optional list of Python scripts to run after
    the compilation/main execution cycles have completed.
    """

    @property
    def mandatory_files(self):
        return []

    @property
    def script_and_args(self):
        return [os.path.join(self.support_dir, "lkt_build_and_run.py")]
