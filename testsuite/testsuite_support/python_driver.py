import os
import os.path
import sys

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class PythonDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()
        self.check_file('test.py')

        # Make the common Python modules available from the testcase script
        self.add_path('PYTHONPATH', self.support_dir)
        self.add_path('PYTHONPATH', self.langkit_root_dir)

    @catch_test_errors
    def run(self):
        # The "test.py" script will not import a generated library, however
        # another spawned script could: provide the path to the interpreter in
        # the environment so it can use it.
        derived_env = dict(os.environ)
        derived_env['PYTHON_INTERPRETER'] = self.python_interpreter

        self.run_and_check([sys.executable, 'test.py'], derived_env)

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, 'python_support')
