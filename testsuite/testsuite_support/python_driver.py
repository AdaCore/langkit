import os
import os.path

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


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
        self.run_and_check([self.python_interpreter, 'test.py'])

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, 'python_support')
