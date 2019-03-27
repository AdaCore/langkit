from __future__ import absolute_import, division, print_function

import os
import os.path
import sys

import testsuite_support
from testsuite_support.base_driver import BaseDriver, catch_test_errors


class PythonDriver(BaseDriver):
    TIMEOUT = 600

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()
        self.check_file('test.py')

        # Unless this mechanism is specifically disabled, make the Langkit
        # library relative to this testsuite available to tests.
        if not self.global_env['options'].no_auto_path:
            self.add_path(b'PYTHONPATH', self.langkit_root_dir)

        # Make the common Python modules available from the testcase script
        self.add_path(b'PYTHONPATH', self.support_dir)
        self.add_path(
            b'PYTHONPATH',
            os.path.dirname(os.path.dirname(
                os.path.abspath(testsuite_support.__file__)
            ))
        )

    @catch_test_errors
    def run(self):
        # The "test.py" script will not import a generated library, however
        # another spawned script could: provide the path to the interpreter in
        # the environment so it can use it.
        derived_env = dict(os.environ)
        derived_env[b'PYTHON_INTERPRETER'] = self.python_interpreter
        derived_env[b'LANGKIT_ROOT_DIR'] = self.langkit_root_dir
        derived_env['LANGKIT_PRETTY_PRINT'] = str(
            int(self.global_env['pretty_print']))

        # Assign a sane default language source directory for Langkit's
        # diagnostics.
        derived_env['LANGKIT_LANG_SOURCE_DIR'] = self.working_dir()

        # If code coverage is requested, run the test script under the
        # "coverage" program.
        if self.coverage_enabled:
            # Consider all Langkit Python source files, except modules which
            # are currently not tested at all.
            source = os.path.join(self.langkit_root_dir, 'langkit')
            argv = [
                'coverage', 'run', '--source={}'.format(source),
                '--rcfile={}'.format(os.path.join(self.testsuite_dir,
                                                  'coverage.ini'))
            ]
            derived_env['COVERAGE_FILE'] = self.coverage_file('coverage')
        else:
            argv = [sys.executable]

        if self.valgrind_enabled:
            derived_env['VALGRIND_ENABLED'] = '1'

        self.run_and_check(argv + ['test.py'], derived_env)

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, b'python_support')
