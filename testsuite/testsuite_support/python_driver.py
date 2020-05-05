from __future__ import absolute_import, division, print_function

import os
import os.path
import sys

import testsuite_support
from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, with_gnatpython
)


if with_gnatpython:
    from gnatpython import fileutils
else:
    from testsuite_support.polyfill import fileutils


class PythonDriver(BaseDriver):
    TIMEOUT = 600

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()
        for f in self.mandatory_files():
            self.check_file(f)

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

    def mandatory_files(self):
        """
        Return the set of mandatory files for this driver. Meant to be
        overloaded by subclasses.
        """
        return ["test.py"]

    def script_and_args(self):
        """
        Return the name of the python script to run, as well as the arguments.
        Meant to be overloaded by subclasses.
        """
        return ["test.py"]

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

        self.run_and_check(argv + self.script_and_args(), derived_env)

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, b'python_support')

    def analyze(self):
        # RA22-015: For the transition to the concrete syntax, we want to
        # check-in and test unparsing results.
        failures = super(PythonDriver, self).analyze()

        rewrite = (self.global_env['options'].rewrite
                   and not self.expect_failure)

        expected_lkt = self.working_dir('expected_concrete_syntax.lkt')
        actual_lkt = self.working_dir('concrete_syntax.lkt')

        if not os.path.exists(actual_lkt):
            return failures

        # We just open the file in append mode, to create it if it doesn't
        # exist.
        with open(expected_lkt, 'a+'):
            pass

        # Check for the test output itself
        diff = fileutils.diff(expected_lkt, actual_lkt,
                              ignore_white_chars=False)
        if diff:
            if rewrite:
                new_baseline = self.read_file(actual_lkt)
                with open(os.path.join(
                    self.test_dir, 'expected_concrete_syntax.lkt'
                ), 'w') as f:
                    f.write(new_baseline)
            self.result.actual_output += diff
            failures.append('output is not as expected{}'.format(
                ' (baseline updated)' if rewrite else ''
            ))

        if failures:
            self.set_failure(' | '.join(failures))
        else:
            self.set_passed()
