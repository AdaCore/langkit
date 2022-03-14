import os
import os.path
import sys

from drivers.base_driver import BaseDriver


class PythonDriver(BaseDriver):
    default_process_timeout = 600

    #
    # Driver entry poins
    #

    def set_up(self):
        super().set_up()
        for f in self.mandatory_files:
            self.check_file(f)

    @property
    def mandatory_files(self):
        """
        Return the set of mandatory files for this driver. Meant to be
        overloaded by subclasses.
        """
        return ['test.py']

    @property
    def script_and_args(self):
        """
        Return the name of the python script to run, as well as the arguments.
        Meant to be overloaded by subclasses.
        """
        return ['test.py']

    def run(self):
        # The "test.py" script will not import a generated library, however
        # another spawned script could: provide the path to the interpreter in
        # the environment so it can use it.
        derived_env = dict(os.environ)
        derived_env['PYTHON_INTERPRETER'] = self.python_interpreter
        derived_env['LANGKIT_ROOT_DIR'] = self.langkit_root_dir
        derived_env['LANGKIT_PRETTY_PRINT'] = str(
            int(self.env.options.pretty_print))
        derived_env['LANGKIT_JOBS'] = str(self.env.inner_jobs)

        # Unless this mechanism is specifically disabled, make the Langkit
        # library relative to this testsuite available to tests.
        if not self.env.options.no_auto_path:
            self.add_path(derived_env, 'PYTHONPATH', self.langkit_root_dir)

        # Make the common Python modules available from the testcase script
        self.add_path(derived_env, 'PYTHONPATH', self.support_dir)
        self.add_path(derived_env, 'PYTHONPATH', self.env.root_dir)

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

        self.run_and_check(argv + self.script_and_args, derived_env)

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, 'python_support')

    def compute_failures(self):
        # RA22-015: For the transition to the concrete syntax, we want to
        # check-in and test unparsing results.
        failures = super().compute_failures()

        expected_lkt = self.test_dir('expected_concrete_syntax.lkt')
        actual_lkt = self.working_dir('concrete_syntax.lkt')

        # Some tests do not create a concrete_syntax.lkt file. There is nothing
        # to compare for them.
        if not os.path.exists(actual_lkt):
            return failures

        # We just open the file in append mode, to create it if it doesn't
        # exist.
        with open(expected_lkt, 'a+'):
            pass
        expected = self.read_file(expected_lkt)

        failures += self.compute_diff(
            expected_lkt, expected,
            self.read_file(actual_lkt),
            failure_message="unexpected concrete syntax"
        )
        return failures
