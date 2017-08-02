from __future__ import absolute_import, division, print_function

import os
import os.path

# pyflakes off
with_gnatpython = False
if not os.environ.get('WITHOUT_GNATPYTHON'):
    try:
        from gnatpython import fileutils
        from gnatpython.ex import Run, STDOUT
        from gnatpython.testsuite.driver import TestDriver
    except ImportError:
        pass
    else:
        with_gnatpython = True
if not with_gnatpython:
    from testsuite_support.polyfill import fileutils, Run, STDOUT, TestDriver
# pyflakes on


class SetupError(Exception):
    """Exception to raise when the testcase is invalid.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


class TestError(Exception):
    """Exception to raise when the testcase fails.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


def catch_test_errors(func):
    """
    Helper decorator for driver entry points.

    This returns a wrapper around func that catches SetupError and TestError
    exceptions and that turns them into the appropriate test status. Using
    exceptions is convenient to stop any method from any point: this simplifies
    the control flow.
    """

    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except SetupError as exc:
            self.set_setup_error(exc.message)
        except TestError as exc:
            self.set_failure(exc.message)
    return wrapper


class BaseDriver(TestDriver):
    """
    Base class to provide common test driver helpers.

    Ideally, these should end up in GNATpython, but this base class acts as a
    staging area: once it has been proven that some feature is useful, it may
    be easier to submit it upstream...
    """

    TIMEOUT = None

    def tear_up(self):
        super(BaseDriver, self).tear_up()
        self.create_test_workspace()
        self.check_file(self.expected_file)

        # See if we expect a failure for this testcase
        try:
            comment = self.test_env['expect_failure']
        except KeyError:
            self.expect_failure = False
            self.expect_failure_comment = None
        else:
            # Because of wrapping in the YAML file, we can get multi-line
            # strings, which is not valid for comments.
            comment = comment.replace('\n', ' ').strip()

            self.expect_failure = True
            if not (comment is None or isinstance(comment, basestring)):
                raise SetupError('Invalid "expect_failure" entry:'
                                 ' expected a string but got {}'.format(
                                     type(comment)))
            self.expect_failure_comment = comment

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, 'r') as f:
            return f.read()

    def set_setup_error(self, message):
        self.result.set_status('PROBLEM', message)

    def set_failure(self, message):
        if self.expect_failure:
            self.result.set_status('XFAIL', '{}{}'.format(
                message,
                ' ({})'.format(self.expect_failure_comment)
                if self.expect_failure_comment else ''
            ))
        else:
            self.result.set_status('FAILED', message)

    def set_passed(self):
        if self.expect_failure:
            msg = (
                'Failure was expected: {}'.format(self.expect_failure_comment)
                if self.expect_failure_comment else None
            )
            self.result.set_status('UOK', msg)
        else:
            self.result.set_status('PASSED')

    # Convenience path builders

    @property
    def langkit_root_dir(self):
        """Return the absolute path to the repository root directory."""
        return os.path.abspath(
            os.path.join(self.testsuite_dir, b'..')
        )

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              b'..')
        return os.path.abspath(result)

    @property
    def test_dir(self):
        """Return the path of the current testcase directory."""
        return self.test_env['test_dir']

    def working_dir(self, *args):
        """
        Return the working dir, plus any path elements joined to it if passed
        in *args.
        """
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'], *args)

    @property
    def output_file(self):
        return self.working_dir('actual.out')

    @property
    def expected_file(self):
        return self.working_dir('test.out')

    @property
    def original_expected_file(self):
        return os.path.join(self.test_dir, 'test.out')

    @property
    def coverage_enabled(self):
        return self.global_env['options'].coverage

    def coverage_file(self, ext):
        """
        Return the name of a coverage data file (or trace file) for the current
        test.

        :param str ext: File extension for this file.
        :rtype: str
        """
        return os.path.join(
            self.global_env['coverage_dir'],
            self.test_env['test_name'] + '.' + ext
        )

    #
    # Tear up helpers
    #

    @property
    def python_interpreter(self):
        return self.global_env['options'].with_python or b'python'

    def check_file(self, filename):
        """
        Check file presence.

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(os.path.join(self.test_dir, filename)):
            raise SetupError('Missing mandatory file: {}'.format(filename))

    def check_file_list(self, what, file_list, can_be_empty=True):
        """Raise a SetupError if `file_list` is not a list of existing files.

        Also raise an error if it is an empty list while `can_be_empty` is
        False.
        """
        # First check we have a list of strings
        if (not isinstance(file_list, list) or
                (not can_be_empty and len(file_list) == 0) or
                not all(isinstance(fn, basestring) for fn in file_list)):
            empty_msg = 'non-empty '
            raise SetupError(
                '{} must be a {}list of strings'.format(what, empty_msg))

        # Then check that these are existing files
        for filename in file_list:
            self.check_file(filename)

    def create_test_workspace(self):
        """
        Create a test workspace.

        This function copies the test sources into the working directory.
        """

        fileutils.sync_tree(self.test_dir, self.working_dir())

    def add_path(self, env_var, path):
        """
        Add a path to some environment variable.

        :param str env_var: Name of the environment variable to define/extend.
        :param str path: Path to prepend.
        """
        assert isinstance(env_var, str)
        assert isinstance(path, str)
        path_list = os.environ.get(env_var, b'')
        assert isinstance(path_list, str)
        if path_list:
            path_list = b'{}{}{}'.format(
                path, os.path.pathsep, path_list
            )
        else:
            path_list = path

        os.environ[env_var] = path_list

    #
    # Run helpers
    #

    def run_and_check(self, argv, env=None, for_coverage=False):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        In case of failure, the test output is appended to the actual output
        and a TestError is raised.

        :param list[str] argv: List of arguments to pass to the subprocess.
        :param None|dict[str, str] env: If provided, environment variables to
            pass to the subprocess.
        :param bool for_coverage: If true and if coverage is enabled, produce a
            trace file.
        """
        program = argv[0]

        if for_coverage and self.coverage_enabled:
            trace_file = self.coverage_file('trace')
            argv = ['gnatcov', 'run', '-o', trace_file] + argv

        p = Run(argv, cwd=self.working_dir(),
                timeout=self.TIMEOUT,
                output=self.output_file,
                error=STDOUT,
                env=env)

        if p.status != 0:
            self.result.actual_output += (
                '{} returned status code {}\n'.format(program, p.status))
            self.result.actual_output += self.read_file(self.output_file)
            raise TestError(
                '{} returned status code {}'.format(program, p.status))

    def create_project_file(self, project_file, mains):
        """
        Create a project file for the given main source files.

        The project file is created in the working directory. It gathers the
        Ada source files in the working directory.

        :param str project_file: Project file name to create.
        :param list[str] mains: List of main source files.
        """
        with open(self.working_dir(project_file), 'w') as f:
            f.write("""
            with "{lk_support}";

            project P is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ({mains});
            end P;
            """.format(
                mains=', '.join('"{}"'.format(m) for m in mains),
                lk_support=os.path.join(self.testsuite_dir, '..', 'langkit',
                                        'support', 'langkit_support.gpr')
            ))

    def gprbuild(self, project_file):
        """
        Run GPRbuild on the given project file.

        :param str project_file: Project file name.
        """
        argv = ['gprbuild', '-P', project_file, '-p']
        cargs = ['-O0', '-g', '-gnata']
        if self.coverage_enabled:
            argv.append('--subdirs=gnatcov')
            cargs += ['-fdump-scos', '-fpreserve-control-flow']
        self.run_and_check(argv + ['-cargs'] + cargs)

    def program_path(self, main_source_file):
        """
        Return the path to the program corresponding to the given main file.

        :param str main_source_file: File name for the main source file from
            which the program has been built.
        :rtype: str
        """
        assert main_source_file.endswith('.adb')
        program_name = main_source_file[:-4]
        return (self.working_dir('gnatcov', program_name)
                if self.coverage_enabled else
                self.working_dir(program_name))

    #
    # Analysis helpers
    #

    def analyze(self):
        rewrite = (self.global_env['options'].rewrite
                   and not self.expect_failure)
        failures = []

        # Check for the test output itself
        diff = fileutils.diff(self.expected_file, self.output_file,
                              ignore_white_chars=False)
        if diff:
            if rewrite:
                new_baseline = self.read_file(self.output_file)
                with open(self.original_expected_file, 'w') as f:
                    f.write(new_baseline)
            self.result.actual_output += diff
            failures.append('output is not as expected{}'.format(
                ' (baseline updated)' if rewrite else ''
            ))

        if failures:
            self.set_failure(' | '.join(failures))
        else:
            self.set_passed()
