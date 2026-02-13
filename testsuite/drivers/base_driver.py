import glob
import os
import os.path
import sys

from e3.fs import sync_tree
from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.driver.diff import (
    DiffTestDriver,
    OutputRefiner,
    PatternSubstitute,
    Substitute,
)

from drivers.valgrind import valgrind_cmd


class PythonTracebackCollapser(OutputRefiner[str]):
    """Remove quoted lines of code from Python tracebacks.

    This is useful to cope with variations depending on Python interpreter
    versions.
    """

    # Sub-refiner to hide line numbers from stack frames
    linenos_refiner = PatternSubstitute(r" line \d+, ", " line XXX, ")

    def __init__(self, hide_linenos: bool):
        self.hide_linenos = hide_linenos

    def refine(self, output: str) -> str:
        result = []

        # To collapse source code quoted in tracebacks, proceed line by line...

        # Are we in the middle of a Python traceback?
        in_traceback = False

        # Are we in the middle of quoted source code?
        in_source_code = False

        for l in output.splitlines():
            if l.startswith("Traceback (most recent call last):"):
                # This marks the beginning of a Python traceback
                in_traceback = True

            elif in_traceback:
                # In tracebacks, quoted source code starts with 4 spaces while
                # frame locations ("File FFF, line LLL, in FFF") start with
                # 2 spaces.
                if not l.startswith("    "):
                    # Less than 4 spaces of indentation: this is not quoted
                    # source code anymore.
                    in_source_code = False
                    if not l.startswith("  "):
                        # Not even 2 spaces of indentation: we are now out of
                        # the traceback.
                        in_traceback = False
                    elif self.hide_linenos:
                        l = self.linenos_refiner.refine(l)

                elif not in_source_code:
                    # We do have a 4-spaces indentation. This is the first line
                    # of quoted source code: add a single "<source code>"
                    # marker for this quoted block.
                    l = "     <source code>"
                    in_source_code = True
                else:
                    # This is the continuation of quoted source code: just skip
                    # it.
                    continue
            result.append(l + "\n")
        return "".join(result)


class BaseDriver(DiffTestDriver):
    """
    Base class to provide common test driver helpers.
    """

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator(self.env.control_condition_env)

    def set_up(self):
        super().set_up()

        # Allow tests to clone directories in their own working dir, such
        # as common dependencies between multiple tests.
        for path in self.test_env.get("sync_trees", []):
            sync_tree(self.test_dir(path), self.working_dir(), delete=False)

    def tear_down(self):
        # Allow test drivers to create "*.log" files in their working space
        # just for logging purposes, and so forward them to the test result,
        # for post-mortem investigation.
        for log_file in sorted(glob.glob(self.working_dir("*.log"))):
            with open(log_file) as f:
                log_content = f.read()

            self.result.log += (
                f"== Content of {os.path.basename(log_file)} =="
                f"\n\n{log_content}"
                "\n\n== END =="
            )

        super().tear_down()

    @property
    def output_refiners(self):
        result = super().output_refiners

        # Replace references to the working directory with a placeholder, so
        # that baselines are not influenced by where the testsuite is run.
        pattern = self.working_dir()
        repl = "<working-dir>"
        if self.default_encoding == "binary":
            pattern = pattern.encode("ascii")
            repl = repl.encode("ascii")
        result.append(Substitute(pattern, repl))

        # If requested, canonicalize Windows-style directory separators to
        # Unix-style.
        if self.test_env.get("canonicalize_directory_separators", False):
            result.append(Substitute("\\", "/"))

        # If requested, collapse Python tracebacks and hide line numbers
        if self.test_env.get("collapse_python_tracebacks"):
            result.append(
                PythonTracebackCollapser(
                    hide_linenos=self.test_env.get(
                        "hide_python_traceback_linenos", False
                    )
                )
            )

        return result + [
            # Hide platform-specific details from Python traceback
            PatternSubstitute(
                r'File "[^"]*[/\\]([^"/\\]+)"',
                r'File ".../\1"',
            ),
        ]

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, "r") as f:
            return f.read()

    # Convenience path builders

    @property
    def langkit_root_dir(self):
        """Return the absolute path to the repository root directory."""
        return os.path.abspath(os.path.join(self.testsuite_dir, ".."))

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
        return os.path.abspath(result)

    @property
    def coverage_enabled(self):
        return self.env.options.coverage

    def coverage_file(self, ext):
        """
        Return the name of a coverage data file (or trace file) for the current
        test.

        :param str ext: File extension for this file.
        :rtype: str
        """
        return os.path.join(
            self.env.coverage_dir, self.test_env["test_name"] + "." + ext
        )

    @property
    def valgrind_enabled(self):
        return self.env.options.valgrind

    #
    # Tear up helpers
    #

    @property
    def langkit_python_interpreter(self):
        """
        See the documentation for --with-langkit-python.
        """
        return self.env.options.with_langkit_python or sys.executable

    @property
    def python_interpreter(self):
        """
        See the documentation for --with-python.
        """
        return self.env.options.with_python or "python"

    def check_file(self, filename):
        """
        Check file presence.

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(self.test_dir(filename)):
            raise TestAbortWithError(
                "Missing mandatory file: {}".format(filename)
            )

    def check_file_list(self, what, file_list, can_be_empty=True):
        """Raise a SetupError if `file_list` is not a list of existing files.

        Also raise an error if it is an empty list while `can_be_empty` is
        False.
        """
        # First check we have a list of strings
        if (
            not isinstance(file_list, list)
            or (not can_be_empty and len(file_list) == 0)
            or not all(isinstance(fn, str) for fn in file_list)
        ):
            empty_msg = "non-empty "
            raise TestAbortWithError(
                "{} must be a {}list of strings".format(what, empty_msg)
            )

        # Then check that these are existing files
        for filename in file_list:
            self.check_file(filename)

    def add_path(self, env, env_var, path):
        """
        Add a path to some environment variable.

        :param dict[str, str] env: Environment to modify.
        :param str env_var: Name of the environment variable to define/extend.
        :param str path: Path to prepend.
        """
        assert isinstance(env_var, str)
        assert isinstance(path, str)
        path_list = env.get(env_var, "")
        assert isinstance(path_list, str)
        if path_list:
            path_list = "{}{}{}".format(path, os.path.pathsep, path_list)
        else:
            path_list = path

        env[env_var] = path_list

    #
    # Run helpers
    #

    def run_and_check(
        self,
        argv,
        env=None,
        for_coverage=False,
        memcheck=False,
        valgrind_suppressions=None,
        analyze_output=True,
    ):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        In case of failure, the test output is appended to the actual output
        and a TestError is raised.

        :param list[str] argv: List of arguments to pass to the subprocess.
        :param None|dict[str, str] env: If provided, environment variables to
            pass to the subprocess.
        :param bool for_coverage: If true and if coverage is enabled, produce a
            trace file.
        :param bool memcheck: If true and if Valgrind runs are requested, run
            this process under Valgrind. If there are memory issues, they be
            reported on the testcase output and the process will return
            non-zero.
        :param suppressions: List of names for suppression files to pass to
            Valgrind (see drivers.valgrind.suppression_file).
        :param bool analyze_output: See
            e3.testsuite.driver.classic.ClassicTestDriver.shell.
        """
        if for_coverage and self.coverage_enabled:
            trace_file = self.coverage_file("trace")
            argv = ["gnatcov", "run", "-o", trace_file] + argv

        if memcheck and self.valgrind_enabled:
            argv = valgrind_cmd(argv, suppressions=valgrind_suppressions)

        self.shell(argv, env=env, analyze_output=analyze_output)

    @property
    def memcheck_for_lkt(self) -> bool:
        """
        Return whether Lkt programs can be run under Valgrind.

        We always build Langkit for 64-bit platforms, so when we test it to
        target 32-bit, we cannot use Valgrind (32-bit) to memcheck Langkit
        binaries (64-bit).
        """
        return self.env.build.cpu.bits == 64

    def create_project_file(self, project_file, mains):
        """
        Create a project file for the given main source files.

        The project file is created in the working directory. It gathers the
        Ada source files in the working directory.

        :param str project_file: Project file name to create.
        :param list[str] mains: List of main source files.
        """

        def format_string_list(strings):
            return ", ".join('"{}"'.format(s) for s in strings)

        cargs = ["-O0", "-g", "-gnata", "-gnatwae"]
        if self.coverage_enabled:
            cargs += ["-fdump-scos", "-fpreserve-control-flow"]

        with open(self.working_dir(project_file), "w") as f:
            f.write(
                """
            with "langkit_support";

            project P is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use "obj";
                for Main use ({mains});

                package Compiler is
                    for Default_Switches ("Ada") use ({cargs});
                end Compiler;
            end P;
            """.format(
                    mains=", ".join('"{}"'.format(m) for m in mains),
                    cargs=format_string_list(cargs),
                )
            )

    def gprbuild(self, project_file):
        """
        Run GPRbuild on the given project file.

        :param str project_file: Project file name.
        """
        argv = [
            "gprbuild",
            f"-P{project_file}",
            "-p",
            f"-j{self.env.inner_jobs}",
        ]
        if self.coverage_enabled:
            argv.append("--subdirs=gnatcov")
        self.run_and_check(argv, analyze_output=False)

    def program_path(self, main_source_file):
        """
        Return the path to the program corresponding to the given main file.

        :param str main_source_file: File name for the main source file from
            which the program has been built.
        :rtype: str
        """
        assert main_source_file.endswith(".adb")
        program_name = main_source_file[:-4]
        return (
            self.working_dir("obj", "gnatcov", program_name)
            if self.coverage_enabled
            else self.working_dir("obj", program_name)
        )
