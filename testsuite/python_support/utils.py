from __future__ import annotations

import dataclasses
import os
import os.path as P
import shlex
import shutil
import subprocess
import sys

from langkit.compile_context import CompileCtx
import langkit.config as C
from langkit.diagnostics import DiagnosticError
from langkit.libmanage import ManageScript
from langkit.utils import PluginLoader

from drivers.valgrind import valgrind_cmd


python_support_dir = P.dirname(P.abspath(__file__))
c_support_dir = P.join(python_support_dir, "..", "c_support")


# We don't want to be forced to provide dummy docs for nodes and public
# properties in testcases.
default_warnings = {
    "undocumented-nodes": False,
    "undocumented-public-properties": False,
}

base_config = {
    "lkt_spec": {
        "entry_point": "test.lkt",
        "source_dirs": [python_support_dir],
    },
    "library": {
        "language_name": "Foo",
        "short_name": "foo",
    },
    "warnings": default_warnings,
}

project_template = """
with "libfoolang";

project Gen is
    for Languages use ({languages});
    for Source_Dirs use ({source_dirs});
    for Object_Dir use "obj";
    for Main use ({main_sources});

    package Compiler is
        for Default_Switches ("Ada") use
          ("-g", "-O0", "-gnata", "-gnatwae", "-gnatyg");
        for Default_Switches ("C") use
          ("-g", "-O0", "-Wall", "-W", "-Werror", "-pedantic");
    end Compiler;

    package Binder is
        for Switches ("Ada") use ("-Es");
    end Binder;
end Gen;
"""


def derive_config(base_config, overridings):

    def error(context, exp_type, act_value):
        raise ValueError(
            f"{context}: {exp_type} expected, got {type(act_value).__name__}"
        )

    def recurse(context, base, overriding):
        if isinstance(base, dict):
            if not isinstance(overriding, dict):
                error(context, "dict", overriding)
            result = dict(base)
            for name, value in overriding.items():
                try:
                    base_value = base[name]
                except KeyError:
                    result[name] = value
                else:
                    result[name] = recurse(
                        f"{context}.{name}", base_value, value
                    )
            return result
        else:
            return overriding

    return (
        base_config
        if overridings is None
        else recurse("", base_config, overridings)
    )


@dataclasses.dataclass
class Main:
    encoding: str
    """
    Expected encoding for the main output.
    """

    source_file: str
    """
    Basename of the main source file.
    """

    args: list[str] = dataclasses.field(default_factory=list)
    """
    Arguments to pass to this main when running it.
    """

    @property
    def label(self) -> str:
        """
        Return a representation of this main that is suitable to include in
        test baselines.
        """
        return " ".join([self.source_file] + self.args)

    @property
    def unit_name(self) -> str:
        """
        Return the name of the main source file without extension.
        """
        return os.path.splitext(self.source_file)[0]

    @classmethod
    def parse(cls, value: dict | str) -> Main:
        """
        Create a Main instance from a shell-encoded list of arguments.
        """
        if isinstance(value, str):
            encoding = "utf-8"
            argv_str = value
        else:
            encoding = value["encoding"]
            argv_str = value["argv"]
        argv = shlex.split(argv_str)
        return cls(encoding, argv[0], argv[1:])


valgrind_enabled = bool(os.environ.get("VALGRIND_ENABLED"))
jobs = int(os.environ.get("LANGKIT_JOBS", "1"))


# Determine where to find the root directory for Langkit sources
langkit_root = os.environ.get("LANGKIT_ROOT_DIR")
if not langkit_root:
    test_dir = P.dirname(P.abspath(__file__))
    testsuite_dir = P.dirname(test_dir)
    langkit_root = P.dirname(testsuite_dir)


def prepare_context(config: C.CompilationConfig):
    """
    Create a compile context and prepare the build directory for code
    generation.

    :param config: Configuration for the language spec to compile.
    """

    # Have a clean build directory
    if P.exists("build"):
        shutil.rmtree("build")
    os.mkdir("build")

    return CompileCtx(
        config=config,
        plugin_loader=PluginLoader(config.library.root_directory),
    )


def emit_and_print_errors(
    config: dict | None = None,
    lkt_file: str | None = None,
):
    """
    Compile and emit code the given set of arguments. Return the compile
    context if this was successful, None otherwise.

    See ``prepare_context`` arguments.
    """
    actual_base_config = dict(base_config)
    actual_base_config["lkt_spec"]["entry_point"] = lkt_file

    actual_config = C.CompilationConfig.deserialize(
        "test.yaml:config", derive_config(actual_base_config, config)
    )

    try:
        ctx = prepare_context(actual_config)
        ctx.create_all_passes()
        ctx.emit()
        # ... and tell about how it went
    except DiagnosticError:
        # If there is a diagnostic error, don't say anything, the diagnostics
        # are enough.
        return None
    else:
        print("Code generation was successful")
        return ctx


def build_and_run(
    config: dict | None,
    py_script: str | None = None,
    py_args: list[str] | None = None,
    gpr_mains: list[Main] | None = None,
    ocaml_main: Main | None = None,
    java_main: Main | None = None,
    ni_main: Main | None = None,
) -> None:
    """
    Compile and emit code for a Lkt language spec and build the generated
    library. Then, execute the provided scripts/programs, if any.

    An exception is raised if any step fails (the script must return code 0).

    :param config: Overridings on top of "base_config" for the JSON-like form
        of the compilation configuration for this language. Just use the base
        config if None.
    :param py_script: If not None, name of the Python script to run with the
        built library available.
    :param python_args: Arguments to pass to the Python interpreter when
        running a Python script.
    :param gpr_mains: If not None, list of name of mains (Ada and/or C) for the
        generated GPR file, to build and run with the generated library. Each
        main can be either a Main instance or a string (for the main source
        file basename, the main is run without arguments).
    :param ocaml_main: If not None, name of the OCaml source file to build and
        run with the built library available.
    :param java_main: If not None, name of the Java main sourec file to build
        and run with the Langkit Java lib through JNI.
    :param ni_main: If not None, name of the Java main sourec file to build
        and run with the Langkit Java lib through Native Image.
    """
    # All tests are expected to write their output encoded with UTF-8. This is
    # the default on Unix systems, but not on Windows: reconfigure stdout
    # accordingly.
    sys.stdout.reconfigure(encoding="utf-8")

    class Manage(ManageScript):
        def __init__(self, config):
            self._cached_config = config
            super().__init__()

        def create_config(self, args):
            return self._cached_config

    build_mode = "dev"

    maven_exec = os.environ.get("MAVEN_EXECUTABLE")
    maven_repo = os.environ.get("MAVEN_LOCAL_REPO")

    config = C.CompilationConfig.deserialize(
        "test.yaml:config", derive_config(base_config, config)
    )
    m = Manage(config)

    # First build the library. Forward all test.py's arguments to the libmanage
    # call so that manual testcase runs can pass "-g", for instance.
    argv = (
        ["make"]
        + sys.argv[1:]
        + ["-vnone", f"-j{jobs}", "--full-error-traces"]
    )

    # If there is a Java main, enable the Java bindings building
    if java_main is not None or ni_main is not None:
        argv.append("--enable-java")
        if maven_exec:
            argv.append("--maven-executable")
            argv.append(maven_exec)
        if maven_repo:
            argv.append("--maven-local-repo")
            argv.append(maven_repo)
        if ni_main is not None and os.name == "nt":
            argv.append("--generate-msvc-lib")

    argv.append("--build-mode={}".format(build_mode))

    # No testcase uses the generated mains, so save time: never build them
    argv.append("--disable-all-mains")

    return_code = m.run_no_exit(argv)

    # Flush stdout and stderr, so that diagnostics appear deterministically
    # before the script/program output.
    sys.stdout.flush()
    sys.stderr.flush()

    if return_code != 0:
        raise DiagnosticError()

    # Write a "setenv" script to make developper investigation convenient
    with open("setenv.sh", "w") as f:
        m.write_printenv(f)

    env = m.derived_env()

    def run(*argv, **kwargs):
        subp_env = kwargs.pop("env", env)
        valgrind = kwargs.pop("valgrind", False)
        suppressions = kwargs.pop("valgrind_suppressions", [])
        encoding = kwargs.pop("encoding", "utf-8")
        forward_output = kwargs.pop("forward_output", True)
        assert not kwargs

        if valgrind_enabled and valgrind:
            argv = valgrind_cmd(list(argv), suppressions)

        p = subprocess.run(
            argv,
            env=subp_env,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            encoding=encoding,
        )
        if forward_output:
            sys.stdout.write(p.stdout)
            sys.stdout.flush()
        p.check_returncode()

    if py_script is not None:
        # Run the Python script.
        #
        # Note that in order to use the generated library, we have to use the
        # special Python interpreter the testsuite provides us. See the
        # corresponding code in testsuite/drivers/python_driver.py.
        args = [os.environ["PYTHON_INTERPRETER"]]
        if py_args:
            args.extend(py_args)

        # Also note that since Python 3.8, we need special PATH processing for
        # DLLs: see the path_wrapper.py script.
        args.append(P.join(python_support_dir, "path_wrapper.py"))

        args.append(py_script)
        run(*args)

    if gpr_mains:
        # Canonicalize mains to Main instances
        gpr_mains = [(Main(m) if isinstance(m, str) else m) for m in gpr_mains]

        source_dirs = [".", c_support_dir]
        main_source_files = sorted(m.source_file for m in gpr_mains)

        # Detect languages based on the source files present in the test
        # directory.
        langs = set()
        for f in os.listdir("."):
            if any(f.endswith(ext) for ext in [".c", ".h"]):
                langs.add("C")
            if any(f.endswith(ext) for ext in [".adb", ".ads"]):
                langs.add("Ada")

        # Generate a project file to build the given mains. Do a static build
        # (the default) to improve the debugging experience.
        with open("gen.gpr", "w") as f:

            def fmt_str_list(strings: list[str]) -> str:
                return ", ".join(f'"{s}"' for s in strings)

            f.write(
                project_template.format(
                    languages=fmt_str_list(langs),
                    source_dirs=fmt_str_list(source_dirs),
                    main_sources=fmt_str_list(main_source_files),
                )
            )
        run("gprbuild", "-Pgen", "-q", "-p")

        # Now run all mains. If there are more than one main to run, print a
        # heading before each one.
        for i, main in enumerate(gpr_mains):
            if i > 0:
                print("")
            if len(gpr_mains) > 1:
                print(f"== {main.label} ==")
            sys.stdout.flush()
            run(
                P.join("obj", main.unit_name),
                *main.args,
                valgrind=True,
                valgrind_suppressions=["gnat"],
                encoding=main.encoding,
            )

    if ocaml_main is not None:
        # Set up a Dune project
        with open("dune", "w") as f:
            f.write(
                """
                (executable
                  (name {})
                  (flags (-w -9))
                  (libraries {}))
            """.format(
                    ocaml_main.unit_name, m.context.c_api_settings.lib_name
                )
            )
        with open("dune-project", "w") as f:
            f.write("(lang dune 1.6)")

        # Build the ocaml executable
        run(
            "dune",
            "build",
            "--display",
            "quiet",
            "--root",
            ".",
            "./{}.exe".format(ocaml_main.unit_name),
        )

        # Run the ocaml executable
        run(
            "./_build/default/{}.exe".format(ocaml_main.unit_name),
            *ocaml_main.args,
            valgrind=True,
            valgrind_suppressions=["ocaml"],
            encoding=ocaml_main.encoding,
        )

    if java_main is not None:
        java_exec = P.realpath(P.join(env["JAVA_HOME"], "bin", "java"))
        cmd = [
            java_exec,
            "-Dfile.encoding=UTF-8",
            # Enable the Java application to load and use native libraries
            "--enable-native-access=ALL-UNNAMED",
            f"-Djava.library.path={env['LD_LIBRARY_PATH']}",
        ]
        cmd += [java_main.source_file] + java_main.args
        run(*cmd, encoding=java_main.encoding)

    if ni_main is not None:
        # Compile the Java tests
        javac_exec = P.realpath(P.join(env["JAVA_HOME"], "bin", "javac"))
        run(javac_exec, "-encoding", "utf8", ni_main.source_file)

        # Run native-image to compile the tests.  Building Java bindings does
        # not go through GPRbuild, so we must explicitly give access to the
        # generated C header.
        java_env = m.derived_env()
        ni_exec = P.realpath(
            P.join(
                os.environ["GRAAL_HOME"],
                "bin",
                ("native-image.cmd" if os.name == "nt" else "native-image"),
            )
        )
        class_path = os.path.pathsep.join(
            [
                P.realpath("."),
                m.dirs.build_dir(
                    "java", "target", f"{m.lib_name.lower()}.jar"
                ),
                m.dirs.build_dir(
                    "java", "target", "lib", "langkit_support.jar"
                ),
                m.dirs.build_dir("java", "target", "lib", "truffle-api.jar"),
                m.dirs.build_dir("java", "target", "lib", "polyglot.jar"),
            ]
        )
        os_specific_options = []
        if os.name != "nt":
            # Provide GCC options to retrieve the currently tested library
            lib_path = m.dirs.build_lib_dir("relocatable", "dev")
            os_specific_options.extend(
                [
                    f"--native-compiler-options=-I{m.dirs.build_dir("src")}",
                    f"--native-compiler-options=-L{lib_path}",
                ]
            )

            # In order to compile with native-image, we need to provide the
            # path to "zlib.so".
            for dir in os.environ.get("LIBRARY_PATH", "").split(os.pathsep):
                if os.path.isfile(os.path.join(dir, "libz.so")):
                    os_specific_options.append(
                        f"--native-compiler-options=-L{dir}",
                    )
                    break
        else:
            # Ensure the compiler isn't emitting warnings about CPU features
            os_specific_options.append("-march=native")
        run(
            ni_exec,
            "-cp",
            class_path,
            "--no-fallback",
            "-Ob",
            "--silent",
            "--parallelism=2",
            "-H:+UnlockExperimentalVMOptions",
            "-H:-StrictQueryCodeCompilation",
            *os_specific_options,
            os.path.splitext(ni_main.source_file)[0],
            "main",
            env=java_env,
            encoding=ni_main.encoding,
            forward_output=False,
        )

        # Run the newly created main
        run(P.realpath("main"), *ni_main.args)


def indent(text: str, prefix: str = "  ") -> str:
    """
    Indent all lines in `text` with the given prefix.

    :param text: Text to indent.
    :param prefix: Indentation string.
    """
    return "\n".join(prefix + line for line in text.splitlines())
