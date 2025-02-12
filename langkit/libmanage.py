from __future__ import annotations

import abc
import argparse
import glob
import json
import os
from os import path
import pdb
import re
import shlex
import shutil
import subprocess
import sys
import textwrap
import traceback
from typing import (
    Any, Callable, Sequence, TYPE_CHECKING, Text, TextIO, Type, cast
)

from e3.fs import sync_tree
import yaml

from langkit.compile_context import Verbosity
import langkit.config as C
from langkit.diagnostics import (
    DiagnosticError, DiagnosticStyle, Diagnostics, WarningSet,
)
from langkit.packaging import WheelPackager
from langkit.utils import (
    BuildMode,
    Colors,
    LibraryType,
    Log,
    PluginLoader,
    add_to_path,
    col,
    format_printenv,
    get_cpu_count,
    parse_choice,
    parse_cmdline_args,
    parse_list_of_choices,
    printcol,
)
import langkit.windows


if TYPE_CHECKING:
    from enum import Enum
    from langkit.compile_context import CompileCtx
    from types import TracebackType


class Directories:
    """
    Helper class used to get various path in source/build/install trees.
    """

    def __init__(self,
                 lang_source_dir: str,
                 build_dir: str | None = None,
                 install_dir: str | None = None):

        self.root_lang_source_dir = lang_source_dir
        self.root_build_dir = build_dir
        self.root_install_dir = install_dir

    def set_build_dir(self, build_dir: str) -> None:
        self.root_build_dir = build_dir

    def set_install_dir(self, install_dir: str) -> None:
        self.root_install_dir = path.abspath(install_dir)

    def lang_source_dir(self, *args: str) -> str:
        return path.join(self.root_lang_source_dir, *args)

    def build_dir(self, *args: str) -> str:
        assert self.root_build_dir is not None
        return self.lang_source_dir(
            self.root_lang_source_dir, self.root_build_dir, *args
        )

    def build_lib_dir(self, *args: str) -> str:
        return self.build_dir("lib", *args)

    def install_dir(self, *args: str) -> str:
        assert self.root_install_dir is not None
        return path.join(self.root_install_dir, *args)


class EnableWarningAction(argparse.Action):
    def __call__(self,
                 parser: argparse.ArgumentParser,
                 namespace: argparse.Namespace,
                 values: Text | Sequence[Any] | None,
                 option_string: Text | None = None) -> None:
        namespace.enabled_warnings.enable(values)


class DisableWarningAction(argparse.Action):
    def __call__(self,
                 parser: argparse.ArgumentParser,
                 namespace: argparse.Namespace,
                 values: Text | Sequence[Any] | None,
                 option_string: Text | None = None) -> None:

        namespace.enabled_warnings.disable(values)


class ManageScript(abc.ABC):
    build_modes: list[BuildMode]
    """
    Build modes to build.
    """

    ENABLE_BUILD_WARNINGS_DEFAULT = False
    """
    Whether warnings to build the generated library are enabled by default.
    """

    ENABLE_JAVA_DEFAULT = False
    """
    Whether to build Java bindings by default.
    """

    enable_build_warnings: bool
    """
    Whether to enable build warnings.
    """

    context: CompileCtx
    """
    Langkit compilation context. The create_context method will create the
    context and set it only right before executing commands.
    """

    # This will be set in the "run" method, when we have parsed arguments
    # from the command line.
    verbosity: Verbosity

    def __init__(self) -> None:
        self.dirs: Directories
        self.context: C.CompilationConfig

        ########################
        # Main argument parser #
        ########################

        self.args_parser = args_parser = argparse.ArgumentParser(
            description='General manager to handle actions relative to'
                        ' building/testing your language.'
        )
        self.subparsers = args_parser.add_subparsers()

        ############
        # Generate #
        ############

        self.generate_parser = generate_parser = self.add_subcommand(
            self.do_generate
        )
        self.add_generate_args(generate_parser)

        #########
        # Build #
        #########

        self.build_parser = build_parser = self.add_subcommand(self.do_build)
        self.add_build_args(build_parser)

        ########
        # Make #
        ########

        self.make_parser = make_parser = self.add_subcommand(self.do_make)
        self.add_generate_args(make_parser)
        self.add_build_args(make_parser)

        ########################
        # List optional passes #
        ########################

        self.list_optional_passes_parser = self.add_subcommand(
            self.do_list_optional_passes
        )
        self.add_generate_args(self.list_optional_passes_parser)

        ###########
        # Install #
        ###########

        self.install_parser = install_parser = self.add_subcommand(
            self.do_install
        )
        self.add_build_mode_arg(install_parser)
        install_parser.add_argument(
            '--force', '-f', action='store_true',
            help='Force installation, overwrite files.'
        )
        install_parser.add_argument(
            '--disable-all-mains', action='store_true',
            help='Do not install main program.'
        )
        install_parser.add_argument(
            'install-dir',
            help='Installation directory.'
        )

        ############
        # Printenv #
        ############

        self.printenv_parser = self.add_subcommand(self.do_printenv)
        self.add_build_mode_arg(self.printenv_parser)
        self.printenv_parser.add_argument(
            '--json', '-J', action='store_true',
            help='Output necessary env keys to JSON.'
        )

        #######
        # Run #
        #######

        self.run_parser = run_parser = self.add_subcommand(
            self.do_run, accept_unknown_args=True
        )
        self.add_build_args(run_parser)

        #######################
        # Create Python wheel #
        #######################

        self.create_wheel_parser = self.add_subcommand(self.do_create_wheel)
        WheelPackager.add_platform_options(self.create_wheel_parser)
        self.create_wheel_parser.add_argument(
            '--with-python',
            help='Python intererpter to use in order to build the wheel. If'
                 ' not provided, use the current one.'
        )
        self.create_wheel_parser.add_argument(
            '--python-tag',
            help="Forwarded to setup.py bdist_wheel."
        )
        self.create_wheel_parser.add_argument(
            '--plat-name',
            help="Forwarded to setup.py bdist_wheel."
        )
        self.create_wheel_parser.add_argument(
            'wheel-dir',
            help='Destination directory for the wheel.'
        )
        self.create_wheel_parser.add_argument(
            'build-dir',
            help='Temporary directory to use in order to build the wheel.'
        )
        self.create_wheel_parser.add_argument(
            'dyn-deps-dir',
            help='Directory that contains all the dynamic libraries to ship in'
                 ' the wheel (i.e. dependencies).'
        )
        self.create_wheel_parser.add_argument(
            'install-dir',
            help='Directory in which the library is installed.'
        )

        self.add_extra_subcommands()

    def add_extra_subcommands(self) -> None:
        """
        Subclasses may override this to create new subcommands for the manage
        script, using the "add_subcommand" method.
        """
        pass

    def add_subcommand(
        self,
        callback: (
            Callable[[argparse.Namespace], None]
            | Callable[[argparse.Namespace, list[str]], None]
        ),
        *,
        accept_unknown_args: bool = False,
    ) -> argparse.ArgumentParser:
        """
        Create a subcommand for the manage script.

        :param callback: Function to call when executing the subcommand.

            Its name is assumed to be "do_X" with "X" being the name of the
            command, with underscores instead of dashes. For instance, if the
            function name is "do_foo_bar", this creates a "foo-bar" subcommand.

            The first of its docstring is used as the help message for the
            subcommand.

        :param accept_unknown_args: Whether the subcommand parser accepts
            unknown arguments. If this is enabled, unknown arguments are passed
            as a second argument to ``callback``.

        :return: The argument parser for this new subcommand's arguments.
        """
        assert callback.__name__.startswith("do_")
        name = callback.__name__[3:].replace("_", "-")

        # Take the first paragraph of callback's docstring
        assert callback.__doc__
        help_message = callback.__doc__.split("\n\n")[0]

        # Remove indentation and line breaks from it
        help_message = " ".join(
            line.strip()
            for line in help_message.split("\n")
            if line.strip()
        )

        # Create the arguments parser for the subcommand and add common
        # arguments.
        parser = self.subparsers.add_parser(name, help=help_message)
        self.add_common_args(parser)

        # Wrapper function to invoke the callback with the right arguments,
        # depending on whether it accepts unknown arguments.

        def wrapper(parsed_args: argparse.Namespace,
                    unknown_args: list[str]) -> None:
            if accept_unknown_args:
                cb_full = cast(
                    Callable[[argparse.Namespace, list[str]], None],
                    callback,
                )
                cb_full(parsed_args, unknown_args)
            elif unknown_args:
                print(
                    f"{sys.argv[0]}: error: unrecognized arguments:"
                    f" {' '.join(unknown_args)}"
                )
                sys.exit(1)
            else:
                cb_single = cast(
                    Callable[[argparse.Namespace], None],
                    callback
                )
                cb_single(parsed_args)

        parser.set_defaults(func=wrapper)
        return parser

    @staticmethod
    def add_common_args(subparser: argparse.ArgumentParser) -> None:
        """
        Add command-line arguments common to all subcommands to ``subparser``.
        """
        C.add_args(subparser)

        LibraryType.add_argument(subparser)

        subparser.add_argument(
            '--verbosity', '-v', nargs='?',
            type=Verbosity,
            choices=Verbosity.choices(),
            default=Verbosity('info'),
            const=Verbosity('debug'),
            help='Verbosity level'
        )
        subparser.add_argument(
            '--full-error-traces', '-E', action='store_true', default=False,
            help='Always show full error traces, whatever the verbosity level'
                 ' (default: disabled).'
        )
        subparser.add_argument(
            '--trace', '-t', action='append', default=[],
            help='Activate given debug trace.'
        )

        # Don't enable this by default so that errors will not make automated
        # tasks hang.
        subparser.add_argument(
            '-g', '--debug', action='store_true',
            help='In case of internal error or diagnostic error, run a'
                 ' post-mortem PDB session.'
        )
        subparser.add_argument(
            '--profile', action='store_true',
            help='Run cProfile and langkit, and generate a data file'
                 ' "langkit.prof".'
        )
        subparser.add_argument(
            '--diagnostic-style', '-D', type=DiagnosticStyle,
            default=DiagnosticStyle.default,
            help='Style for error messages.'
        )

    @classmethod
    def add_generate_args(cls, subparser: argparse.ArgumentParser) -> None:
        """
        Add arguments to tune code generation to "subparser".
        """
        subparser.add_argument(
            '--check-only', action='store_true',
            help="Only check the input for errors, don't generate the code."
        )
        subparser.add_argument(
            '--list-warnings', action='store_true',
            help='Display the list of available warnings.'
        )

    def add_build_mode_arg(self, subparser: argparse.ArgumentParser) -> None:

        def parse_choice_into_list(s: str) -> list[Enum]:
            fn = parse_choice(BuildMode)
            return [fn(s)]

        # We need to set the default for both build mode arguments, because
        # else they might conflict and the default value might be none.
        default = [BuildMode.dev]

        subparser.add_argument(
            '--build-mode',
            type=parse_choice_into_list,
            dest="build_modes",
            default=default,
            help='Select a build mode. This is a shortcut for --build-modes'
            ' with a single argument'
        )
        subparser.add_argument(
            '--build-modes',
            type=parse_list_of_choices(BuildMode),
            default=default,
            help='Select a list of build modes'
        )

    def add_build_args(self, subparser: argparse.ArgumentParser) -> None:
        """
        Add arguments to tune code compilation to "subparser".
        """
        subparser.add_argument(
            '--jobs', '-j', type=int, default=get_cpu_count(),
            help='Number of jobs to spawn in parallel for calls to builders'
                 ' (default: your number of cpu).'
        )
        subparser.add_argument(
            '--parallel-builds', type=int, default=1,
            help='Number of builds to run in parallel. Default is 1. Be'
            ' careful because this is in addition to the number of jobs.'
        )

        self.add_build_mode_arg(subparser)
        subparser.add_argument(
            '--enable-build-warnings',
            action='store_true', dest='enable_build_warnings',
            default=self.ENABLE_BUILD_WARNINGS_DEFAULT,
            help='Enable warnings to build the generated library.'
        )
        subparser.add_argument(
            '--disable-build-warnings',
            action='store_false', dest='enable_build_warnings',
            default=self.ENABLE_BUILD_WARNINGS_DEFAULT,
            help='Disable warnings to build the generated library.'
        )
        subparser.add_argument(
            '--gargs', action='append',
            help='Options appended to GPRbuild invocations.'
        )
        subparser.add_argument(
            '--disable-all-mains', action='store_true',
            help='Do not build any main program.'
        )
        subparser.add_argument(
            '--generate-msvc-lib', action='store_true', default=False,
            help='Generate a .lib file from the library DLL that MSVC'
                 ' toolchains need in order to link against the DLL. This is'
                 ' supported only on Windows, and requires the Visual Studio'
                 ' Build Tools in the environment.'
        )
        subparser.add_argument(
            '--with-rpath', action='store_true', dest='with_rpath',
            default=True,
            help='Build libraries with run path options (by default)'
        )
        subparser.add_argument(
            '--without-rpath', action='store_false', dest='with_rpath',
            help='Do not build libraries with run path options'
        )

        subparser.add_argument(
            '--enable-java', action='store_true', dest='enable_java',
            default=self.ENABLE_JAVA_DEFAULT,
            help='Enable the Java bindings building/installation.'
        )
        subparser.add_argument(
            '--disable-java', action='store_false', dest='enable_java',
            help='Disable the Java bindings building/installation.'
        )
        subparser.add_argument(
            '--maven-local-repo',
            help='Specify the Maven repository to use, default one is the'
                 " user's repository (~/.m2)."
        )
        subparser.add_argument(
            '--maven-executable',
            help='Specify the Maven executable to use. The default one is'
                 ' "mvn".'
        )

    def load_yaml_config(self, filename: str) -> C.CompilationConfig:
        """
        Common implementation fr ``create_config`` when the configuration must
        be loaded from a ``langkit.yaml`` file.

        :param filename: YAML file to load.
        """
        with open(filename) as f:
            json = yaml.safe_load(f)
        return C.CompilationConfig.from_json(
            context=os.path.basename(filename),
            json=json,
            base_directory=os.path.dirname(filename),
        )

    @abc.abstractmethod
    def create_config(self, args: argparse.Namespace) -> C.CompilationConfig:
        """
        Return the compilation configuration to use for this language spec.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        ...

    def create_context(
        self,
        config: C.CompilationConfig,
        verbosity: Verbosity,
    ) -> CompileCtx:
        """
        Return a Langkit context for a language spec compilation configuration.
        """
        from langkit.compile_context import CompileCtx

        return CompileCtx(
            config=config,
            plugin_loader=PluginLoader(config.library.root_directory),
            verbosity=verbosity,
        )

    @property
    def main_source_dirs(self) -> set[str]:
        """
        Return a potentially empty set of source directories to use in the
        project file for mains. Source directories must be either absolute or
        relative to the language directory.
        """
        ext_dir = self.context.extensions_dir
        if ext_dir is not None:
            mains_dir = os.path.join(ext_dir, "mains")
            if os.path.isdir(mains_dir):
                return {mains_dir}

        return set()

    @property
    def extra_main_programs(self) -> set[str]:
        """
        List of names for programs to build on top the generated library in
        addition to the built in Langkit ones.

        Subclasses should override this to add more main programs.
        """
        return set()

    @property
    def lib_name(self) -> str:
        return self.context.ada_api_settings.lib_name

    def run(self, argv: list[str] | None = None) -> None:
        return_code = self.run_no_exit(argv)
        if return_code != 0:
            sys.exit(return_code)

    def run_no_exit(self, argv: list[str] | None = None) -> int:
        parsed_args, unknown_args = self.args_parser.parse_known_args(argv)
        if getattr(parsed_args, "func", None) is None:
            print(col("Please provide a subcommand to run", Colors.FAIL))
            self.args_parser.print_help()
            return 1

        for trace in parsed_args.trace:
            print("Trace {} is activated".format(trace))
            Log.enable(trace)

        Diagnostics.set_style(parsed_args.diagnostic_style)

        if parsed_args.profile:
            import cProfile
            import pstats

            pr = cProfile.Profile()
            pr.enable()

        # Set the verbosity
        self.verbosity = parsed_args.verbosity

        self.enable_build_warnings = getattr(
            parsed_args, "enable_build_warnings", False
        )

        # If there is no build_modes (ie. we're not running a command that
        # requires it), we still need one to call gnatpp, so set it to a dummy
        # build mode.
        self.build_modes = getattr(parsed_args, 'build_modes', [])

        # If asked to, setup the exception hook as a last-chance handler to
        # invoke a debugger in case of uncaught exception.
        if parsed_args.debug:
            # Try to use IPython's debugger if it is available, otherwise
            # fallback to PDB.
            try:
                # noinspection PyPackageRequirements
                from IPython.core import ultratb
            except ImportError:

                def excepthook(typ: Type[BaseException],
                               value: BaseException,
                               tb: TracebackType | None) -> Any:
                    traceback.print_exception(typ, value, tb)
                    pdb.post_mortem(tb)

                sys.excepthook = excepthook
            else:
                sys.excepthook = ultratb.FormattedTB(
                    mode='Verbose', color_scheme='Linux', call_pdb=1
                )

        # noinspection PyBroadException
        try:
            # If the subcommand requires a context, create it now
            config = self.create_config(parsed_args)
            C.update_config_from_args(config, parsed_args)
            self.context = self.create_context(
                config, parsed_args.verbosity
            )

            # Setup directories. If there is a configuration, get the root
            # directory from it. Otherwise, consider that the directory in
            # which the ManageScript subclass was defined is the root
            # directory.
            self.dirs = Directories(
                lang_source_dir=self.context.config.library.root_directory
            )

            # Refine build/install directories from command line arguments
            self.dirs.set_build_dir(parsed_args.build_dir)
            install_dir = getattr(parsed_args, 'install-dir', None)
            if install_dir:
                self.dirs.set_install_dir(install_dir)

            if getattr(parsed_args, 'list_warnings', False):
                WarningSet.print_list()
                return 0
            parsed_args.func(parsed_args, unknown_args)
            return 0

        except DiagnosticError:
            if parsed_args.debug:
                raise
            if parsed_args.verbosity.debug or parsed_args.full_error_traces:
                traceback.print_exc()
            print(col('Errors, exiting', Colors.FAIL))
            return 1

        finally:
            if parsed_args.profile:
                pr.disable()
                ps = pstats.Stats(pr)
                ps.dump_stats('langkit.prof')

    def do_generate(self, args: argparse.Namespace) -> None:
        """
        Generate source code for the user language.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        self.context.create_all_passes(check_only=args.check_only)

        self.log_info(
            "Generating source for {}...".format(self.lib_name.lower()),
            Colors.HEADER
        )

        self.context.emit()

        if args.check_only:
            return

        self.log_info("Generation complete!", Colors.OKGREEN)

    def what_to_build(
        self,
        args: argparse.Namespace,
        is_library: bool
    ) -> list[list[tuple[BuildMode, LibraryType]]]:
        """
        Determine what kind of builds to perform.

        :param args: The arguments parsed from the command line invocation of
            manage.py.

        :param is_library: If true, build all modes (depending on modes enabled
            in `args`). Otherwise, use relocatable if allowed, static-pic
            otherwise and static otherwise.

        :return: A list of lists of build configurations to perform. Nested
            lists contain build configurations that must be built sequentially.
            The top level list contains builds that can be parallelized.
        """
        if is_library:
            return [[(b, l) for l in args.library_types]
                    for b in self.build_modes]
        else:
            # Program are built only once, so build them as relocatable if
            # allowed, otherwise as static-pic if allowed, otherwise as static.
            for l in (LibraryType.relocatable, LibraryType.static_pic,
                      LibraryType.static):
                if l in args.library_types:
                    lib_type = l
                    break
            return [[(b, lib_type) for b in self.build_modes]]

    def gpr_scenario_vars(
        self, library_type: str = 'relocatable', build_mode: str = 'dev'
    ) -> list[str]:
        """
        Return the project scenario variables to pass to GPRbuild.

        :param library_type: Library flavor to use. Must be "relocatable" or
            "static".
        """
        result = ['-XBUILD_MODE={}'.format(build_mode),
                  '-XLIBRARY_TYPE={}'.format(library_type),
                  '-XGPR_BUILD={}'.format(library_type),
                  '-XXMLADA_BUILD={}'.format(library_type)]

        if self.enable_build_warnings:
            result.append(
                '-X{}_WARNINGS=true'.format(self.lib_name.upper())
            )

        return result

    def gprbuild(self,
                 args: argparse.Namespace,
                 project_file: str,
                 is_library: bool,
                 mains: set[str] = set()) -> None:
        """
        Run GPRbuild on a project file.

        :param args: The arguments parsed from the command line invocation of
            manage.py.

        :param project_file: Path to the project file to pass to GPRbuild.

        :param is_library: See the "what_to_build" method.

        :param mains: If provided, list of main programs to build. By default,
            GPRbuild builds them all, so this arguments makes it possible to
            build only a subset of them.
        """
        base_argv = [
            'gprbuild', '-p', '-j{}'.format(args.jobs),
            '-P{}'.format(project_file),
        ]

        if not args.with_rpath:
            # Prevent GPRbuild from adding RPATH to links, as paths will not be
            # valid once generated libraries are installed.
            base_argv.append('-R')

        if args.verbosity == Verbosity('none'):
            base_argv.append('-q')
        elif args.verbosity == Verbosity('debug'):
            base_argv.append('-vl')

        gargs = parse_cmdline_args(getattr(args, 'gargs'))

        def run(build_mode: BuildMode, library_type: LibraryType) -> None:
            self.log_info(
                f"Building for config ({build_mode}, {library_type})",
                Colors.HEADER
            )
            # Workaround a GPRbuild bug (see SB18-035): Library types share the
            # same object directory, however GPRbuild uses a file in the object
            # directory (*.lexch) to know if the library must be rebuilt. Not
            # removing it will make it skip the "static" build after the
            # "static-pic" was built. So we remove it.
            #
            # TODO: We assume the object dir is always "obj" because it is in
            # the only case that matters (building the library), and in other
            # cases the globbing will return no files because the dir doesn't
            # exist. Ideally we shouldn't duplicate this information here and
            # in the project file.
            obj_dir = self.dirs.build_dir('obj', build_mode.value)
            lexch_pattern = os.path.join(os.path.dirname(project_file),
                                         obj_dir, '*.lexch')

            # Remove the "*.lexch" files
            files = glob.glob(lexch_pattern)
            for f in files:
                self.log_debug('Removing {}'.format(f), Colors.CYAN)
                os.remove(f)
            if not files:
                self.log_debug('No *.lexch file to remove from {}'
                               .format(lexch_pattern), Colors.CYAN)

            argv = list(base_argv)
            argv.extend(self.gpr_scenario_vars(
                library_type=library_type.value,
                build_mode=build_mode.value
            ))
            if mains:
                argv.extend('{}.adb'.format(main) for main in mains)
            if Diagnostics.style == DiagnosticStyle.gnu_full:
                argv.append('-gnatef')
            argv.extend(gargs)
            self.check_call('Build', argv)

        def build(configs: list[tuple[BuildMode, LibraryType]]) -> None:
            """
            Sequentially build a list of configs.
            """
            for c in configs:
                run(*c)

        from concurrent import futures
        with futures.ThreadPoolExecutor(
            max_workers=args.parallel_builds
        ) as executor:
            list(executor.map(lambda v: build(v),
                              self.what_to_build(args, is_library)))

    def gprinstall(self,
                   args: argparse.Namespace,
                   project_file: str,
                   is_library: bool,
                   build_mode: str) -> None:
        """
        Run GPRinstall on a project file.

        See gprbuild for arguments description.
        """
        assert project_file.endswith('.gpr')
        project_name = os.path.basename(project_file)[:-4].upper()

        base_argv = ['gprinstall', '-p',
                     '-P{}'.format(project_file),
                     '--prefix={}'.format(self.dirs.install_dir()),
                     '--build-var=LIBRARY_TYPE',
                     '--build-var={}_LIBRARY_TYPE'.format(project_name)]

        # If this is a library, install sources in an unique location: there is
        # no need to have one location per build mode as sources are going to
        # be exactly the same. If this is a program, no need to install
        # anything but the executable itself.
        if is_library:
            lib_name, _ = os.path.splitext(os.path.basename(project_file))
            base_argv.append('--sources-subdir={}'.format(os.path.join(
                'include', lib_name
            )))
        else:
            base_argv.append('--mode=usage')

        if args.force:
            base_argv.append('-f')

        if args.verbosity == Verbosity('none'):
            base_argv.append('-q')

        def run(library_type: str) -> None:
            argv = list(base_argv)
            argv.append('--build-name={}'.format(library_type))
            argv.extend(self.gpr_scenario_vars(library_type, build_mode))
            self.check_call('Install', argv)

        # Install the static libraries first, so that in the resulting project
        # files, "static" is the default library type.
        if LibraryType.static in args.library_types:
            run('static')
        if LibraryType.static_pic in args.library_types:
            run('static-pic')
        if LibraryType.relocatable in args.library_types:
            run('relocatable')

    def generate_lib_file(
        self,
        build_mode: BuildMode,
        verbosity: Verbosity,
    ) -> None:
        """
        Run tools to generate a .lib file from the language DLL.
        """
        # Create the windows directory if it doesn't exist
        if not path.isdir(self.dirs.build_lib_dir("windows")):
            os.makedirs(self.dirs.build_lib_dir("windows"))

        langkit.windows.generate_lib_file(
            dll_filename=self.dirs.build_lib_dir(
                "relocatable",
                build_mode.value,
                f"{self.lib_name.lower()}.dll"
            ),
            lib_filename=self.dirs.build_lib_dir(
                "windows",
                f"{self.context.config.library.language_name.lower}lang.lib"
            ),
            quiet=verbosity == Verbosity("none"),
        )

    def maven_command(self,
                      goals: list[str],
                      args: argparse.Namespace) -> None:
        """
        Run Maven for the given goals with the given args.

        :param goals: Ordered goals for Maven to execute.
        :param args: Arguments parsed from the invocation of manage.py.
            We use them for Maven configuration.
        """
        # Get the Maven executable
        maven_exec = args.maven_executable or 'mvn'

        # Compute the additional Maven arguments
        maven_args = []
        if args.maven_local_repo is not None:
            maven_args.append(f'-Dmaven.repo.local={args.maven_local_repo}')

        if not self.verbosity.debug:
            maven_args.append('-q')

        env = self.derived_env()

        # Pass the build mode to the Makefile. We do not support building Java
        # bindings with multiple modes in parallel, so just pick the first one.
        env[f"{self.context.lib_name.upper}_BUILD_MODE"] = (
            args.build_modes[0].value
        )

        # Call Maven on the Java bindings
        argv = [
            maven_exec,
            '-f',
            self.maven_project,
            *goals,
            *maven_args,
        ]
        self.check_call('Maven-Command', argv, env=env)

    @property
    def lib_project(self) -> str:
        """
        Path to the project file for the generated library.
        """
        return self.dirs.build_dir('{}.gpr'.format(self.lib_name.lower()))

    @property
    def mains_project(self) -> str:
        """
        Path to the project file for the mains.
        """
        return self.dirs.build_dir('mains.gpr')

    @property
    def maven_project(self) -> str:
        """
        Path to the Maven project file for the Java bindings.
        """
        return self.dirs.build_dir('java', 'pom.xml')

    def do_build(self, args: argparse.Namespace) -> None:
        """
        Build generated source code.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        # Build the generated library itself
        self.log_info("Building the generated source code", Colors.HEADER)

        self.gprbuild(args, self.lib_project, is_library=True)

        # Then build the main programs (unless asked to skip them)
        if not args.disable_all_mains:
            self.log_info("Building the main programs...", Colors.HEADER)
            self.gprbuild(args, self.mains_project, is_library=False)

        # On macOS systems, run the gprbuild workraound, necessary to get
        # correctly working rpath for libraries.
        #
        # Note: we must do this only after the mains project has been built, as
        # running gprbuild on the mains project may re-create the shared
        # library.
        if (
            LibraryType.relocatable in args.library_types
            and args.with_rpath
            and sys.platform == "darwin"
        ):
            for build_mode in self.build_modes:
                self.run_macos_workaround(
                    self.dirs.build_dir(
                        "lib", LibraryType.relocatable.value, build_mode.value
                    )
                )

        # If requested, generate the .lib file for MSVC
        if args.generate_msvc_lib:
            self.log_info("Generating the .lib file for MSVC", Colors.HEADER)

            # Verify the OS and the library
            if os.name != 'nt':
                self.log_info(
                    ".lib file generation is only available on Windows",
                    Colors.FAIL
                )
            elif LibraryType.relocatable not in args.library_types:
                self.log_info(
                    "You need to build a relocatable library to generate"
                    " the .lib file",
                    Colors.FAIL
                )
            else:
                self.generate_lib_file(self.build_modes[0], args.verbosity)

        # Build the Java bindings
        if args.enable_java:
            self.log_info("Building the Java bindings...", Colors.HEADER)

            # Verify that JAVA_HOME is set
            if not os.environ.get("JAVA_HOME"):
                self.log_info(
                    "Setting the JAVA_HOME environment variable to your "
                    "JDK installation is mandatory for the Java "
                    "bindings building",
                    Colors.FAIL
                )
            else:
                self.maven_command(['clean', 'package'], args)

        self.log_info("Build complete!", Colors.OKGREEN)

    def run_macos_workaround(self, dylib_dir: str) -> None:
        """
        On macOS, there are two issues with gprbuild:

        1. The linker arguments for rpath produce a leading space into paths:
           ``-Wl,-rpath, @executable_path/...``. This makes the dynamic library
           loader unable to use those rpaths at runtime.

        2. The linker uses ``@executable_path`` which applies in the context of
           an exectuable but not in a context where the library is loaded
           directly, which is precisely the case for the generated libraries,
           so that language bindings work when all shared libraries are put in
           the same directory. Instead, ``loader_path`` should be used.

        This function applies a workaround by removing the leading space from
        rpath entries, and replacing ``@executable_path`` with
        ``@loader_path``.

        :param dylib_dir: Name of the directory that contains the shared
            libraries (*.dylib) to fix.
        """
        path_line_re = re.compile(
            r"         path (.*) \(offset \d+\)\s*"
        )

        for filename in glob.glob(os.path.join(dylib_dir, "*.dylib")):
            # Log the full output of otool for debugging
            otool_args = ["otool", "-l", filename]
            if self.verbosity.debug:
                subprocess.run(otool_args, stdin=subprocess.DEVNULL)

            otool_output = subprocess.check_output(
                otool_args, stdin=subprocess.DEVNULL, encoding="utf-8"
            ).splitlines()

            # The output of otool that we want to match looks like the
            # following::
            #
            #   Load command 49
            #             cmd LC_RPATH
            #         cmdsize 42
            #            path  /some/lookup/path (offset 12)
            #                 ^ extra leading space here
            #   Load command 50
            #             cmd LC_RPATH
            #         cmdsize 42
            #            path @executable_path/../lib (offset 12)
            #                 ^ should be @loader_path
            #   Load command 51
            #             cmd LC_RPATH
            #         cmdsize 42
            #            path  @executable_path/../lib (offset 12)
            #                 ^ two problems to fix at the same time

            for i, line in enumerate(otool_output):
                if "LC_RPATH" in line:
                    path_line = otool_output[i + 2]
                    m = path_line_re.match(path_line)
                    assert m, f"unexpected path line: {path_line!r}"
                    original_path = m.group(1)

                    # First fix paths with a leading space
                    path = original_path.lstrip()

                    # Then replace @executable_path with @loader_path
                    path = path.replace("@executable_path", "@loader_path")

                    subprocess.check_call(
                        [
                            "install_name_tool",
                            "-rpath",
                            original_path,
                            path,
                            filename,
                        ],
                        stdin=subprocess.DEVNULL,
                    )

    def do_make(self, args: argparse.Namespace) -> None:
        """
        Generate and build in one command.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        self.do_generate(args)
        self.do_build(args)

    def do_install(self, args: argparse.Namespace) -> None:
        """
        Install programs and libraries.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        if len(self.build_modes) != 1:
            print("Exactly one build mode required")
            raise DiagnosticError
        build_mode = self.build_modes[0].value

        lib_name = self.lib_name.lower()
        lib_name_camel = lib_name.capitalize()

        self.gprinstall(
            args,
            self.lib_project,
            is_library=True,
            build_mode=build_mode,
        )

        # Install programs if they are all required
        if not args.disable_all_mains:
            self.gprinstall(
                args,
                self.mains_project,
                is_library=False,
                build_mode=build_mode,
            )

        # Install scripts into "bin"
        scripts = glob.glob(self.dirs.build_dir("scripts", "*"))
        install_dir = self.dirs.install_dir("bin")
        if scripts:
            if not os.path.exists(install_dir):
                os.mkdir(install_dir)
            for f in scripts:
                shutil.copy2(
                    f,
                    os.path.join(install_dir, os.path.basename(f))
                )

        # Install the GDB helpers setup script in "share"
        share_dir = self.dirs.install_dir("share", lib_name)
        if not path.isdir(share_dir):
            os.makedirs(share_dir)
        gdbinit_filename = self.dirs.build_dir("gdbinit.py")
        shutil.copyfile(
            gdbinit_filename,
            os.path.join(share_dir, os.path.basename(gdbinit_filename))
        )

        # Install the remaining miscellaneous files
        for fpath in [
            os.path.join('python', lib_name, '*.py'),
            os.path.join('python', lib_name, 'py.typed'),
            os.path.join('python', 'setup.py'),
            os.path.join('ocaml', lib_name + '.ml'),
            os.path.join('ocaml', lib_name + '.mli'),
            os.path.join('ocaml', 'dune'),
            os.path.join('ocaml', 'dune-project'),
            os.path.join('ocaml', lib_name + '.opam'),
            os.path.join('java', 'Makefile'),
            os.path.join('java', 'pom.xml'),
            os.path.join('java', 'reflect_config.json'),
            os.path.join('java', 'jni',
                         f'com_adacore_{lib_name}_{lib_name_camel}_NI_LIB.h'),
            os.path.join('java', 'jni', 'jni_impl.c'),
            os.path.join('java', 'src', 'main', 'java', 'com', 'adacore',
                         lib_name, f'{lib_name_camel}.java'),
            os.path.join('lib', 'windows', '*'),
        ]:
            install_path = os.path.dirname(self.dirs.install_dir(fpath))
            if not path.isdir(install_path):
                os.makedirs(install_path)

            for f in glob.glob(self.dirs.build_dir(fpath)):
                shutil.copyfile(
                    f, os.path.join(install_path, os.path.basename(f))
                )

        # Install extra files listed in the configuration
        for dest_dir, files in (
            self.context.config.library.extra_install_files.items()
        ):
            install_path = self.dirs.install_dir(dest_dir)
            if not os.path.isdir(install_path):
                os.makedirs(install_path)

            for f in files:
                sync_tree(
                    f,
                    os.path.join(install_path, os.path.basename(f)),
                    delete=False,
                )

    def do_printenv(self, args: argparse.Namespace) -> None:
        """
        Unless --json is passed, display Bourne shell commands that setup
        environment in order to make the generated library available.
        Otherwise, return a JSON document that describe this environment.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        if args.json:
            result: dict[str, str] = {}

            def add_json(name: str, path: str) -> None:
                try:
                    result[name] = '{}{}{}'.format(
                        result[name], os.path.pathsep, path
                    )
                except KeyError:
                    result[name] = path

            self.setup_environment(add_json)
            print(json.dumps(result))
        else:
            self.write_printenv()

    def do_run(self, args: argparse.Namespace, argv: list[str]) -> None:
        """
        Run a subcommand with the environment set up to use the generated
        library.
        """
        # The special "--" argument can be used to separate options meant for
        # "lkm run" itself from options meant for the subprocess. If it is
        # present, do not include it in the subprocess command line.
        if argv and argv[0] == "--":
            argv = argv[1:]
        self.check_call("Subcommand", argv)

    def do_create_wheel(self, args: argparse.Namespace) -> None:
        """
        Create a standalone Python wheel for the Python bindings.
        """
        packager = WheelPackager(
            WheelPackager.args_to_env(args), args.library_types
        )
        packager.create_python_wheel(
            args.python_tag,
            args.plat_name,
            getattr(args, 'wheel-dir'),
            getattr(args, 'build-dir'),
            getattr(args, 'dyn-deps-dir'),
            getattr(args, 'install-dir'),
            project_name=self.context.ada_api_settings.lib_name.lower(),
            lib_name='lib{}'.format(
                self.context.c_api_settings.shared_object_basename
            ),
            python_interpreter=args.with_python
        )

    def do_list_optional_passes(self, args: argparse.Namespace) -> None:
        """
        List optional passes and exit.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        printcol("Optional passes", Colors.CYAN)
        self.context.create_all_passes()

        for p in self.context.all_passes:
            if p.is_optional:
                print("")
                print(
                    col(p.name, Colors.YELLOW),
                    "({} by default)".format(
                        "disabled" if p.disabled else "enabled"
                    ),
                )
                for line in textwrap.wrap(
                    p.doc,
                    79,
                    initial_indent="  ",
                    subsequent_indent="  ",
                ):
                    print(line)

    def setup_environment(self, add_path: Callable[[str, str], None]) -> None:
        """
        Call ``add_path(varname, dirname)`` for each path environment variable
        ``varname`` that must be extended to include directory ``dirname``.

        :param add_path: Callback to add a directory to a path environment
            variable.
        """

        P = self.dirs.build_dir

        # Make the project file available
        add_path("GPR_PROJECT_PATH", P())

        # Give direct access to the C header so that non-GPR build systems
        # (like when building JNI bindings) can use this header.
        add_path("C_INCLUDE_PATH", P("src"))

        # Make the scripts and mains available
        add_path("PATH", P("scripts"))

        # If we're in a command that supports the build-mode argument, then set
        # environment variables that depends on it, such as the bin directory
        # for programs, and the various dynlib paths.

        if self.build_modes:
            # printenv only supports one build mode, and for other commands we
            # don't care about what this changes. TODO: It still feels ugly,
            # probably decoupling setup_environment so that this is not needed
            # except for printenv would be better.
            build_mode = self.build_modes[0]
            add_path("PATH", P("obj-mains", build_mode.value))

            # Make the shared lib available, regardless of the operating system
            shared_dir = P("lib", "relocatable", build_mode.value)
            add_path("LD_LIBRARY_PATH", shared_dir)
            add_path("LIBRARY_PATH", shared_dir)
            add_path("DYLD_LIBRARY_PATH", shared_dir)
            add_path("PATH", shared_dir)

        # Make the Python bindings available to Python interpreters and to Mypy
        add_path("PYTHONPATH", P("python"))
        add_path("MYPYPATH", P("python"))

        # If the .lib file has been generated, provide path to it and set the
        # additional required paths.
        lib_file = P(
            "lib",
            "windows",
            f"{self.context.config.library.language_name.lower}lang.lib"
        )
        if path.isfile(lib_file):
            add_path("LIB", path.dirname(lib_file))
            add_path("INCLUDE", P("src"))

        # Set required environment variables for Java bindings
        add_path(
            "CLASSPATH",
            P('java', 'target', f'{self.lib_name.lower()}.jar')
        )
        add_path("LD_LIBRARY_PATH", P('java', 'jni'))
        add_path("PATH", P('java', 'jni'))

    def derived_env(self) -> dict[str, str]:
        """
        Return a copy of the environment after an update using
        setup_environment.
        """
        env = dict(os.environ)
        self.setup_environment(lambda name, p: add_to_path(env, name, p))
        return env

    def write_printenv(self, output_file: TextIO = sys.stdout) -> None:
        """
        Display Bourne shell commands that setup environment in order to make
        the generated library available.

        :param output_file: File to which this should write the shell commands.
        """
        def add_path(name: str, path: str) -> None:
            output_file.write(format_printenv(name, path) + '\n')

        self.setup_environment(add_path)

    def check_call(
        self,
        name: str,
        argv: list[str],
        env: dict[str, str] | None = None,
        abort_on_error: bool = True,
    ) -> bool:
        """
        Log and run a command with a derived environment.

        Return True if the command completes with success. Otherwise, emit an
        error message and, if `abort_on_error` is true, exit with an error
        status code.

        :param name: Name of the process to run, use for error message
            formatting only.
        :param argv: Arguments for the command to run.
        :param env: Environment to use for the command to run. If None, use
            self.derived_env().
        :param abort_on_error: If the command stops with an error, exit
            ourselves.
        """
        self.log_exec(argv)
        if env is None:
            env = self.derived_env()
        try:
            subprocess.check_call(argv, env=env)
        except (subprocess.CalledProcessError, OSError) as exc:
            print(
                '{color}{name} failed:{reset}'
                ' error while running {argv}:'
                '\n    {exc}'.format(
                    color=Colors.FAIL if abort_on_error else Colors.WARNING,
                    name=name,
                    reset=Colors.ENDC,
                    argv=' '.join(argv),
                    exc=exc
                )
            )
            if abort_on_error:
                sys.exit(1)
            else:
                return False
        return True

    def log_exec(self, argv: list[str]) -> None:
        """
        If verbosity level is debug, log a command we are about to execute.

        :param argv: Arguments for the command to log.
        """
        if self.verbosity.debug:
            printcol('Executing: {}'.format(shlex.join(argv)), Colors.CYAN)

    def log_info(self, msg: str, color: str) -> None:
        """
        If verbosity level is info, log a message with given color.
        """
        if self.verbosity.info:
            printcol(msg, color)

    def log_debug(self, msg: str, color: str) -> None:
        """
        If verbosity level is debug, log a message with given color.
        """
        if self.verbosity.debug:
            printcol(msg, color)
