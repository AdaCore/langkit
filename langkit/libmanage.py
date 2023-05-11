from __future__ import annotations

import argparse
from functools import reduce
import glob
import inspect
import json
import os
from os import path
import pdb
import pipes
import shutil
import subprocess
import sys
import traceback
from typing import (
    Any, Callable, Dict, List, Optional, Optional as Opt, Sequence, Set,
    TYPE_CHECKING, Text, TextIO, Tuple, Type, Union, cast
)

from langkit.compile_context import UnparseScript, Verbosity
from langkit.diagnostics import (
    DiagnosticError, DiagnosticStyle, Diagnostics, Location, WarningSet,
    check_source_language, diagnostic_context, extract_library_location
)
from langkit.packaging import Packager
from langkit.utils import (
    BuildMode, Colors, LibraryType, Log, add_to_path, col, format_setenv,
    get_cpu_count, parse_choice, parse_cmdline_args, parse_list_of_choices,
    printcol
)


if TYPE_CHECKING:
    from enum import Enum
    from langkit.compile_context import CompileCtx
    from langkit.passes import AbstractPass
    from types import TracebackType


class Directories:
    """
    Helper class used to get various path in source/build/install trees.
    """

    def __init__(self,
                 lang_source_dir: str,
                 build_dir: Opt[str] = None,
                 install_dir: Opt[str] = None):

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
                 values: Union[Text, Sequence[Any], None],
                 option_string: Opt[Text] = None) -> None:
        namespace.enabled_warnings.enable(values)


class DisableWarningAction(argparse.Action):
    def __call__(self,
                 parser: argparse.ArgumentParser,
                 namespace: argparse.Namespace,
                 values: Union[Text, Sequence[Any], None],
                 option_string: Opt[Text] = None) -> None:

        namespace.enabled_warnings.disable(values)


class ManageScript:
    build_modes: List[BuildMode]
    """
    Build modes to build.
    """

    ENABLE_BUILD_WARNINGS_DEFAULT = False
    """
    Whether warnings to build the generated library are enabled by default.
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

    def __init__(self, root_dir: Optional[str] = None) -> None:
        """
        :param root_dir: Root directory for the language specification. All
            source file under that directory are considered to be part of the
            language spec, and build trees are by default relative to it.

            If left to None, take the directory that contains the Python file
            that defined ``self``'s class.
        """
        self.dirs = Directories(
            lang_source_dir=root_dir or path.dirname(
                path.abspath(inspect.getfile(self.__class__))
            )
        )

        ########################
        # Main argument parser #
        ########################

        self.args_parser = args_parser = argparse.ArgumentParser(
            description='General manager to handle actions relative to'
                        ' building/testing your language.'
        )
        self.subparsers = args_parser.add_subparsers()

        ########
        # Help #
        ########

        self.help_parser = self.add_subcommand(self.do_help)

        ############
        # Generate #
        ############

        self.generate_parser = generate_parser = self.add_subcommand(
            self.do_generate, needs_context=True
        )
        self.add_generate_args(generate_parser)

        #########
        # Build #
        #########

        self.build_parser = build_parser = self.add_subcommand(
            self.do_build, needs_context=True
        )
        self.add_build_args(build_parser)

        ########
        # Make #
        ########

        self.make_parser = make_parser = self.add_subcommand(
            self.do_make, needs_context=True
        )
        self.add_generate_args(make_parser)
        self.add_build_args(make_parser)

        ########################
        # List optional passes #
        ########################

        self.list_optional_passes_parser = self.add_subcommand(
            self.do_list_optional_passes, needs_context=True
        )
        self.add_generate_args(self.list_optional_passes_parser)

        ###########
        # Install #
        ###########

        self.install_parser = install_parser = self.add_subcommand(
            self.do_install, needs_context=True
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

        ##########
        # Setenv #
        ##########

        self.setenv_parser = self.add_subcommand(
            self.do_setenv, needs_context=True
        )
        self.add_build_mode_arg(self.setenv_parser)
        self.setenv_parser.add_argument(
            '--json', '-J', action='store_true',
            help='Output necessary env keys to JSON.'
        )

        #######################
        # Create Python wheel #
        #######################

        self.create_wheel_parser = self.add_subcommand(
            self.do_create_wheel, needs_context=True
        )
        Packager.add_platform_options(self.create_wheel_parser)
        self.create_wheel_parser.add_argument(
            '--with-python',
            help='Python intererpter to use in order to build the wheel. If'
                 ' not provided, use the current one.'
        )
        self.create_wheel_parser.add_argument(
            '--tag',
            help="Tag for the wheel (setup.py's --python-tag argument)."
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
        callback: Union[Callable[[argparse.Namespace], None],
                        Callable[[argparse.Namespace, List[str]], None]],
        *,
        needs_context: bool = False,
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

        :param needs_context: Whether ``callback`` needs a ``CompileCtx``
            created beforehand.

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

        # If this subcommand requires a context, make sure we create the
        # context before running callback.
        def wrapper(parsed_args: argparse.Namespace,
                    unknown_args: List[str]) -> None:
            if needs_context:
                self.set_context(parsed_args)
            if accept_unknown_args:
                cb_full = cast(
                    Callable[[argparse.Namespace, List[str]], None],
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
        subparser.add_argument(
            '--build-dir', default='build',
            help='Directory to use for generated source code and binaries. By'
                 ' default, use "build" in the current directory.'
        )

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
        subparser.add_argument(
            '--no-ada-api', action='store_true',
            help='Do not generate units to provide an Ada API, and disable the'
                 ' generation of mains.'
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
        subparser.add_argument(
            '--plugin-pass', action='append', default=[],
            help='Fully qualified name to a Langkit plug-in pass constructor.'
                 ' The function must return a Langkit pass, whose type derives'
                 ' from langkit.passes.AbstractPass. It will be ran at the end'
                 ' of the pass preexisting order.'
        )
        subparser.add_argument(
            '--pass-on', type=str, action='append', default=[],
            help='Activate an optional pass by name.'
        )
        subparser.add_argument(
            '--pass-off', type=str, action='append', default=[],
            help='Deactivate an optional pass by name.'
        )

    @staticmethod
    def add_generate_args(subparser: argparse.ArgumentParser) -> None:
        """
        Add arguments to tune code generation to "subparser".
        """
        subparser.add_argument(
            '--pretty-print', '-p', action='store_true',
            help='Pretty-print generated source code'
        )
        subparser.add_argument(
            '--no-pretty-print', '-P',
            dest='pretty_print', action='store_false',
            help='Do not try to pretty-print generated source code (the'
                 ' default).'
        )
        subparser.add_argument(
            '--annotate-fields-types', action='store_true',
            help='Experimental feature. Modify the Python files where the'
                 ' node types are defined, to annotate empty Field() '
                 ' definitions.'
        )
        subparser.add_argument(
            '--check-only', action='store_true',
            help="Only check the input for errors, don't generate the code."
        )
        subparser.add_argument(
            '--no-property-checks', action='store_true',
            help="Don't generate runtime checks for properties."
        )
        subparser.add_argument(
            '--list-warnings', action='store_true',
            help='Display the list of available warnings.'
        )
        subparser.add_argument(
            '--enable-warning', '-W', dest='enabled_warnings',
            default=WarningSet(),
            action=EnableWarningAction,
            choices=[w.name for w in WarningSet.available_warnings],
            help='Enable a warning.'
        )
        subparser.add_argument(
            '--disable-warning', '-w',
            action=DisableWarningAction,
            choices=[w.name for w in WarningSet.available_warnings],
            help='Disable a warning.'
        )
        subparser.add_argument(
            '--generate-unparser', action='store_true', default=False,
            help='Generate an unparser along with the parser for the grammar.'
                 ' Note that this machinery is intended only for languages'
                 ' that have no significant whitespace, i.e. where whitespaces'
                 ' can be abitrary inserted between two tokens without'
                 ' affecting lexing.'
        )
        subparser.add_argument(
            '--no-gdb-hook', action='store_true',
            help='Do not generate the ".debug_gdb_script" section. This'
                 ' section is used to automatically run Langkit GDB helpers'
                 ' when loading the generated library in a debugger.'
                 ' Conventient for debugging, but bad for releases as this'
                 ' hardcodes source paths in the sources.'
        )
        subparser.add_argument(
            '--coverage', action='store_true',
            help='Instrument the generated library to compute its code'
                 ' coverage. This requires GNATcoverage.'
        )
        subparser.add_argument(
            '--relative-project', action='store_true',
            help='Use relative paths in generated project files. This is'
                 ' useful in order to get portable generated sources, for'
                 ' releases for instance.'
        )
        subparser.add_argument(
            "--version", help="Version number for the generated library",
        )
        subparser.add_argument(
            "--build-date", help="Build date number for the generated library",
        )

        # RA22-015: option to dump the results of the unparsing concrete syntax
        # to a file.
        subparser.add_argument(
            '--unparse-script', type=UnparseScript, default=None,
            help='If specified, sequence of actions to generate definition of'
                 ' the source language using the concrete syntax DSL. Actions'
                 ' are separated by commas. These can be:'
                 ' to:FILE (write the next actions to the given file),'
                 ' import:MODULE (write an import statement for the given'
                 ' module name,'
                 ' nodes (write definitions for nodes),'
                 ' lexer (write the lexer definition),'
                 ' grammar (write the grammar definition).'
        )

    def add_build_mode_arg(self, subparser: argparse.ArgumentParser) -> None:

        def parse_choice_into_list(s: str) -> List[Enum]:
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
            '--disable-mains', type=self.parse_mains_list, default=[], nargs=1,
            help=('Comma-separated list of main programs no to build.'
                  ' Supported main programs are: {}'.format(
                      ', '.join(sorted(self.main_programs))
                  ))
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
            '--enable-java', action='store_true',
            help='Enable the Java bindings building/installation.'
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

    def create_context(self, args: argparse.Namespace) -> 'CompileCtx':
        """
        Return a Langkit context (langkit.compile_context.CompileContext
        instance).

        This must be overriden by subclasses.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        raise NotImplementedError()

    @property
    def main_source_dirs(self) -> Set[str]:
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
    def main_programs(self) -> Set[str]:
        """
        Return the list of main programs to build in addition to the generated
        library. Subclasses should override this to add more main programs.
        """
        return {'parse'}

    def parse_mains_list(self, mains: str) -> Set[str]:
        """
        Parse a comma-separated list of main programs. Raise a ValueError if
        one is not a supported main program.
        """
        if not mains:
            return set()

        supported_mains = self.main_programs
        result = set(mains.split(','))
        if not result.issubset(supported_mains):
            raise ValueError('Invalid main programs: {}'.format(
                ', '.join(sorted(result - supported_mains))
            ))
        return result

    @property
    def lib_name(self) -> str:
        return self.context.ada_api_settings.lib_name

    def run(self, argv: Opt[List[str]] = None) -> None:
        return_code = self.run_no_exit(argv)
        if return_code != 0:
            sys.exit(return_code)

    def run_no_exit(self, argv: Opt[List[str]] = None) -> int:
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
                               tb: Optional[TracebackType]) -> Any:
                    traceback.print_exception(typ, value, tb)
                    pdb.post_mortem(tb)

                sys.excepthook = excepthook
            else:
                sys.excepthook = ultratb.FormattedTB(
                    mode='Verbose', color_scheme='Linux', call_pdb=1
                )

        self.dirs.set_build_dir(parsed_args.build_dir)
        install_dir = getattr(parsed_args, 'install-dir', None)
        if install_dir:
            self.dirs.set_install_dir(install_dir)

        if getattr(parsed_args, 'list_warnings', False):
            WarningSet.print_list()
            return 0

        # noinspection PyBroadException
        try:
            parsed_args.func(parsed_args, unknown_args)
            return 0

        except DiagnosticError:
            if parsed_args.debug:
                raise
            if parsed_args.verbosity.debug or parsed_args.full_error_traces:
                traceback.print_exc()
            print(col('Errors, exiting', Colors.FAIL))
            return 1

        except Exception as e:
            if parsed_args.debug:
                raise
            ex_type, ex, tb = sys.exc_info()

            # If we have a syntax error, we know for sure the last stack frame
            # points to the code that must be fixed. Otherwise, point to the
            # top-most stack frame that does not belong to Langkit.
            if e.args and e.args[0] == 'invalid syntax':
                assert isinstance(e, SyntaxError)
                loc = Location(cast(str, e.filename), cast(int, e.lineno))
            else:
                loc = cast(Location,
                           extract_library_location(traceback.extract_tb(tb)))
            with diagnostic_context(loc):
                check_source_language(False, str(e), do_raise=False)

            # Keep Langkit bug "pretty" for users: display the Python stack
            # trace only when requested.
            if parsed_args.verbosity.debug or parsed_args.full_error_traces:
                traceback.print_exc()

            print(col('Internal error! Exiting', Colors.FAIL))
            return 1

        finally:
            if parsed_args.profile:
                pr.disable()
                ps = pstats.Stats(pr)
                ps.dump_stats('langkit.prof')

    def set_context(self, parsed_args: argparse.Namespace) -> None:
        self.context = self.create_context(parsed_args)

        # Set version numbers from the command-line, if present
        self.context.set_versions(
            version=getattr(parsed_args, "version", None),
            build_date=getattr(parsed_args, "build_date", None),
        )

        # Set the extensions dir on the compile context
        self.context.extensions_dir = self.dirs.lang_source_dir(
            "extensions"
        )

    @property
    def extra_code_emission_passes(self) -> List[AbstractPass]:
        """
        Return passes to forward to ``CompileCtx.code_emission_passes``.

        ``ManageScript`` subclasses can override this to add the generation of
        extra Ada source files.
        """
        return []

    def prepare_generation(self, args: argparse.Namespace) -> None:
        """
        Prepare generation of the DSL code (initialize the compilation context
        and user defined options).
        """

        # Get source directories for the mains project file. making them
        # relative to the generated project file (which is
        # $BUILD_DIR/mains.gpr).
        main_source_dirs = {
            os.path.relpath(
                self.dirs.lang_source_dir(sdir),
                os.path.dirname(self.mains_project)
            )
            for sdir in self.main_source_dirs
        }

        explicit_passes_triggers = {p: True for p in args.pass_on}
        explicit_passes_triggers.update({p: False for p in args.pass_off})

        self.context.create_all_passes(
            lib_root=self.dirs.build_dir(),
            main_source_dirs=main_source_dirs,
            main_programs=self.main_programs,
            check_only=args.check_only,
            warnings=args.enabled_warnings,
            no_property_checks=args.no_property_checks,
            generate_unparser=args.generate_unparser,
            generate_gdb_hook=not args.no_gdb_hook,
            plugin_passes=args.plugin_pass,
            pretty_print=args.pretty_print,
            coverage=args.coverage,
            relative_project=args.relative_project,
            unparse_script=args.unparse_script,
            explicit_passes_triggers=explicit_passes_triggers,
            extra_code_emission_passes=self.extra_code_emission_passes,
        )

    def gnatpp(self, project_file: str, glob_pattern: str) -> None:
        """
        Helper function to pretty-print files from a GPR project.
        """
        # In general, don't abort if we can't find gnatpp or if gnatpp
        # crashes: at worst sources will not be pretty-printed, which is
        # not a big deal. `check_call` will emit warnings in this case.

        if self.verbosity.debug:
            self.check_call('Show pp path', ['which', 'gnatpp'],
                            abort_on_error=False)
            self.check_call('Show pp version',
                            ['gnatpp', '--version'],
                            abort_on_error=False)

        argv = ['gnatpp', '-P{}'.format(project_file),
                '--syntax-only',
                '--eol=lf']

        if self.verbosity.debug:
            argv.append('-v')

        self.check_call(
            'Pretty-printing',
            argv + self.gpr_scenario_vars('relocatable')
            + glob.glob(glob_pattern),
            abort_on_error=False
        )

    def do_generate(self, args: argparse.Namespace) -> None:
        """
        Generate source code for the user language.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        self.prepare_generation(args)

        self.log_info(
            "Generating source for {}...".format(self.lib_name.lower()),
            Colors.HEADER
        )

        self.context.emit()

        if args.check_only:
            return

        if getattr(args, 'pretty_print', False):
            self.log_info(
                "Pretty-printing sources for {}...".format(
                    self.lib_name.lower()
                ),
                Colors.HEADER
            )
            self.gnatpp(self.lib_project, self.dirs.build_dir('src', '*.ad*'))
            self.gnatpp(self.mains_project,
                        self.dirs.build_dir('src-mains', '*.ad*'))

        self.log_info("Generation complete!", Colors.OKGREEN)

    def what_to_build(
        self,
        args: argparse.Namespace,
        is_library: bool
    ) -> List[List[Tuple[BuildMode, LibraryType]]]:
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
    ) -> List[str]:
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
                 mains: Set[str] = set()) -> None:
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

        def build(configs: List[Tuple[BuildMode, LibraryType]]) -> None:
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

    def generate_lib_file(self, build_mode: BuildMode) -> None:
        """
        Run tools to generate a .lib file from the language DLL.
        """
        def parse_dumpbin_result(dumpbin_result: str) -> list[str]:
            """
            Extract all symbol names from the output of the "dumpbin" command.

            Dumpbin's results are following this format:

            .. code-block::

                Microsoft (R) COFF/PE Dumper Version 14.35.32217.1
                Copyright (C) Microsoft Corporation.  All rights reserved.


                Dump of file my_lib.dll

                File Type: DLL

                  Section contains the following exports for my_lib.dll

                    00000000 characteristics
                    64382B47 time date stamp Thu Apr 13 18:18:15 2023
                        0.00 version
                           1 ordinal base
                        4 number of functions
                        4 number of names

                    ordinal hint RVA      name

                          1    0 0001BA4E Symbol_1
                          2    1 001AF008 Symbol_2
                          3    2 0001BB12 Symbol_3
                          4    3 0002B19E Symbol_N

                Summary

                    1000 .CRT
                    6000 .bss
                    2000 .data
                ...

            :param dumpbin_result: Result of a "dumpbin /exports" run.
            :return: A list containing all names.
            """
            res = []
            parse_line = False

            # For each line of the dumpbin result parse it
            for line in dumpbin_result.splitlines():
                words = line.split()

                if not parse_line:
                    # Spot the heading line of the function listing
                    parse_line = words == ['ordinal', 'hint', 'RVA', 'name']

                else:
                    # Add the function name
                    if len(words) == 4:
                        res.append(words[3])

                    # Spot the ending line
                    if len(words) != 4 and len(words) != 0:
                        parse_line = False

            return res

        # Create the file and directory names
        dll_file = self.dirs.build_lib_dir(
            "relocatable",
            build_mode.value,
            f"{self.lib_name.lower()}.dll"
        )
        def_file = self.dirs.build_lib_dir(
            "windows",
            f"{self.lib_name.lower()}.def"
        )
        lib_file = self.dirs.build_lib_dir(
            "windows",
            f"{self.context.lang_name.lower}lang.lib"
        )

        # Create the windows directory if it doesn't exist
        if not path.isdir(self.dirs.build_lib_dir("windows")):
            os.makedirs(self.dirs.build_lib_dir("windows"))

        # Run dumpbin to get the DLL names
        dumpbin_out = subprocess.check_output([
            "dumpbin",
            "/exports",
            dll_file,
        ])

        # Write the result of the parsed dumpbin in the .def file
        with open(def_file, 'w') as f:
            print("EXPORTS", file=f)
            for name in parse_dumpbin_result(dumpbin_out.decode()):
                print(name, file=f)

        # Generate the .lib file from the .def one
        subprocess.check_call([
            "lib",
            f"/def:{def_file}",
            f"/out:{lib_file}",
            "/machine:x64",
            "/nologo",
        ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

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

        # Building Java bindings does not go through GPRbuild, so we must
        # explicitly give access to the generated C header.
        env = self.derived_env(direct_c_header=True)

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

        # Then build the main programs
        disabled_mains: Set[str] = reduce(
            set.union, args.disable_mains, set()
        )
        mains = (set()
                 if args.disable_all_mains else
                 self.main_programs - disabled_mains)
        if mains:
            self.log_info("Building the main programs...", Colors.HEADER)
            self.gprbuild(args, self.mains_project,
                          is_library=False, mains=mains)

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
                self.generate_lib_file(self.build_modes[0])

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

    def do_setenv(self, args: argparse.Namespace) -> None:
        """
        Unless --json is passed, display Bourne shell commands that setup
        environment in order to make the generated library available.
        Otherwise, return a JSON document that describe this environment.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        if args.json:
            result: Dict[str, str] = {}

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
            self.write_setenv()

    def do_create_wheel(self, args: argparse.Namespace) -> None:
        """
        Create a standalone Python wheel for the Python bindings.
        """
        packager = Packager(Packager.args_to_env(args),
                            args.library_types,
                            None)
        packager.create_python_wheel(
            args.tag,
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
        self.prepare_generation(args)
        printcol("Optional passes\n", Colors.CYAN)
        for p in self.context.all_passes:
            if p.is_optional:
                printcol(p.name, Colors.YELLOW)
                print(p.doc)
                print()

    def do_help(self, args: argparse.Namespace) -> None:
        """
        Print usage and exit.

        :param args: The arguments parsed from the command line invocation of
            manage.py.
        """
        del args  # Unused in this implementation
        self.args_parser.print_help()

    def setup_environment(
        self,
        add_path: Callable[[str, str], None],
        direct_c_header: bool = False,
    ) -> None:
        """
        Call ``add_path(varname, dirname)`` for each path environment variable
        ``varname`` that must be extended to include directory ``dirname``.

        :param add_path: Callback to add a directory to a path environment
            variable.
        :param direct_c_header: Whether the environment must be set to give
            direct access to the C header. This is needed when using a build
            system other than GPRbuild (for instance when building JNI
            bindings).
        """

        P = self.dirs.build_dir

        # Make the project file available
        add_path("GPR_PROJECT_PATH", P())

        # If requested, also give direct access to the C header
        if direct_c_header:
            add_path("C_INCLUDE_PATH", P("src"))

        # Make the scripts and mains available
        add_path("PATH", P("scripts"))

        # If we're in a command that supports the build-mode argument, then set
        # environment variables that depends on it, such as the bin directory
        # for programs, and the various dynlib paths.

        if self.build_modes:
            # setenv only supports one build mode, and for other commands we
            # don't care about what this changes. TODO: It still feels ugly,
            # probably decoupling setup_environment so that this is not needed
            # except for setenv would be better.
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
            f"{self.context.lang_name.lower}lang.lib"
        )
        if path.isfile(lib_file):
            add_path("LIB", path.dirname(lib_file))
            add_path("INCLUDE", P())

        # If the Java bindings have been build, set the necessary environment
        # variables.
        bindings_jar = P('java', 'target', f'{self.lib_name.lower()}.jar')
        if path.isfile(bindings_jar):
            add_path("CLASSPATH", bindings_jar)
            add_path("LD_LIBRARY_PATH", P('java', 'jni'))

    def derived_env(self, direct_c_header: bool = False) -> Dict[str, str]:
        """
        Return a copy of the environment after an update using
        setup_environment.

        :param direct_c_header: See ``setup_environment``.
        """
        env = dict(os.environ)
        self.setup_environment(
            lambda name, p: add_to_path(env, name, p),
            direct_c_header=direct_c_header,
        )
        return env

    def write_setenv(self, output_file: TextIO = sys.stdout) -> None:
        """
        Display Bourne shell commands that setup environment in order to make
        the generated library available.

        :param output_file: File to which this should write the shell commands.
        """
        def add_path(name: str, path: str) -> None:
            output_file.write(format_setenv(name, path) + '\n')

        self.setup_environment(add_path)

    def check_call(self,
                   name: str,
                   argv: List[str],
                   env: Opt[Dict[str, str]] = None,
                   abort_on_error: bool = True) -> bool:
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

    def log_exec(self, argv: List[str]) -> None:
        """
        If verbosity level is debug, log a command we are about to execute.

        :param argv: Arguments for the command to log.
        """
        if self.verbosity.debug:
            printcol('Executing: {}'.format(
                ' '.join(pipes.quote(arg) for arg in argv)
            ), Colors.CYAN)

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
