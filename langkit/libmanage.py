from __future__ import absolute_import, division, print_function

import argparse
from functools import reduce
from funcy import keep
import glob
import inspect
import json
import os
from os import path
import pdb
import pipes
import shlex
import shutil
import subprocess
import sys
import traceback

from langkit.compile_context import Verbosity
from langkit.diagnostics import (
    Context, DiagnosticError, DiagnosticStyle, Diagnostics, Location,
    WarningSet, check_source_language, extract_library_location
)
from langkit.langkit_support import LangkitSupport
from langkit.utils import Colors, Log, col, printcol


class Directories(object):
    """
    Helper class used to get various path in source/build/install trees.
    """

    def __init__(self, lang_source_dir=None, build_dir=None, install_dir=None):
        self.root_lang_source_dir = lang_source_dir
        self.root_langkit_source_dir = path.dirname(
            path.abspath(__file__)
        )
        self.root_build_dir = build_dir
        self.root_install_dir = install_dir

    def set_build_dir(self, build_dir):
        self.root_build_dir = path.abspath(build_dir)

    def set_install_dir(self, install_dir):
        self.root_install_dir = path.abspath(install_dir)

    def lang_source_dir(self, *args):
        return path.join(self.root_lang_source_dir, *args)

    def build_dir(self, *args):
        return path.join(self.root_build_dir, *args)

    def install_dir(self, *args):
        return path.join(self.root_install_dir, *args)

    def langkit_source_dir(self, *args):
        """
        Build and return the path for ``args`` under the root source directory
        for langkit.

        :param list[str] args: The path components, same semantics as in
            path.join.
        """
        return path.join(self.root_langkit_source_dir, *args)


class EnableWarningAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string):
        namespace.enabled_warnings.enable(values)


class DisableWarningAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string):
        namespace.enabled_warnings.disable(values)


def get_cpu_count():
    # The "multiprocessing" module is not available on GNATpython's
    # distribution for PPC AIX and the "cpu_count" is not available on Windows:
    # give up on default parallelism on these platforms.
    try:
        import multiprocessing
        return multiprocessing.cpu_count()
    except (ImportError, NotImplementedError):
        return 1


class LibraryTypes(object):

    types = {'static', 'static-pic', 'relocatable'}

    def __init__(self, static=False, static_pic=False, relocatable=False):
        self.static = static
        self.static_pic = static_pic
        self.relocatable = relocatable

    def __str__(self):
        return ','.join(
            name for enabled, name in [(self.static, 'static'),
                                       (self.static_pic, 'static-pic'),
                                       (self.relocatable, 'relocatable')]
            if enabled)

    @classmethod
    def parse(cls, arg):
        """
        Decode the value passed to the --library-types command-line argument.

        :param str arg: Value to decode.
        :rtype: LibraryTypes
        """
        library_types = arg.split(',')
        library_type_set = set(library_types)

        # Make sure that all requested library types are supported
        unsupported = library_type_set - cls.types
        if unsupported:
            raise ValueError('Unsupported library types: {}'
                             .format(', '.join(sorted(unsupported))))

        # Make sure that the given list of library types contains no double
        # entries.
        if len(library_types) != len(library_type_set):
            raise ValueError('Library types cannot be requested twice')

        return cls(static='static' in library_type_set,
                   static_pic='static-pic' in library_type_set,
                   relocatable='relocatable' in library_type_set)


class ManageScript(object):

    BUILD_MODES = ('dev', 'prod')

    ENABLE_BUILD_WARNINGS_DEFAULT = False
    """
    Whether warnings to build the generated library are enabled by default.
    """

    def __init__(self, override_lang_source_dir=True):

        self.dirs = Directories(
            # It is assumed that manage.py is at the root of the language
            # definition source directory.
            lang_source_dir=path.dirname(
                path.abspath(inspect.getfile(self.__class__))
            )
        )
        if override_lang_source_dir:
            Diagnostics.set_lang_source_dir(self.dirs.lang_source_dir())

        ########################
        # Main argument parser #
        ########################

        self.args_parser = args_parser = argparse.ArgumentParser(
            description='General manager to handle actions relative to'
                        ' building/testing your language.'
        )
        self.subparsers = subparsers = args_parser.add_subparsers()

        args_parser.add_argument(
            '--build-dir', default='build',
            help='Directory to use for generated source code and binaries. By'
                 ' default, use "build" in the current directory.'
        )
        args_parser.add_argument(
            '--library-types', default=LibraryTypes(relocatable=True),
            type=LibraryTypes.parse,
            help='Comma-separated list of library types to build (relocatable,'
                 ' static-pic and static). By default, build only shared'
                 ' libraries.'
        )
        args_parser.add_argument(
            '--verbosity', '-v', nargs='?',
            type=Verbosity,
            choices=Verbosity.choices(),
            default=Verbosity('info'),
            const=Verbosity('debug'),
            help='Verbosity level'
        )
        args_parser.add_argument(
            '--full-error-traces', '-E', action='store_true', default=False,
            help='Always show full error traces, whatever the verbosity level'
                 ' (default: disabled).'
        )
        args_parser.add_argument(
            '--trace', '-t', action='append', default=[],
            help='Activate given debug trace.'
        )
        args_parser.add_argument(
            '--no-langkit-support', action='store_true',
            help='Assuming that Langkit_Support is already built and'
                 ' installed. This is useful to package the generated library'
                 ' only.'
        )

        # Don't enable this by default so that errors will not make automated
        # tasks hang.
        args_parser.add_argument(
            '-g', '--debug', action='store_true',
            help='In case of internal error or diagnostic error, run a'
                 ' post-mortem PDB session.'
        )
        args_parser.add_argument(
            '--profile', action='store_true',
            help='Run cProfile and langkit, and generate a data file'
                 ' "langkit.prof".'
        )
        args_parser.add_argument(
            '--diagnostic-style', '-D', type=DiagnosticStyle,
            default=DiagnosticStyle.default,
            help='Style for error messages.'
        )
        args_parser.add_argument(
            '--plugin-pass', action='append', default=[],
            help='Fully qualified name to a Langkit plug-in pass constructor.'
                 ' The function must return a Langkit pass, whose type derives'
                 ' from langkit.passes.AbstractPass. It will be ran at the end'
                 ' of the pass preexisting order.'
        )

        def create_parser(fn, needs_context=False):
            """
            Create a subparser from a function. Uses the name and the docstring
            of the function to document the subparsers.

            :param (ManageScript, Namespace) -> None fn: The function to use.
            :param bool needs_context: Whether the executed function needs a
                CompileCtx created beforehand or not.
            :rtype: argparse.ArgumentParser
            """
            p = subparsers.add_parser(
                # Take the name of the function without the do_ prefix and with
                # dashes instead of underscores.
                fn.__name__.replace('do_', '').replace('_', '-'),

                # Take the first paragraph of the function's documentation as
                # help.
                help=fn.__doc__.split('\n\n')[0].strip()
            )

            def internal(*args, **kwargs):
                if needs_context:
                    self.set_context(*args, **kwargs)
                fn(*args, **kwargs)

            p.set_defaults(func=internal)
            return p

        ########
        # Help #
        ########

        self.help_parser = create_parser(self.do_help)

        ############
        # Generate #
        ############

        self.generate_parser = generate_parser = create_parser(
            self.do_generate, True
        )
        self.add_generate_args(generate_parser)

        #########
        # Build #
        #########

        self.build_parser = build_parser = create_parser(self.do_build, True)
        self.add_build_args(build_parser)

        ########
        # Make #
        ########

        self.make_parser = make_parser = create_parser(self.do_make, True)
        self.add_generate_args(make_parser)
        self.add_build_args(make_parser)

        ###########
        # Install #
        ###########

        self.install_parser = install_parser = create_parser(self.do_install,
                                                             True)
        install_parser.add_argument(
            'install-dir',
            help='Installation directory.'
        )

        install_parser.add_argument(
            '--force', '-f', action='store_true',
            help='Force installation, overwrite files.'
        )

        ##########
        # Setenv #
        ##########

        self.setenv_parser = create_parser(self.do_setenv, True)

        self.setenv_parser.add_argument(
            '--json', '-J', action='store_true',
            help='Output necessary env keys to JSON.'
        )

        ###############################################
        # Generate, Build and Install Langkit_Support #
        ###############################################

        self.generate_lksp_parser = create_parser(
            self.do_generate_langkit_support
        )
        self.build_lksp_parser = create_parser(
            self.do_build_langkit_support
        )
        self.install_lksp_parser = create_parser(
            self.do_install_langkit_support
        )
        self.install_lksp_parser.add_argument(
            'install-dir',
            help='Installation directory.'
        )
        self.install_lksp_parser.add_argument(
            '--force', '-f', action='store_true',
            help='Force installation, overwrite files.'
        )

        self.add_build_args(self.build_lksp_parser)
        self.add_build_args(self.install_lksp_parser)

        # The create_context method will create the context and set it here
        # only right before executing commands.
        self.context = None

        # This will be set in the run method, when we have parsed arguments
        # from the command line.
        self.verbosity = None
        ":type: Verbosity"

    def add_generate_args(self, subparser):
        """
        Add arguments to tune code generation to "subparser".

        :type subparser: argparse.ArgumentParser
        """
        subparser.add_argument(
            '--no-pretty-print', '-P', action='store_true',
            help='Do not try to pretty-print generated source code.'
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
            '--no-astdoc', '-D', dest='no_astdoc',
            action='store_true',
            help='Do not generate the HTML documentation for AST nodes, their'
                 ' fields and their properties.'
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

    def add_build_args(self, subparser):
        """
        Add arguments to tune code compilation to "subparser".

        :type subparser: argparse.ArgumentParser
        """
        subparser.add_argument(
            '--jobs', '-j', type=int, default=get_cpu_count(),
            help='Number of parallel jobs to spawn in parallel (default: your'
                 ' number of cpu).'
        )
        subparser.add_argument(
            '--build-mode', '-b', choices=list(self.BUILD_MODES),
            default='dev',
            help='Selects a preset for build options.'
        )
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
            '--with-rpath', action='store_true', dest='with_rpath',
            default=True,
            help='Build libraries with run path options (by default)'
        )
        subparser.add_argument(
            '--without-rpath', action='store_false', dest='with_rpath',
            help='Do not build libraries with run path options'
        )

    def create_context(self, args):
        """
        Return a Langkit context (langkit.compile_context.CompileContext
        instance).

        This must be overriden by subclasses.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        raise NotImplementedError()

    @property
    def main_source_dirs(self):
        """
        Return a potentially empty set of source directories to use in the
        project file for mains. Source directories must be either absolute or
        relative to the language directory.

        :rtype: set[str]
        """
        return set()

    @property
    def main_programs(self):
        """
        Return the list of main programs to build in addition to the generated
        library. Subclasses should override this to add more main programs.

        :rtype: set[str]
        """
        return {'parse'}

    def parse_mains_list(self, mains):
        """
        Parse a comma-separated list of main programs. Raise a ValueError if
        one is not a supported main program.

        :param str mains: String to parse.
        :rtype: set[str]
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
    def lib_name(self):
        return self.context.ada_api_settings.lib_name

    def run(self, argv=None):
        parsed_args = self.args_parser.parse_args(argv)

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

        # If asked to, setup the exception hook as a last-chance handler to
        # invoke a debugger in case of uncaught exception.
        if parsed_args.debug:
            # Try to use IPython's debugger if it is available, otherwise
            # fallback to PDB.
            try:
                # noinspection PyPackageRequirements
                from IPython.core import ultratb
            except ImportError:
                ultratb = None  # To keep PyCharm happy...

                def excepthook(type, value, tb):
                    traceback.print_exception(type, value, tb)
                    pdb.post_mortem(tb)
                sys.excepthook = excepthook
            else:
                sys.excepthook = ultratb.FormattedTB(
                    mode='Verbose', color_scheme='Linux', call_pdb=1
                )
            del ultratb

        self.dirs.set_build_dir(parsed_args.build_dir)
        install_dir = getattr(parsed_args, 'install-dir', None)
        if install_dir:
            self.dirs.set_install_dir(install_dir)

        if getattr(parsed_args, 'list_warnings', False):
            WarningSet.print_list()
            return

        # noinspection PyBroadException
        try:
            parsed_args.func(parsed_args)

        except DiagnosticError:
            if parsed_args.debug:
                raise
            if parsed_args.verbosity.debug or parsed_args.full_error_traces:
                traceback.print_exc()
            print(col('Errors, exiting', Colors.FAIL), file=sys.stderr)
            sys.exit(1)

        except Exception as e:
            if parsed_args.debug:
                raise
            ex_type, ex, tb = sys.exc_info()

            # If we have a syntax error, we know for sure the last stack frame
            # points to the code that must be fixed. Otherwise, point to the
            # top-most stack frame that does not belong to Langkit.
            if e.args and e.args[0] == 'invalid syntax':
                loc = Location(e.filename, e.lineno)
            else:
                loc = extract_library_location(traceback.extract_tb(tb))
            with Context("", loc, "recovery"):
                check_source_language(False, str(e), do_raise=False)

            # Keep Langkit bug "pretty" for users: display the Python stack
            # trace only when requested.
            if parsed_args.verbosity.debug or parsed_args.full_error_traces:
                traceback.print_exc()

            print(col('Internal error! Exiting', Colors.FAIL), file=sys.stderr)
            sys.exit(1)

        finally:
            if parsed_args.profile:
                pr.disable()
                ps = pstats.Stats(pr)
                ps.dump_stats('langkit.prof')

    def set_context(self, parsed_args):
        self.context = self.create_context(parsed_args)

        # Set the extensions dir on the compile context
        self.context.extensions_dir = self.dirs.lang_source_dir(
            "extensions"
        )

    def do_generate(self, args):
        """
        Generate source code for the user language.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """

        def gnatpp(project_file, glob_pattern):
            """
            Helper function to pretty-print files from a GPR project.
            """

            # In general, don't abort if we can't find gnatpp or if gnatpp
            # crashes: at worst sources will not be pretty-printed, which is
            # not a big deal. `check_call` will emit warnings in this case.

            if self.verbosity.debug:
                self.check_call(args, 'Show pp path', ['which', 'gnatpp'],
                                abort_on_error=False)
                self.check_call(args, 'Show pp version',
                                ['gnatpp', '--version'],
                                abort_on_error=False)

            argv = ['gnatpp', '-P{}'.format(project_file),
                    '--syntax-only',
                    '--eol=lf']

            if self.verbosity.debug:
                argv.append('-v')

            self.check_call(
                args, 'Pretty-printing',
                argv + self.gpr_scenario_vars(args, 'prod', 'relocatable')
                + glob.glob(glob_pattern),
                abort_on_error=False
            )

        self.log_info(
            "Generating source for {}...".format(self.lib_name.lower()),
            Colors.HEADER
        )

        # Get source directories for the mains project file that are relative
        # to the generated project file (i.e. $BUILD_DIR/src/mains.gpr).
        main_source_dirs = {
            os.path.relpath(
                self.dirs.lang_source_dir(sdir),
                self.dirs.build_dir('src')
            )
            for sdir in self.main_source_dirs
        }

        self.context.emit(lib_root=self.dirs.build_dir(),
                          main_source_dirs=main_source_dirs,
                          main_programs=self.main_programs,
                          annotate_fields_types=args.annotate_fields_types,
                          check_only=args.check_only,
                          warnings=args.enabled_warnings,
                          no_property_checks=args.no_property_checks,
                          generate_unparser=args.generate_unparser,
                          generate_astdoc=not args.no_astdoc,
                          generate_gdb_hook=not args.no_gdb_hook,
                          plugin_passes=args.plugin_pass,
                          pretty_print=not args.no_pretty_print)

        if args.check_only:
            return

        if not args.no_langkit_support:
            self.do_generate_langkit_support(args)

        if not getattr(args, 'no_pretty_print', False):
            self.log_info(
                "Pretty-printing sources for {}...".format(
                    self.lib_name.lower()
                ),
                Colors.HEADER
            )
            gnatpp(
                self.dirs.build_dir('lib', 'gnat',
                                    '{}.gpr'.format(self.lib_name.lower())),
                self.dirs.build_dir('include', self.lib_name.lower(), '*.ad*')
            )
            gnatpp(self.dirs.build_dir('src', 'mains.gpr'),
                   self.dirs.build_dir('src', '*.ad*'))

        self.log_info("Generation complete!", Colors.OKGREEN)

    def what_to_build(self, args, is_library):
        """
        Determine what kind of build to perform.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.

        :param bool is_library: If true, build all modes (depending on modes
            enabled in `args`). Otherwise, use relocatable if allowed,
            static-pic otherwise and static otherwise.

        :return: Whether to build in 1) shared mode 2) static-pic mode 3)
            static mode. Only one is True when is_library is False.
        :rtype: (bool, bool, bool)
        """
        # Build libraries for all requested library types.
        #
        # Program are built only once, so build them as relocatable if
        # allowed, otherwise as static-pic if allowed, otherwise as static.
        build_shared = args.library_types.relocatable
        build_static_pic = (args.library_types.static_pic and
                            (is_library or not build_shared))
        build_static = (args.library_types.static and
                        (is_library or not build_static_pic))
        return (build_shared, build_static_pic, build_static)

    def gpr_scenario_vars(self, args, build_mode=None,
                          library_type='relocatable'):
        """
        Return the project scenario variables to pass to GPRbuild.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.

        :param str|None build_mode: Build mode to use. If left to None, use the
            one selected in `args`.

        :param str library_type: Library flavor to use. Must be "relocatable"
            or "static".
        """
        if build_mode is None:
            build_mode = args.build_mode

        result = ['-XBUILD_MODE={}'.format(build_mode),
                  '-XLIBRARY_TYPE={}'.format(library_type),
                  '-XGPR_BUILD={}'.format(library_type),
                  '-XXMLADA_BUILD={}'.format(library_type)]

        enable_build_warnings = getattr(args, 'enable_build_warnings', False)
        if enable_build_warnings:
            result.append(
                '-X{}_WARNINGS=true'.format(self.lib_name.upper())
            )

        return result

    def gprbuild(self, args, project_file, is_library, mains=None):
        """
        Run GPRbuild on a project file.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.

        :param str project_file: Path to the project file to pass to GPRbuild.

        :param bool is_library: See the "what_to_build" method.

        :param set[str]|None mains: If provided, list of main programs to
            build. By default, GPRbuild builds them all, so this arguments
            makes it possible to build only a subset of them.
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

        # Depending on where this is invoked, the "--gargs" option may not be
        # set. Don't call shlex.split with an empty input, otherwise it will
        # try to read something from stdin...
        gargs = getattr(args, 'gargs') or []
        gargs = sum((shlex.split(args) for args in gargs), [])

        def run(library_type):
            argv = list(base_argv)
            argv.extend(self.gpr_scenario_vars(args,
                                               library_type=library_type))
            if mains:
                argv.extend('{}.adb'.format(main) for main in mains)
            if Diagnostics.style == DiagnosticStyle.gnu_full:
                argv.append('-gnatef')
            argv.extend(gargs)
            self.check_call(args, 'Build', argv)

        build_shared, build_static_pic, build_static = self.what_to_build(
            args, is_library)
        if build_shared:
            run('relocatable')
        if build_static_pic:
            run('static-pic')
        if build_static:
            run('static')

    # noinspection PyIncorrectDocstring
    def gprinstall(self, args, project_file, is_library):
        """
        Run GPRinstall on a project file.

        See gprbuild for arguments description.

        :type args: argparse.Namespace
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

        def run(library_type):
            argv = list(base_argv)
            argv.append('--build-name={}'.format(library_type))
            argv.extend(self.gpr_scenario_vars(args, 'prod', library_type))
            self.check_call(args, 'Install', argv)

        # Install the static libraries first, so that in the resulting project
        # files, "static" is the default library type.
        build_shared, build_static_pic, build_static = self.what_to_build(
            args, is_library)
        if build_static:
            run('static')
        if build_static_pic:
            run('static-pic')
        if build_shared:
            run('relocatable')

    def do_build(self, args):
        """
        Build generated source code.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """

        # Build the generated library itself
        self.log_info("Building the generated source code", Colors.HEADER)

        lib_project = self.dirs.build_dir(
            'lib', 'gnat', '{}.gpr'.format(self.lib_name.lower())
        )
        self.gprbuild(args, lib_project, True)

        # Then build the main programs, if any
        disabled_mains = reduce(set.union, args.disable_mains, set())
        mains = (set()
                 if args.disable_all_mains else
                 self.main_programs - disabled_mains)
        if mains:
            self.log_info("Building the main programs...", Colors.HEADER)
            self.gprbuild(args, self.dirs.build_dir('src', 'mains.gpr'), False,
                          mains)

        # On Windows, shared libraries (DLL) are looked up in the PATH, just
        # like binaries (it's LD_LIBRARY_PATH on Unix). For this platform,
        # don't bother and just copy these DLL next to binaries.
        if os.name == 'nt':
            for dll in glob.glob(self.dirs.build_dir('lib', '*.dll')):
                shutil.copy(dll,
                            self.dirs.build_dir('bin', path.basename(dll)))

        self.log_info("Compilation complete!", Colors.OKGREEN)

    def do_make(self, args):
        """
        Generate and build in one command.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        self.do_generate(args)
        self.do_build(args)

    def do_install(self, args):
        """
        Install programs and libraries.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        lib_name = self.lib_name.lower()

        # Install libraries
        projects = ([] if args.no_langkit_support else
                    ['langkit_support.gpr']) + [lib_name + '.gpr']
        for prj in projects:
            self.gprinstall(args, self.dirs.build_dir('lib', 'gnat', prj),
                            True)

        # Install programs if they are all required.  If some are missing,
        # installing them is useless (this is in development mode).
        self.gprinstall(args, self.dirs.build_dir('src', 'mains.gpr'), False)

        # Install the remaining miscellaneous files
        for fpath in [
            os.path.join('include', lib_name + '.h'),
            os.path.join('share', lib_name, 'ast-types.html'),
            os.path.join('python', lib_name, '__init__.py'),
            os.path.join('python', 'setup.py'),
            os.path.join('ocaml', lib_name + '.ml'),
            os.path.join('ocaml', lib_name + '.mli'),
            os.path.join('ocaml', 'dune'),
            os.path.join('ocaml', 'dune-project'),
            os.path.join('ocaml', lib_name + '.opam'),
        ]:
            build_path = self.dirs.build_dir(fpath)
            install_path = self.dirs.install_dir(fpath)

            subdir = os.path.dirname(install_path)
            if not path.isdir(subdir):
                os.makedirs(subdir)

            shutil.copyfile(build_path, install_path)

    def do_setenv(self, args):
        """
        Unless --json is passed, display Bourne shell commands that setup
        environment in order to make the generated library available.
        Otherwise, return a JSON document that describe this environment.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        if args.json:
            result = {}

            def add_json(name, path):
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

    def lksp(self, args):
        return LangkitSupport(args.build_dir)

    def do_generate_langkit_support(self, args):
        """
        Generate the build tree and project file for Langkit_Support.
        """
        self.lksp(args).generate()

    def do_build_langkit_support(self, args):
        """
        Build Langkit_Support.
        """
        self.gprbuild(args, self.lksp(args).lksp_project_file, True)

    def do_install_langkit_support(self, args):
        """
        Install Langkit_Support.
        """
        self.gprinstall(args, self.lksp(args).lksp_project_file, True)

    def do_help(self, args):
        """
        Print usage and exit.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        del args  # Unused in this implementation
        self.args_parser.print_help()

    def setup_environment(self, add_path):
        add_path('PATH', self.dirs.build_dir('bin'))
        add_path('C_INCLUDE_PATH', self.dirs.build_dir('include'))

        libs = ['langkit_support']
        if self.context:
            libs.append(self.lib_name.lower())
        for lib in libs:
            add_path('LIBRARY_PATH',
                     self.dirs.build_dir('lib', lib + '.static'))
            add_path('LD_LIBRARY_PATH',
                     self.dirs.build_dir('lib', lib + '.relocatable'))
            add_path('PATH',
                     self.dirs.build_dir('lib', lib + '.relocatable'))

        add_path('GPR_PROJECT_PATH', self.dirs.build_dir('lib', 'gnat'))
        add_path('PYTHONPATH', self.dirs.build_dir('python'))
        add_path('PYTHONPATH', self.dirs.lang_source_dir('python_src'))

    def derived_env(self):
        """
        Return a copy of the environment after an update using
        setup_environment.
        """
        env = dict(os.environ)

        def add_path(name, p):
            if isinstance(name, unicode):
                name = name.encode('ascii')
            if isinstance(p, unicode):
                p = p.encode('ascii')
            env[name] = path.pathsep.join(keep([p, env.get(name, b'')]))

        self.setup_environment(add_path)
        return env

    def write_setenv(self, output_file=sys.stdout):
        """
        Display Bourne shell commands that setup environment in order to make
        the generated library available.

        :param file output_file: File to which this should write the shell
            commands.
        """
        def add_path(name, path):
            output_file.write(
                '{name}={path}"{sep}${name}"; export {name}\n'.format(
                    name=name, path=pipes.quote(path),
                    # On Cygwin, PATH keeps the Unix syntax instead of using
                    # the Window path separator.
                    sep=':' if name == 'PATH' else os.path.pathsep,
                )
            )
        self.setup_environment(add_path)

    def check_call(self, args, name, argv, env=None, abort_on_error=True):
        """
        Log and run a command with a derived environment.

        Return True if the command completes with success. Otherwise, emit an
        error message and, if `abort_on_error` is true, exit with an error
        status code.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        :param str name: Name of the process to run, use for error message
            formatting only.
        :param list[str] argv: Arguments for the command to run.
        :param dict[str, str]|None env: Environment to use for the command to
            run. If None, use self.derived_env().
        :param bool abort_on_error: If the command stops with an error, exit
            ourselves.
        """
        self.log_exec(args, argv)
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

    def log_exec(self, args, argv):
        """
        If verbosity level is debug, log a command we are about to execute.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        :param list[str] argv: Arguments for the command to log.
        """
        if args.verbosity.debug:
            printcol('Executing: {}'.format(
                ' '.join(pipes.quote(arg) for arg in argv)
            ), Colors.CYAN)

    def log_info(self, msg, color):
        """
        If verbosity level is info, log a message with given color.
        """
        if self.verbosity.info:
            printcol(msg, color)
