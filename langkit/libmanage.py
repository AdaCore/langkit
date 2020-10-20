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

from langkit.compile_context import UnparseScript, Verbosity
from langkit.diagnostics import (
    Context, DiagnosticError, DiagnosticStyle, Diagnostics, Location,
    WarningSet, check_source_language, extract_library_location
)
from langkit.packaging import Packager
from langkit.utils import (Colors, LibraryTypes, Log, col, format_setenv,
                           get_cpu_count, printcol)


class Directories:
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


class ManageScript:

    BUILD_MODES = ('dev', 'prod')

    ENABLE_BUILD_WARNINGS_DEFAULT = False
    """
    Whether warnings to build the generated library are enabled by default.
    """

    def __init__(self):
        self.dirs = Directories(
            # It is assumed that manage.py is at the root of the language
            # definition source directory, if no source directory is explicitly
            # passed.
            lang_source_dir=path.dirname(
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
        self.subparsers = subparsers = args_parser.add_subparsers()

        args_parser.add_argument(
            '--build-dir', default='build',
            help='Directory to use for generated source code and binaries. By'
                 ' default, use "build" in the current directory.'
        )
        LibraryTypes.add_option(args_parser)
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
            '--no-ada-api', action='store_true',
            help='Do not generate units to provide an Ada API, and disable the'
                 ' generation of mains.'
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

        self.add_build_mode_arg(install_parser)
        install_parser.add_argument(
            '--force', '-f', action='store_true',
            help='Force installation, overwrite files.'
        )
        install_parser.add_argument(
            '--disable-all-mains', action='store_true',
            help='Do not install main program.'
        )

        ##########
        # Setenv #
        ##########

        self.setenv_parser = create_parser(self.do_setenv, True)

        self.add_build_mode_arg(self.setenv_parser)
        self.setenv_parser.add_argument(
            '--json', '-J', action='store_true',
            help='Output necessary env keys to JSON.'
        )

        #######################
        # Create Python wheel #
        #######################

        self.create_wheel_parser = create_parser(self.do_create_wheel, True)
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
            '--report-unused-doc-entries', action='store_true', default=False,
            help='Emit warnings for unused documentation entries .'
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

    def add_build_mode_arg(self, subparser):
        subparser.add_argument(
            '--build-mode', '-b', choices=list(self.BUILD_MODES),
            default='dev',
            help='Selects a preset for build options.'
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
        ext_dir = self.context.extensions_dir
        if ext_dir is not None:
            mains_dir = os.path.join(ext_dir, "mains")
            if os.path.isdir(mains_dir):
                return {mains_dir}

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

        self.no_ada_api = parsed_args.no_ada_api

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
            with Context(loc):
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

        # The call to "check_call" in gnatpp does a setenv, so we need a build
        # mode. Given that do_make (which already have a build mode) calls
        # do_generate, we must provide a dummy build mode iff there isn't
        # already a build mode. The setenv is required for gnatpp to see the
        # project files, i.e. access to build artifacts is not requested, so we
        # can provide a dummy build mode.
        if not getattr(args, 'build_mode', None):
            args.build_mode = self.BUILD_MODES[0]

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
                argv + self.gpr_scenario_vars(args, 'relocatable')
                + glob.glob(glob_pattern),
                abort_on_error=False
            )

        self.log_info(
            "Generating source for {}...".format(self.lib_name.lower()),
            Colors.HEADER
        )

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

        main_programs = ([] if self.no_ada_api else self.main_programs)

        self.context.emit(
            lib_root=self.dirs.build_dir(),
            main_source_dirs=main_source_dirs,
            main_programs=main_programs,
            annotate_fields_types=args.annotate_fields_types,
            check_only=args.check_only,
            warnings=args.enabled_warnings,
            report_unused_documentation_entries=args.report_unused_doc_entries,
            no_property_checks=args.no_property_checks,
            generate_ada_api=not args.no_ada_api,
            generate_unparser=args.generate_unparser,
            generate_gdb_hook=not args.no_gdb_hook,
            plugin_passes=args.plugin_pass,
            pretty_print=args.pretty_print,
            coverage=args.coverage,
            relative_project=args.relative_project,
            unparse_script=args.unparse_script
        )

        if args.check_only:
            return

        if getattr(args, 'pretty_print', False):
            self.log_info(
                "Pretty-printing sources for {}...".format(
                    self.lib_name.lower()
                ),
                Colors.HEADER
            )
            gnatpp(self.lib_project, self.dirs.build_dir('src', '*.ad*'))
            gnatpp(self.mains_project,
                   self.dirs.build_dir('src-mains', '*.ad*'))

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
        lt = args.library_types
        if is_library:
            # Build libraries for all requested library types
            build_shared = lt.relocatable
            build_static_pic = lt.static_pic
            build_static = lt.static
        else:
            # Program are built only once, so build them as relocatable if
            # allowed, otherwise as static-pic if allowed, otherwise as static.
            build_shared = build_static_pic = build_static = False
            if lt.relocatable:
                build_shared = True
            elif lt.static_pic:
                build_static_pic = True
            elif lt.static:
                build_static = True
        return (build_shared, build_static_pic, build_static)

    def gpr_scenario_vars(self, args, library_type='relocatable'):
        """
        Return the project scenario variables to pass to GPRbuild.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.

        :param str library_type: Library flavor to use. Must be "relocatable"
            or "static".
        """
        result = ['-XBUILD_MODE={}'.format(args.build_mode),
                  '-XLIBRARY_TYPE={}'.format(library_type),
                  '-XGPR_BUILD={}'.format(library_type),
                  '-XXMLADA_BUILD={}'.format(library_type)]

        enable_build_warnings = getattr(args, 'enable_build_warnings', False)
        if enable_build_warnings:
            result.append(
                '-X{}_WARNINGS=true'.format(self.lib_name.upper())
            )

        return result

    def gprbuild(self, args, project_file, is_library, mains=None,
                 obj_dirs=[]):
        """
        Run GPRbuild on a project file.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.

        :param str project_file: Path to the project file to pass to GPRbuild.

        :param bool is_library: See the "what_to_build" method.

        :param list[str] obj_dirs: List of paths (relative to the project file
            directory) to the object directory where gprbuild creates the
            "*.lexch" files. We will remove all such files before each gprbuild
            run.

            This allows us to workaround a GPRbuild bug (see SB18-035).
            Library types share the same object directory, however GPRbuild
            uses a file in the object directory (*.lexch) to know if the
            library must be rebuilt. Not removing it will make it skip the
            "static" build after the "static-pic" was built. So we remove it.

        :param set[str]|None mains: If provided, list of main programs to
            build. By default, GPRbuild builds them all, so this arguments
            makes it possible to build only a subset of them.
        """
        lexch_patterns = [os.path.join(os.path.dirname(project_file),
                                       obj_dir, '*.lexch')
                          for obj_dir in obj_dirs]

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
            # Remove the "*.lexch" file
            for pattern in lexch_patterns:
                files = glob.glob(pattern)
                for f in files:
                    self.log_debug('Removing {}'.format(f), Colors.CYAN)
                    os.remove(f)
                if not files:
                    self.log_debug('No *.lexch file to remove from {}'
                                   .format(pattern), Colors.CYAN)

            argv = list(base_argv)
            argv.extend(
                self.gpr_scenario_vars(args, library_type=library_type)
            )
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
            argv.extend(self.gpr_scenario_vars(args, library_type))
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

    def do_build(self, args):
        """
        Build generated source code.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """

        # Build the generated library itself
        self.log_info("Building the generated source code", Colors.HEADER)

        obj_dirs = [self.dirs.build_dir('obj', args.build_mode)]
        self.gprbuild(args, self.lib_project, is_library=True,
                      obj_dirs=obj_dirs)

        # Then build the main programs, if any
        if not self.no_ada_api:
            disabled_mains = reduce(set.union, args.disable_mains, set())
            mains = (set()
                     if args.disable_all_mains else
                     self.main_programs - disabled_mains)
            if mains:
                self.log_info("Building the main programs...", Colors.HEADER)
                self.gprbuild(args, self.mains_project,
                              is_library=False, mains=mains)

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

        self.gprinstall(args, self.lib_project, is_library=True)

        # Install programs if they are all required
        if not args.disable_all_mains:
            self.gprinstall(args, self.mains_project, is_library=False)

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

        # Install the C header for the generated library in "include".
        #
        # TODO (TA20-017: gprinstall bug): remove this (see corresponing TODO
        # in emitter.py.
        header_filename = "{}.h".format(self.context.c_api_settings.lib_name)
        shutil.copyfile(
            self.dirs.build_dir(header_filename),
            self.dirs.install_dir("include", header_filename)
        )

        # Install the remaining miscellaneous files
        for fpath in [
            os.path.join('python', lib_name, '*.py'),
            os.path.join('python', 'setup.py'),
            os.path.join('ocaml', lib_name + '.ml'),
            os.path.join('ocaml', lib_name + '.mli'),
            os.path.join('ocaml', 'dune'),
            os.path.join('ocaml', 'dune-project'),
            os.path.join('ocaml', lib_name + '.opam'),
        ]:
            install_path = os.path.dirname(self.dirs.install_dir(fpath))
            if not path.isdir(install_path):
                os.makedirs(install_path)

            for f in glob.glob(self.dirs.build_dir(fpath)):
                shutil.copyfile(
                    f, os.path.join(install_path, os.path.basename(f))
                )

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

            self.setup_environment(args.build_mode, add_json)
            print(json.dumps(result))
        else:
            self.write_setenv(args.build_mode)

    def do_create_wheel(self, args):
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

    def do_help(self, args):
        """
        Print usage and exit.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        del args  # Unused in this implementation
        self.args_parser.print_help()

    def setup_environment(self, build_mode, add_path):
        P = self.dirs.build_dir

        # Make the project file available
        add_path("GPR_PROJECT_PATH", P())

        # Make the scripts and mains available
        add_path("PATH", P("scripts"))
        add_path("PATH", P("obj-mains"))

        # Make the shared lib available, regardless of the operating system
        shared_dir = P("lib", "relocatable", build_mode)
        add_path("LD_LIBRARY_PATH", shared_dir)
        add_path("DYLD_LIBRARY_PATH", shared_dir)
        add_path("PATH", shared_dir)

        # Make the Python bindings and the associated Mypy type hints available
        add_path("PYTHONPATH", P("python"))
        add_path("MYPYPATH", P("python"))

        # Make the C header available.
        #
        # TODO (TA20-017: gprinstall bug): remove this (see corresponing TODO
        # in emitter.py.
        add_path("C_INCLUDE_PATH", P())

    def derived_env(self, build_mode):
        """
        Return a copy of the environment after an update using
        setup_environment.
        """
        env = dict(os.environ)

        def add_path(name, p):
            env[name] = path.pathsep.join(keep([p, env.get(name, '')]))

        self.setup_environment(build_mode, add_path)
        return env

    def write_setenv(self, build_mode, output_file=sys.stdout):
        """
        Display Bourne shell commands that setup environment in order to make
        the generated library available.

        :param file output_file: File to which this should write the shell
            commands.
        """
        def add_path(name, path):
            output_file.write(format_setenv(name, path) + '\n')
        self.setup_environment(build_mode, add_path)

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
            env = self.derived_env(args.build_mode)
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

    def log_debug(self, msg, color):
        """
        If verbosity level is debug, log a message with given color.
        """
        if self.verbosity.debug:
            printcol(msg, color)
