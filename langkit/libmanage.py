from __future__ import absolute_import

import argparse
import glob
import inspect
import os.path
import pdb
import pipes
import shutil
import subprocess
import sys

from langkit.compile_context import Verbosity
from langkit.diagnostics import Diagnostics, DiagnosticError
from langkit.utils import Colors, printcol


class Directories(object):
    """
    Helper class used to get various path in source/build/install trees.
    """

    def __init__(self, lang_source_dir=None, build_dir=None, install_dir=None):
        self.root_lang_source_dir = lang_source_dir
        self.root_langkit_source_dir = os.path.dirname(
            os.path.abspath(__file__)
        )
        self.root_build_dir = build_dir
        self.root_install_dir = install_dir

    def set_build_dir(self, build_dir):
        self.root_build_dir = os.path.abspath(build_dir)

    def set_install_dir(self, install_dir):
        self.root_install_dir = os.path.abspath(install_dir)

    def lang_source_dir(self, *args):
        return os.path.join(self.root_lang_source_dir, *args)

    def build_dir(self, *args):
        return os.path.join(self.root_build_dir, *args)

    def install_dir(self, *args):
        return os.path.join(self.root_install_dir, *args)

    def langkit_source_dir(self, *args):
        """
        Build and return the path for ``args`` under the root source directory
        for langkit.

        :param list[str] args: The path components, same semantics as in
            os.path.join.
        """
        return os.path.join(self.root_langkit_source_dir, *args)


class Coverage(object):
    """
    Guard object used to compute code coverage (optionally).
    """

    def __init__(self, dirs):
        self.dirs = dirs

        import coverage
        self.cov = coverage.coverage(
            branch=True,
            source=[
                self.dirs.langkit_source_dir(),
                self.dirs.lang_source_dir(),
            ],
            omit=[
                self.dirs.langkit_source_dir('libmanage.py'),
                self.dirs.lang_source_dir('manage.py'),
                self.dirs.lang_source_dir('env.py'),
            ],
        )

        self.cov.exclude('def __repr__')
        self.cov.exclude('raise NotImplementedError()')
        self.cov.exclude('assert False')

    def start(self):
        self.cov.start()

    def stop(self):
        self.cov.stop()

    def generate_report(self):
        self.cov.html_report(
            directory=self.dirs.build_dir('coverage'),
            ignore_errors=True
        )


def get_cpu_count():
    # The "multiprocessing" module is not available on GNATpython's
    # distribution for PPC AIX and the "cpu_count" is not available on Windows:
    # give up on default parallelism on these platforms.
    try:
        import multiprocessing
        return multiprocessing.cpu_count()
    except (ImportError, NotImplementedError):
        return 1


class ManageScript(object):

    BUILD_MODES = ('dev', 'prod')

    def __init__(self):

        self.dirs = Directories(
            # It is assumed that manage.py is at the root of the language
            # definition source directory.
            lang_source_dir=os.path.dirname(
                os.path.abspath(inspect.getfile(self.__class__))
            )
        )
        Diagnostics.set_lang_source_dir(self.dirs.lang_source_dir())

        ########################
        # Main argument parser #
        ########################

        self.args_parser = args_parser = argparse.ArgumentParser(
            description='General manager to handle actions relative to'
                        ' building/testing libadalang'
        )
        self.subparsers = subparsers = args_parser.add_subparsers()

        args_parser.add_argument(
            '--build-dir', default='build',
            help=(
                'Directory to use for generated source code and binaries. By'
                ' default, use "build" in the current directory.'
            )
        )
        args_parser.add_argument(
            '--enable-static', action='store_true',
            help='Enable the generation of static libraries (default:'
                 ' disabled)'
        )
        args_parser.add_argument(
            '--disable-static', action='store_false', dest='enable_static',
            help='Disable the generation of static libraries'
        )
        args_parser.add_argument(
            '--enable-shared', action='store_true', default=True,
            help='Enable the generation (and testing) of shared libraries'
                 ' (default: enabled)'
        )
        args_parser.add_argument(
            '--disable-shared', action='store_false', dest='enable_shared',
            help='Disable the generation (and testing) of shared libraries'
        )
        args_parser.add_argument(
            '--bindings', '-b', nargs='+', choices=('python', ),
            default=['python'],
            help='Bindings to generate (by default: only Python)'
        )
        args_parser.add_argument(
            '--verbosity', '-v', nargs='?',
            type=Verbosity,
            choices=Verbosity.choices(),
            default=Verbosity('info'),
            const=Verbosity('debug'),
            help='Verbosity level'
        )

        # Don't enable this by default so that errors will not make automated
        # tasks hang.
        args_parser.add_argument(
            '-g', '--debug', action='store_true',
            help='In case of internal error or diagnostic error, run a'
                 ' post-motrem PDB session'
        )

        def create_parser(fn):
            """
            Create a subparser from a function. Uses the name and the docstring
            of the function to document the subparsers.

            :param (ManageScript, Namespace) -> None fn: The function to use.
            :rtype: argparse.ArgumentParser
            """
            p = subparsers.add_parser(
                # Take the name of the function without the do_ prefix
                fn.__name__.replace('do_', ''),

                # Take the first paragraph of the function's documentation as
                # help.
                help=fn.__doc__.split('\n\n')[0].strip()
            )
            p.set_defaults(func=fn)
            return p

        ########
        # Help #
        ########

        self.help_parser = create_parser(self.do_help)

        ############
        # Generate #
        ############

        self.generate_parser = generate_parser = create_parser(
            self.do_generate
        )
        self.add_generate_args(generate_parser)

        #########
        # Build #
        #########

        self.build_parser = build_parser = create_parser(self.do_build)
        self.add_build_args(build_parser)

        ########
        # Make #
        ########

        self.make_parser = make_parser = create_parser(self.do_make)
        self.add_generate_args(make_parser)
        self.add_build_args(make_parser)

        ###########
        # Install #
        ###########

        self.install_parser = install_parser = create_parser(self.do_install)
        install_parser.add_argument(
            'install-dir',
            help='Installation directory.'
        )

        ##########
        # Setenv #
        ##########

        self.setenv_parser = create_parser(self.do_setenv)

        # The create_context method will create the context and set it here
        # only right before executing commands so that coverage computation
        # will apply to create_context.
        self.context = None

    def add_generate_args(self, subparser):
        """
        Add arguments to tune code generation to "subparser".

        :type subparser: argparse.ArgumentParser
        """
        subparser.add_argument(
            '--coverage', '-C', action='store_true',
            help='Compute code coverage for the code generator'
        )
        subparser.add_argument(
            '--pretty-print', '-p', action='store_true',
            help='Pretty-print generated source code'
        )
        subparser.add_argument(
            '--export-private-fields', action='store_true',
            help='Export all fields and properties as if they were all public'
        )

    def add_build_args(self, subparser):
        """
        Add arguments to tune code compilation to "subparser".

        :type subparser: argparse.ArgumentParser
        """
        subparser.add_argument(
            '--jobs', '-j', type=int, default=get_cpu_count(),
            help='Number of parallel jobs to spawn in parallel '
                 '(default: your number of cpu)'
        )
        subparser.add_argument(
            '--build-mode', '-b', choices=list(self.BUILD_MODES),
            default='dev',
            help='Selects a preset for build options'
        )
        subparser.add_argument(
            '--cargs', nargs='*', default=[],
            help='Options to pass as "-cargs" to GPRbuild'
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
    def lib_name(self):
        return self.context.ada_api_settings.lib_name

    def run(self):
        parsed_args = self.args_parser.parse_args()
        self.dirs.set_build_dir(parsed_args.build_dir)
        install_dir = getattr(parsed_args, 'install-dir', None)
        if install_dir:
            self.dirs.set_install_dir(install_dir)

        # Compute code coverage in the code generator if asked to
        if parsed_args.func == self.do_generate and parsed_args.coverage:
            try:
                import coverage
                del coverage
            except Exception as exc:
                import traceback
                print >> sys.stderr, 'Coverage not available:'
                traceback.print_exc(exc)
                sys.exit(1)

            cov = Coverage(self.dirs)
            cov.start()
        else:
            cov = None

        self.context = self.create_context(parsed_args)
        self.context.export_private_fields = getattr(
            parsed_args, 'export_private_fields', False
        )

        # Set the extensions dir on the compile context
        self.context.extensions_dir = self.dirs.lang_source_dir("extensions")

        try:
            parsed_args.func(parsed_args)
        except DiagnosticError:
            print >> sys.stderr, "Errors, exiting"
            if parsed_args.debug:
                pdb.post_mortem()
            sys.exit(1)
        except:
            if parsed_args.debug:
                pdb.post_mortem()
            raise

        if cov is not None:
            cov.stop()
            cov.generate_report()

    def do_generate(self, args):
        """
        Generate source code for libadalang.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """

        if args.verbosity.info:
            printcol("Generating source for libadalang ...", Colors.HEADER)
        self.context.emit(file_root=self.dirs.build_dir())

        def gnatpp(project_file, glob_pattern):
            try:
                subprocess.check_call([
                    'gnatpp',
                    '-P{}'.format(project_file),
                    '-XLIBRARY_TYPE=relocatable',
                    '-rnb',
                ] + glob.glob(glob_pattern), env=self.derived_env())
            except subprocess.CalledProcessError as exc:
                print >> sys.stderr, 'Pretty-printing failed: {}'.format(exc)
                sys.exit(1)

        if hasattr(args, 'pretty_print') and args.pretty_print:
            if args.verbosity.info:
                printcol("Pretty-printing sources for Libadalang ...",
                         Colors.HEADER)
            gnatpp(
                self.dirs.build_dir('lib', 'gnat',
                                    '{}.gpr'.format(self.lib_name.lower())),
                self.dirs.build_dir('include', self.lib_name.lower(), '*.ad*')
            )
            gnatpp(self.dirs.build_dir('src', 'parse.gpr'),
                   self.dirs.build_dir('src', '*.ad*'))

        if args.verbosity.info:
            printcol("Generation complete!", Colors.OKGREEN)

    def gprbuild(self, args, project_file, is_library):
        """
        Run GPRbuild on a project file.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        :param str project_file: Path to the project file to pass to GPRbuild.
        :param bool is_library: If true, build both relocatable and static
            libraries (depending on modes enabled in "args"). Otherwise, use
            relocatable if available or static mode otherwise.
        """

        cargs = []
        # Depending on where this is invoked, the "cargs" option may not be set
        if hasattr(args, 'cargs'):
            cargs.extend(args.cargs)

        env = self.derived_env()

        def run(library_type):
            try:
                subprocess.check_call([
                    'gprbuild', '-m', '-p', '-j{}'.format(args.jobs),
                    '-P{}'.format(project_file),
                    '-XBUILD_MODE={}'.format(args.build_mode),
                    '-XLIBRARY_TYPE={}'.format(library_type),
                    '-XLIBLANG_SUPPORT_EXTERNALLY_BUILT=false',
                    '-X{}_EXTERNALLY_BUILT=false'.format(
                        self.lib_name.upper()
                    ),
                    '-cargs',
                ] + cargs, env=env)
            except subprocess.CalledProcessError as exc:
                print >> sys.stderr, 'Build failed: {}'.format(exc)
                sys.exit(1)

        # The basic principle is: build shared unless disabled and build static
        # unless disabled. But for programs, we can build only one mode: in
        # this case, shared has priority over static.
        build_shared = args.enable_shared
        build_static = (args.enable_static and
                        (is_library or not build_shared))
        if build_shared:
            run('relocatable')
        if build_static:
            run('static')

    def do_build(self, args):
        """
        Build generated source code.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """

        if args.verbosity.info:
            printcol("Building the generated source code ...", Colors.HEADER)
        lib_project = self.dirs.build_dir(
            'lib', 'gnat', '{}.gpr'.format(self.lib_name.lower())
        )
        self.gprbuild(args, lib_project, True)

        if args.verbosity.info:
            printcol("Building the interactive test main ...", Colors.HEADER)
        self.gprbuild(args, self.dirs.build_dir('src', 'parse.gpr'), False)

        # On Windows, shared libraries (DLL) are looked up in the PATH, just
        # like binaries (it's LD_LIBRARY_PATH on Unix). For this platform,
        # don't bother and just copy these DLL next to binaries.
        if os.name == 'nt':
            for dll in glob.glob(self.dirs.build_dir('lib', '*.dll')):
                shutil.copy(dll,
                            self.dirs.build_dir('bin', os.path.basename(dll)))

        if args.verbosity.info:
            printcol("Compilation complete!", Colors.OKGREEN)

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
        del args  # Unused in this implementation

        for subdir in ('bin', 'include', 'lib', 'share', 'python'):
            install_dir = self.dirs.install_dir(subdir)
            if os.path.isdir(install_dir):
                shutil.rmtree(install_dir)
            assert not os.path.exists(install_dir)
            shutil.copytree(
                self.dirs.build_dir(subdir),
                self.dirs.install_dir(subdir)
            )

    def do_setenv(self, args):
        """
        Display Bourne shell commands that setup environment in order to make
        libadalang available.

        :param argparse.Namespace args: The arguments parsed from the command
            line invocation of manage.py.
        """
        del args  # Unused in this implementation

        def add_path(name, path):
            print('{name}={path}:${name}; export {name}'.format(
                name=name, path=pipes.quote(path)
            ))
        self.setup_environment(add_path)

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
        add_path('LIBRARY_PATH', self.dirs.build_dir('lib'))
        add_path('LD_LIBRARY_PATH', self.dirs.build_dir('lib'))
        add_path('GPR_PROJECT_PATH', self.dirs.build_dir('lib', 'gnat'))
        add_path('PYTHONPATH', self.dirs.build_dir('python'))
        add_path('PYTHONPATH', self.dirs.lang_source_dir('python_src'))

    def derived_env(self):
        """
        Return a copy of the environment after an update using
        setup_environment.
        """
        env = dict(os.environ)

        def add_path(name, path):
            old = env.get(name, '')
            env[name] = ('{}{}{}'.format(path, os.path.pathsep, old)
                         if old else path)

        self.setup_environment(add_path)
        return env
