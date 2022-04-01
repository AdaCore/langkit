#! /usr/bin/env python

from argparse import ArgumentParser, Namespace, _SubParsersAction
import glob
import os
import os.path as P
from pathlib import PurePath
import shutil
import subprocess
import sys
from typing import Callable, List, Optional

from langkit.packaging import Packager
from langkit.utils import (LibraryTypes, add_to_path, format_setenv,
                           get_cpu_count, parse_cmdline_args)


LANGKIT_ROOT = PurePath(P.dirname(P.realpath(__file__)))
SUPPORT_ROOT = LANGKIT_ROOT / "langkit" / "support"
SUPPORT_GPR = str(SUPPORT_ROOT / "langkit_support.gpr")
SIGSEGV_HANDLER_ROOT = LANGKIT_ROOT / "sigsegv_handler"
SIGSEGV_HANDLER_GPR = SIGSEGV_HANDLER_ROOT / "langkit_sigsegv_handler.gpr"
LKT_LIB_ROOT = LANGKIT_ROOT / "contrib" / "lkt"
PYTHON_LIB_ROOT = LANGKIT_ROOT / "contrib" / "python"

LIB_ROOTS = {"python": PYTHON_LIB_ROOT, "lkt": LKT_LIB_ROOT}


def selected_libs(args: Namespace) -> List[str]:
    """
    Return the name of the libs on which to operate (--lib command line
    argument).
    """
    return args.lib or ["python", "lkt"]


def selected_lib_roots(args: Namespace) -> List[str]:
    """
    Return paths to the libraries on which to operate (--lib command line
    argument).
    """
    return [LIB_ROOTS[lib] for lib in selected_libs(args)]


def create_subparser(
    subparsers: _SubParsersAction,
    fn: Callable[..., None],
    *,
    with_jobs: bool = False,
    with_no_lksp: bool = False,
    with_gargs: bool = False,
    with_build_dir: bool = False,
    with_libs: bool = False,
    with_no_mypy: bool = False,
    accept_unknown_args: bool = False,
) -> ArgumentParser:
    """
    Create a subparser with given ``fn`` as func. Extract doc and name from
    the function.

    :param bool with_jobs: Whether to create the --jobs/-j option.
    :param bool with_no_lksp: Whether to create the --no-langkit-support
        option.
    :param bool with_gargs: Whether to create the --gargs option.
    :param bool with_build_dir: Whether to create the --build-dir option.
    :param bool with_libs: Whether to create the --lib option.
    :param bool with_no_mypy: Whether to create the --no-mypy option.
    """
    subparser = subparsers.add_parser(
        name=fn.__name__.replace('_', '-'),
        help=fn.__doc__,
        add_help=not accept_unknown_args
    )

    subparser.add_argument(
        "--build-mode", "-b", choices=("dev", "prod"), default="dev",
        help="Select a preset for build options."
    )
    LibraryTypes.add_option(subparser)
    if with_jobs:
        subparser.add_argument(
            "--jobs", "-j", type=int, default=get_cpu_count(),
            help="Number of parallel jobs to spawn in parallel (default: your"
                 " number of cpu)."
        )
    if with_no_lksp:
        subparser.add_argument(
            "--no-langkit-support", action="store_true",
            help="Assume that Langkit_Support is already built and installed."
                 " We rebuild it by default, for the convenience of"
                 " developers."
        )
    if with_gargs:
        subparser.add_argument(
            '--gargs', action='append',
            help='Options appended to GPRbuild invocations.'
        )
    if with_build_dir:
        subparser.add_argument(
            '--build-dir',
            help='Use a non-default build directory. This allows out-of-tree'
                 ' builds.'
        )
    if with_libs:
        subparser.add_argument(
            "--lib", "-l", choices=("python", "lkt"), action="append",
            help="Select which libraries on which to operate. By default, work"
                 " on all libraries."
        )
    if with_no_mypy:
        subparser.add_argument(
            "--no-mypy", action="store_true",
            help="Whether to disable type-checking with mypy."
        )

    def wrapper(args: Namespace, rest: List[str]):
        if len(rest) > 0:
            print("ERROR - unhandled command line arguments: {}".format(
                ", ".join(rest)
            ))
            sys.exit(1)
        fn(args)

    subparser.set_defaults(func=fn if accept_unknown_args else wrapper)

    return subparser


def build_langkit_support(args: Namespace) -> None:
    """
    Build Langkit_Support.
    """
    build_dir = PurePath(args.build_dir) if args.build_dir else SUPPORT_ROOT

    base_argv = [
        "gprbuild", "-p", f"-j{args.jobs}", f"-XBUILD_MODE={args.build_mode}",
    ]
    if args.build_dir:
        base_argv.extend([f"--relocate-build-tree={build_dir}"])

    gargs = parse_cmdline_args(args.gargs)

    # In order to avoid building the library once per library kind (static,
    # static-pic and relocatable), langkit_support.gpr uses the same object
    # directory for every library kind. This optimization is valid only if we
    # remove "*.lexch" files in the object directory between each call to
    # gprbuild.
    lexch_pattern = str(build_dir / "obj" / args.build_mode / "*.lexch")

    for library_type in args.library_types.names:
        for lexch in glob.glob(lexch_pattern):
            os.remove(lexch)
        subprocess.check_call(
            base_argv
            + ["-P", SUPPORT_GPR, f"-XLIBRARY_TYPE={library_type}"]
            + gargs
        )

    subprocess.check_call(base_argv + ["-P", SIGSEGV_HANDLER_GPR] + gargs)


def setenv_langkit_support(args: Namespace) -> None:
    """
    Setenv for Langkit_Support.
    """
    build_dir = PurePath(args.build_dir) if args.build_dir else SUPPORT_ROOT

    # Make the "langkit_support.gpr" available to GPRbuild
    print(format_setenv("GPR_PROJECT_PATH", str(SUPPORT_ROOT)))

    # Make the shared library for Langkit_Support available to the dynamic
    # linker.
    dynamic_lib_dir = str(build_dir / "lib" / "relocatable" / args.build_mode)
    print(format_setenv("PATH", dynamic_lib_dir))
    print(format_setenv("LD_LIBRARY_PATH", dynamic_lib_dir))

    # Make the shared lib for the sigsegv handler available for OCaml on
    # GNU/Linux.
    print(format_setenv("LD_LIBRARY_PATH", str(SIGSEGV_HANDLER_ROOT / "lib")))


def install_langkit_support(args: Namespace) -> None:
    """
    Install the Langkit_Support project.
    """
    base_argv = [
        "gprinstall", "-P", SUPPORT_GPR, "-p",
        f"-XBUILD_MODE={args.build_mode}",
        f"--prefix={args.prefix}",
        "--build-var=LIBRARY_TYPE",
        "--build-var=LANGKIT_SUPPORT_LIBRARY_TYPE",
        "--sources-subdir=include/langkit_support"
    ]
    if args.build_dir:
        base_argv.extend([f"--relocate-build-tree={args.build_dir}"])

    # Install the static libraries first, so that in the resulting project
    # files, "static" is the default library type.
    lib_types = args.library_types.names
    for library_type in ("static", "static-pic", "relocatable"):
        if library_type in lib_types:
            subprocess.check_call(base_argv + [
                f"-XLIBRARY_TYPE={library_type}",
                f"--build-name={library_type}"
            ])


def package_deps(args: Namespace) -> None:
    """
    Bundle all dependencies to complete GNAT Pro.
    """
    p = Packager.from_args(args)
    p.package_deps(getattr(args, "package-dir"))


def package_std_dyn(args: Namespace) -> None:
    """
    Bundle all dependencies to create standalone packages.
    """
    pass
    p = Packager.from_args(args)
    pkg_dir = getattr(args, "package-dir")
    p.package_standalone_dyn(pkg_dir)
    p.package_langkit_support_dyn(pkg_dir)


def setenv(args: Namespace) -> None:
    """
    Print shell commands to add Libpythonlang and Liblktlang to the
    environment.
    """
    if not args.no_langkit_support:
        setenv_langkit_support(args)

    for cwd in selected_lib_roots(args):
        subprocess.check_call(
            [sys.executable,
             "./manage.py",
             "setenv",
             f"--build-mode={args.build_mode}"],
            cwd=cwd
        )


def make(args: Namespace) -> None:
    """
    Generate and build Libpythonlang and Liblktlang.
    """

    # Unless specifically asked to ignore Langkit_Support, make sure it is
    # built and available to build Libpythonlang and Liblktlang.
    if not args.no_langkit_support:
        build_langkit_support(args)
        add_to_path(os.environ, "GPR_PROJECT_PATH", str(SUPPORT_ROOT))

    # We need to clean the build space of the langkit libraries we depend
    # upon before we run langkit again. Else, if we installed a newer version
    # of GNAT since those libraries were built, what will happen is that:
    #
    # 1. Langkit will try loading them.
    # 2. This will cause an uncaught exception trying to load some dynamic
    #    library from the compiler, preventing langkit to run.
    # 3. Langkit cannot be used to recompile libpythonlang and liblktlang to
    #    newer versions.
    if not args.lib:
        shutil.rmtree(PYTHON_LIB_ROOT / 'build', ignore_errors=True)
        shutil.rmtree(LKT_LIB_ROOT / 'build', ignore_errors=True)

    base_argv = [
        sys.executable, "./manage.py",
        "make", "-P",
        "-Dgnu-full",
        f"--library-types={args.library_types}",
        f"--build-mode={args.build_mode}",
        f"-j{args.jobs}",
    ]

    # Forward gargs to each manage.py script
    for gargs in args.gargs or []:
        base_argv.append(f"--gargs={gargs}")

    libs = selected_libs(args)
    m1: Optional[subprocess.Popen] = None
    m2: Optional[subprocess.Popen] = None
    if "python" in libs:
        m1 = subprocess.Popen(
            base_argv + ["--disable-warning", "undocumented-nodes"],
            cwd=PYTHON_LIB_ROOT
        )
    if "lkt" in libs:
        m2 = subprocess.Popen(base_argv, cwd=LKT_LIB_ROOT)

    if m1:
        m1.wait()
        assert m1.returncode == 0
    if m2:
        m2.wait()
        assert m2.returncode == 0

    # Unless disabled, run mypy to type check Langkit itself. We need to do
    # this after building liblktlang and libythonlang as Langkit depend on
    # them.
    if not args.no_mypy:
        run_mypy(args)


def run_mypy(args: Namespace) -> None:
    """
    Type-check the Langkit Python codebase.
    """
    subprocess.check_call(["mypy"], cwd=LANGKIT_ROOT)


def test(args: Namespace, remaining_args: List[str]) -> None:
    """
    Run Langkit's testsuite.
    """
    subprocess.check_call(
        [sys.executable, str(LANGKIT_ROOT / 'testsuite' / 'testsuite.py'),
         '-E']
        + remaining_args
    )


if __name__ == '__main__':
    parser = ArgumentParser(description="Global manage script for langkit")
    subparsers = parser.add_subparsers()

    create_subparser(subparsers, build_langkit_support,
                     with_jobs=True,
                     with_gargs=True,
                     with_build_dir=True)
    create_subparser(subparsers, setenv_langkit_support, with_build_dir=True)
    install_lksp = create_subparser(subparsers, install_langkit_support,
                                    with_build_dir=True)
    install_lksp.add_argument(
        "prefix",
        help="Installation prefix"
    )

    package_deps_parser = create_subparser(subparsers, package_deps)
    package_std_dyn_parser = create_subparser(subparsers, package_std_dyn)
    for p in (package_deps_parser, package_std_dyn_parser):
        p.add_argument("package-dir", help="Destination directory")
        Packager.add_prefix_options(p)
        Packager.add_platform_options(p)

    create_subparser(subparsers, make,
                     with_jobs=True,
                     with_no_lksp=True,
                     with_gargs=True,
                     with_build_dir=True,
                     with_libs=True,
                     with_no_mypy=True)
    create_subparser(subparsers, setenv,
                     with_no_lksp=True,
                     with_build_dir=True,
                     with_libs=True)

    create_subparser(subparsers, run_mypy)

    create_subparser(subparsers, test, accept_unknown_args=True)

    parser.set_defaults(func=lambda _, _1: None)

    args, unknown_args = parser.parse_known_args()
    args.func(args, unknown_args)
