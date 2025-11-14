#! /usr/bin/env python

from argparse import ArgumentParser, FileType, Namespace, _SubParsersAction
import glob
import json
import os
import os.path as P
from pathlib import Path, PurePath
import subprocess
import sys
import tempfile
from typing import Callable, Dict, List

from e3.fs import rm, sync_tree

from langkit.packaging import NativeLibPackager
import langkit.scripts.lkm as lkm
from langkit.utils import (
    LibraryType,
    add_to_path,
    format_printenv,
    get_cpu_count,
    parse_cmdline_args,
)


LANGKIT_ROOT = PurePath(P.dirname(P.realpath(__file__)))
SUPPORT_ROOT = LANGKIT_ROOT / "langkit" / "support"
SUPPORT_GPR = str(SUPPORT_ROOT / "langkit_support.gpr")
SIGSEGV_HANDLER_ROOT = LANGKIT_ROOT / "sigsegv_handler"
SIGSEGV_HANDLER_GPR = SIGSEGV_HANDLER_ROOT / "langkit_sigsegv_handler.gpr"
LKT_LIB_ROOT = LANGKIT_ROOT / "lkt"
LKT_BOOTSTRAP_ROOT = LKT_LIB_ROOT / "bootstrap"

BOOTSTRAP_LKM_BASE_ARGS = [
    f"--config={LKT_BOOTSTRAP_ROOT / 'langkit.yaml'}",
    "--build-dir=.",
]

BOOTSTRAP_LKM_RUN_BASE_ARGS = [
    "run",
    *BOOTSTRAP_LKM_BASE_ARGS,
    "--",
    sys.executable,
    "-m",
    "langkit.scripts.lkm",
]

BOOTSTRAP_LKM_BUILD_BASE_ARGS = [
    *BOOTSTRAP_LKM_BASE_ARGS,
    # Avoid absolute filenames in generated code to avoid variations in the
    # bootstrap code that is under version control.
    "--portable-project",
    # The only thing we need for bootstrap is a shared library that the Python
    # bindings can import: no static nor static-pic libraries nor mains.
    "--library-types=relocatable",
    "--disable-all-mains",
]


def create_subparser(
    subparsers: _SubParsersAction,
    fn: Callable[..., None],
    *,
    with_jobs: bool = False,
    with_no_lksp: bool = False,
    with_gargs: bool = False,
    with_build_dir: bool = False,
    with_libs: bool = False,
    with_generate_dll_lib_adding: bool = False,
    with_generate_msvc_lib: bool = False,
    with_no_mypy: bool = False,
    with_output: bool = False,
    no_basic_options: bool = False,
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
    :param bool with_generate_dll_lib_adding: Whether to create the
        --generate-auto-dll-dirs option.
    :param bool with_output: Whether to create the --output option.
    :param bool with_no_mypy: Whether to create the --no-mypy option.
    :param no_basic_options: Whether to disable the creation of basic options
        (--build-mode, ...).
    """
    subparser = subparsers.add_parser(
        name=fn.__name__.replace("_", "-"),
        help=fn.__doc__,
        add_help=not accept_unknown_args,
    )

    if not no_basic_options:
        subparser.add_argument(
            "--build-mode",
            "-b",
            choices=("dev", "prod"),
            default="dev",
            help="Select a preset for build options.",
        )
        LibraryType.add_argument(subparser)

    if with_jobs:
        subparser.add_argument(
            "--jobs",
            "-j",
            type=int,
            default=get_cpu_count(),
            help="Number of parallel jobs to spawn in parallel (default: your"
            " number of cpu).",
        )
    if with_no_lksp:
        subparser.add_argument(
            "--no-langkit-support",
            action="store_true",
            help="Assume that Langkit_Support is already built and installed."
            " We rebuild it by default, for the convenience of"
            " developers.",
        )
    if with_gargs:
        subparser.add_argument(
            "--gargs",
            action="append",
            help="Options appended to GPRbuild invocations.",
        )
    if with_build_dir:
        subparser.add_argument(
            "--build-dir",
            help="Use a non-default build directory. This allows out-of-tree"
            " builds.",
        )
    if with_libs:
        subparser.add_argument(
            "--lib",
            "-l",
            choices=("python", "lkt"),
            action="append",
            help="Select which libraries on which to operate. By default, work"
            " on all libraries.",
        )
    if with_generate_dll_lib_adding:
        subparser.add_argument(
            "--generate-auto-dll-dirs",
            action="store_true",
            help="For selected libs (python and lkt) forward the DLL"
            " directories adding flag to the generation phase.",
        )
    if with_generate_msvc_lib:
        subparser.add_argument(
            "--generate-msvc-lib",
            action="store_true",
            help="Generate a .lib file from the library DLL that MSVC"
            " toolchains need in order to link against the DLL. This is"
            " supported only on Windows, and requires the Visual Studio"
            " Build Tools in the environment.",
        )
    if with_output:
        subparser.add_argument(
            "--output",
            "-o",
            type=FileType("w"),
            default=sys.stdout,
            help="Write the command output to a file. This is recommended when"
            " writing scripts, so that warnings are not included in the result"
            " (they are not written to that output file)",
        )
    if with_no_mypy:
        subparser.add_argument(
            "--no-mypy",
            action="store_true",
            help="Whether to disable type-checking with mypy.",
        )

    def wrapper(args: Namespace, rest: List[str]):
        if len(rest) > 0:
            print(
                "ERROR - unhandled command line arguments: {}".format(
                    ", ".join(rest)
                )
            )
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
        "gprbuild",
        "-p",
        f"-j{args.jobs}",
        f"-XBUILD_MODE={args.build_mode}",
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

    for library_type in args.library_types:
        for lexch in glob.glob(lexch_pattern):
            os.remove(lexch)
        subprocess.check_call(
            base_argv
            + ["-P", SUPPORT_GPR, f"-XLIBRARY_TYPE={library_type.value}"]
            + gargs
        )

    # SigSegV handler is a relocatable library, skip if only static requested
    if LibraryType.relocatable in args.library_types:
        subprocess.check_call(base_argv + ["-P", SIGSEGV_HANDLER_GPR] + gargs)


def langkit_support_env_map(
    args: Namespace, json: bool = False
) -> Dict[str, str]:
    """
    Helper function. Returns a key-value map for langkit_support's environment.
    """
    # Make the shared library for Langkit_Support available to the dynamic
    # linker.
    build_dir = PurePath(args.build_dir) if args.build_dir else SUPPORT_ROOT
    dynamic_lib_dir = str(build_dir / "lib" / "relocatable" / args.build_mode)

    return {
        # Make the "langkit_support.gpr" available to GPRbuild
        "GPR_PROJECT_PATH": str(SUPPORT_ROOT),
        "PATH": dynamic_lib_dir,
        "LD_LIBRARY_PATH": os.pathsep.join(
            [
                dynamic_lib_dir,
                # Make the shared lib for the sigsegv handler available for
                # OCaml on GNU/Linux.
                str(SIGSEGV_HANDLER_ROOT / "lib"),
            ],
        ),
    }


def install_langkit_support(args: Namespace) -> None:
    """
    Install the Langkit_Support project.
    """
    base_argv = [
        "gprinstall",
        "-P",
        SUPPORT_GPR,
        "-p",
        f"-XBUILD_MODE={args.build_mode}",
        f"--prefix={args.prefix}",
        "--build-var=LIBRARY_TYPE",
        "--build-var=LANGKIT_SUPPORT_LIBRARY_TYPE",
        "--sources-subdir=include/langkit_support",
    ]
    if args.build_dir:
        base_argv.extend([f"--relocate-build-tree={args.build_dir}"])
    if args.force:
        base_argv.append("-f")

    # Install the static libraries first, so that in the resulting project
    # files, "static" is the default library type.
    lib_types = [l.value for l in args.library_types]
    for library_type in ("static", "static-pic", "relocatable"):
        if library_type in lib_types:
            subprocess.check_call(
                base_argv
                + [
                    f"-XLIBRARY_TYPE={library_type}",
                    f"--build-name={library_type}",
                ]
            )


def package_deps(args: Namespace) -> None:
    """
    Bundle all dependencies to complete GNAT Pro.
    """
    p = NativeLibPackager.from_args(args)
    p.package_deps(getattr(args, "package-dir"))


def package_std_dyn(args: Namespace) -> None:
    """
    Bundle all dependencies to create standalone packages.
    """
    pass
    p = NativeLibPackager.from_args(args)
    pkg_dir = getattr(args, "package-dir")
    p.package_standalone_dyn(pkg_dir)
    p.package_langkit_support_dyn(pkg_dir)


def printenv(args: Namespace) -> None:
    """
    Print shell commands to add Libpythonlang and Liblktlang to the
    environment.
    """
    env = {}
    if not args.no_langkit_support:
        env = langkit_support_env_map(args)

    with tempfile.NamedTemporaryFile(
        prefix="lkm-printenv",
        suffix=".json",
        delete=False,
    ) as tmpf:
        try:
            tmpf.close()
            subprocess.run(
                [
                    sys.executable,
                    "-m",
                    "langkit.scripts.lkm",
                    "printenv",
                    f"--config={LKT_LIB_ROOT / 'langkit.yaml'}",
                    f"--build-mode={args.build_mode}",
                    "-J",
                    f"--output={tmpf.name}",
                ],
            )
            with open(tmpf.name) as f:
                d = json.load(f)
        finally:
            os.remove(tmpf.name)

    for k, v in d.items():
        if k in env:
            env[k] = os.pathsep.join([env[k], v])
        else:
            env[k] = v

    if args.json:
        json.dump(env, args.output)
    else:
        for k, v in env.items():
            print(format_printenv(k, v), file=args.output)


def printenv_langkit_support(args: Namespace) -> None:
    """
    Printenv for Langkit_Support.
    """
    for k, v in langkit_support_env_map(args).items():
        print(format_printenv(k, v), file=args.output)


def bootstrap_build_args(args: Namespace, generate: bool = False) -> list[str]:
    """
    Return lkm build arguments to build the bootstrap Liblktlang.

    :param generate: Whether to generate the bootstrap Liblktlang.
    """
    argv = [
        "make" if generate else "build",
        *BOOTSTRAP_LKM_BUILD_BASE_ARGS,
        f"-j{args.jobs}",
    ]
    for a in args.gargs or []:
        argv += ["--gargs", a]

    # Allow the Python bindings to find the necessary DLLs through the PATH
    # environment variable on Windows, to simplify the bootstrap setup.
    if generate:
        argv.append("--generate-auto-dll-dirs")

    return argv


def prepare_bootstrap(args: Namespace) -> None:
    """
    Make sure the bootstrap Liblktlang library is ready to use.
    """
    check_argv = [
        sys.executable,
        "-m",
        "langkit.scripts.lkm",
        "run",
        *BOOTSTRAP_LKM_BASE_ARGS,
        sys.executable,
        str(LKT_LIB_ROOT / "check_bootstrap.py"),
    ]

    # First check if Liblktlang can be imported: if that's the case, there is
    # nothing else to do.
    p = subprocess.run([*check_argv, "-q"])
    if p.returncode == 0:
        return

    print("Bootstrap Liblktlang needs to be built")
    sys.stdout.flush()
    lkm.main(bootstrap_build_args(args))

    # For dev convenience, abort early if Liblktlang still cannot be imported
    subprocess.check_call(check_argv)


def bootstrap(args: Namespace) -> None:
    """
    Regenerate and build the bootstrap Liblktlang.
    """
    prepare_bootstrap(args)

    # Copy the Lkt project sources (YAML config, Lkt sources and extensions)
    # into the bootstrap directory.
    sync_tree(
        source=str(LKT_LIB_ROOT),
        target=str(LKT_BOOTSTRAP_ROOT),
        ignore=[
            ".gitignore",
            "__pycache__",
            "bootstrap",
            "build",
            "check_bootstrap.py",
        ],
        delete=False,
    )

    # Regenerate the Lkt project in the bootstrap directory
    lkm.main(
        [
            *BOOTSTRAP_LKM_RUN_BASE_ARGS,
            *bootstrap_build_args(args, generate=True),
        ]
    )

    # Now that we have the codegen for the bootstrap project, its Lkt sources
    # (just copies of the Lkt project itself) are no longer useful: just remove
    # them, to avoid unecessary bootstrap directory bloat.
    for lkt_src in Path(LKT_BOOTSTRAP_ROOT).glob("**/*.lkt"):
        lkt_src.unlink()

    # Likewise for the extension sources used as templates (i.e. only during
    # codegen).
    for child in Path(LKT_BOOTSTRAP_ROOT / "extensions").iterdir():
        if child.name != "src":
            rm(str(child), recursive=True)

    # Get rid of the Java and OCaml bindings, irrelevant for bootstrap matters
    for d in ["java", "ocaml"]:
        rm(str(LKT_BOOTSTRAP_ROOT / d), recursive=True)


def clean(args: Namespace) -> None:
    """
    Clean up build artifacts for the bootstrap Liblktlang.
    """
    for subdir in ["obj", "lib"]:
        rm(str(LKT_BOOTSTRAP_ROOT / subdir), recursive=True)


def make(args: Namespace) -> None:
    """
    Generate and build Libpythonlang and Liblktlang.
    """
    prepare_bootstrap(args)

    # Unless specifically asked to ignore Langkit_Support, make sure it is
    # built and available to build Liblktlang.
    if not args.no_langkit_support:
        build_langkit_support(args)
        add_to_path(os.environ, "GPR_PROJECT_PATH", str(SUPPORT_ROOT))

    lib_types = ",".join(l.value for l in args.library_types)
    argv = [
        "make",
        "--config",
        str(LKT_LIB_ROOT / "langkit.yaml"),
        "-Dgnu-full",
        f"--library-types={lib_types}",
        f"--build-mode={args.build_mode}",
        f"-j{args.jobs}",
    ]

    # If the DLL directories adding flag is on forward it
    if args.generate_auto_dll_dirs:
        argv.append("--generate-auto-dll-dirs")

    # If the MSVC lib flag is on forward it
    if args.generate_msvc_lib:
        argv.append("--generate-msvc-lib")

    # Forward gargs to each manage.py script
    for gargs in args.gargs or []:
        argv.append(f"--gargs={gargs}")

    lkm.main([*BOOTSTRAP_LKM_RUN_BASE_ARGS, *argv])

    # Unless disabled, run mypy to type check Langkit itself. We need to do
    # this after building Liblktlang as Langkit depend on them.
    if not args.no_mypy:
        run_mypy(args)


def run_mypy(args: Namespace) -> None:
    """
    Type-check the Langkit Python codebase.
    """
    # Make sure mypy can find the type hints for the Liblktlang Python
    # bindings.
    env = dict(os.environ)
    add_to_path(env, "MYPYPATH", str(LKT_LIB_ROOT / "build" / "python"))
    subprocess.check_call(["mypy"], cwd=LANGKIT_ROOT, env=env)


def test(args: Namespace, remaining_args: List[str]) -> None:
    """
    Run Langkit's testsuite.
    """
    # Propagate the return code from the testsuite to our own parent process.
    # This is useful for scripts (for instance CIs) to easily detect when there
    # is at least one failure.
    sys.exit(
        subprocess.call(
            [
                sys.executable,
                str(LANGKIT_ROOT / "testsuite" / "testsuite.py"),
                "-E",
            ]
            + remaining_args
        )
    )


if __name__ == "__main__":
    parser = ArgumentParser(description="Global manage script for langkit")
    subparsers = parser.add_subparsers()

    create_subparser(
        subparsers,
        build_langkit_support,
        with_jobs=True,
        with_gargs=True,
        with_build_dir=True,
    )
    create_subparser(
        subparsers,
        printenv_langkit_support,
        with_build_dir=True,
        with_output=True,
    )
    install_lksp = create_subparser(
        subparsers, install_langkit_support, with_build_dir=True
    )
    install_lksp.add_argument(
        "--force",
        "-f",
        action="store_true",
        help="Force installation, overwrite files.",
    )
    install_lksp.add_argument("prefix", help="Installation prefix")

    package_deps_parser = create_subparser(subparsers, package_deps)
    package_std_dyn_parser = create_subparser(subparsers, package_std_dyn)
    for p in (package_deps_parser, package_std_dyn_parser):
        p.add_argument("package-dir", help="Destination directory")
        NativeLibPackager.add_prefix_options(p)
        NativeLibPackager.add_platform_options(p)

    create_subparser(
        subparsers,
        make,
        with_jobs=True,
        with_no_lksp=True,
        with_gargs=True,
        with_build_dir=True,
        with_libs=True,
        with_generate_dll_lib_adding=True,
        with_generate_msvc_lib=True,
        with_no_mypy=True,
    )
    printenv_parser = create_subparser(
        subparsers,
        printenv,
        with_no_lksp=True,
        with_build_dir=True,
        with_libs=True,
        with_output=True,
    )
    printenv_parser.add_argument(
        "--json",
        "-J",
        action="store_true",
        help="Output necessary env keys to JSON.",
    )

    create_subparser(subparsers, run_mypy)

    create_subparser(subparsers, test, accept_unknown_args=True)

    create_subparser(subparsers, bootstrap, with_jobs=True, with_gargs=True)

    create_subparser(subparsers, clean, no_basic_options=True)

    parser.set_defaults(func=lambda _, _1: None)

    args, unknown_args = parser.parse_known_args()
    args.func(args, unknown_args)
