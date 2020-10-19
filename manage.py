#! /usr/bin/env python

from argparse import ArgumentParser, Namespace, _SubParsersAction
from os import path as P
from pathlib import PurePath
import shutil
import subprocess
import sys
from typing import Callable, List


LANGKIT_ROOT = PurePath(P.dirname(P.realpath(__file__)))
LKT_LIB_ROOT = LANGKIT_ROOT / "contrib" / "lkt"
PYTHON_LIB_ROOT = LANGKIT_ROOT / "contrib" / "python"


def create_subparser(
    subparsers: _SubParsersAction,
    fn: Callable[..., None],
    accept_unknown_args: bool = False,
) -> ArgumentParser:
    """
    Create a subparser with given ``fn`` as func. Extract doc and name from
    the function.
    """
    subparser = subparsers.add_parser(name=fn.__name__, help=fn.__doc__,
                                      add_help=not accept_unknown_args)

    def wrapper(args: Namespace, rest: str):
        if len(rest) > 0:
            print("ERROR - unhandled command line arguments: {}".format(
                ", ".join(rest)
            ))
            sys.exit(1)
        fn(args)

    subparser.set_defaults(func=fn if accept_unknown_args else wrapper)

    return subparser


def setenv(args: Namespace) -> None:
    """
    Print shell commands to add Libpythonlang and Liblktlang to the
    environment.
    """
    for cwd in (LKT_LIB_ROOT, PYTHON_LIB_ROOT):
        subprocess.check_call(
            [sys.executable, "./manage.py", "setenv"],
            cwd=cwd
        )


def make(args: Namespace) -> None:
    """
    Generate and build Libpythonlang and Liblktlang.
    """

    # We need to clean the build space of the langkit libraries we depend
    # upon before we run langkit again. Else, if we installed a newer version
    # of GNAT since those libraries were built, what will happen is that:
    #
    # 1. Langkit will try loading them.
    # 2. This will cause an uncaught exception trying to load some dynamic
    #    library from the compiler, preventing langkit to run.
    # 3. Langkit cannot be used to recompile libpythonlang and liblktlang to
    #    newer versions.
    shutil.rmtree(LKT_LIB_ROOT / 'build', ignore_errors=True)
    shutil.rmtree(PYTHON_LIB_ROOT / 'build', ignore_errors=True)

    m1 = subprocess.Popen(
        [sys.executable, "./manage.py", "-Dgnu-full", "make", "-P",
         "--disable-warning", "undocumented-nodes"],
        cwd=PYTHON_LIB_ROOT
    )
    m2 = subprocess.Popen(
        [sys.executable, "./manage.py", "-Dgnu-full", "make", "-P"],
        cwd=LKT_LIB_ROOT
    )
    m1.wait()
    m2.wait()
    assert m1.returncode == 0
    assert m2.returncode == 0


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

    create_subparser(subparsers, make)
    create_subparser(subparsers, setenv)
    create_subparser(subparsers, test, accept_unknown_args=True)

    parser.set_defaults(func=lambda _, _1: None)

    args, unknown_args = parser.parse_known_args()
    args.func(args, unknown_args)
