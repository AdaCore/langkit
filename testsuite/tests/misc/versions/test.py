"""
Check that version numbers are available from all APIs.
"""

import argparse
import os.path

import langkit.config as C
from langkit.diagnostics import DiagnosticError
import langkit.names as names

from utils import python_support_dir


def run(label, version=None, build_date=None):
    print("== {} ==".format(label))

    config = C.CompilationConfig(
        lkt=C.LktConfig(
            entry_point="test.py",
            source_dirs=[python_support_dir],
            types_from_lkt=True,
        ),
        library=C.LibraryConfig(
            root_directory=os.path.dirname(__file__),
            language_name=names.Name("Foo"),
            version="1.version.number",
            build_date="build.date.number",
        ),
    )

    argv = []
    if version:
        argv.append(f"--version={version}")
    if build_date:
        argv.append(f"--build-date={build_date}")
    parser = argparse.ArgumentParser()
    C.add_args(parser)
    args = parser.parse_args(argv)
    try:
        C.update_config_from_args(config, args)
    except DiagnosticError:
        pass
    else:
        raise RuntimeError("error expected")
    print("")


print("")
run("Conflict on version", version="1.something")
run("Conflict on build date", build_date="something")
