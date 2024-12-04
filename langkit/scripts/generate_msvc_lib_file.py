import argparse

from langkit.windows import generate_lib_file


parser = argparse.ArgumentParser(
    description="Run MSVC tools to generate a .lib file from a shared library"
    " (.dll).",
)
parser.add_argument(
    "--quiet", "-q", help="Hide tool outputs on success."
)
parser.add_argument(
    "dll-filename", help="Shared library for which to create the .lib file."
)
parser.add_argument(
    "lib-filename", help=".lib file to create for the given shared library."
)


def main() -> None:
    args = parser.parse_args()
    generate_lib_file(
        dll_filename=getattr(args, "dll-filename"),
        lib_filename=getattr(args, "lib-filename"),
        quiet=args.quiet,
    )
