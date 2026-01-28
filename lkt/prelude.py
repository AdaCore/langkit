import os.path

from langkit.common import bytes_repr
from langkit.compile_context import AdaSourceKind, CompileCtx
from langkit.emitter import Emitter
from langkit.passes import EmitterPass


prelude_filename = os.path.join(os.path.dirname(__file__), "prelude.lkt")


def generate_prelude(emitter: Emitter, context: CompileCtx) -> None:
    """
    Generate the liblktlang-prelude.ads source file from prelude.lkt.
    """
    # Read the prelude as a sequence of bytes, to match the destination
    # String value.
    with open(prelude_filename, "rb") as f:
        content = f.read()

    # Format the sources
    lines = [
        "package Liblktlang.Prelude is",
        "   Content : constant String :=",
        bytes_repr(content, indent=" " * 6),
        "   ;",
        "end Liblktlang.Prelude;",
    ]

    # Write the source file and register it, so that it is referenced in
    # the generated project file.
    qual_name = ["Liblktlang", "Prelude"]
    emitter.write_ada_file(
        out_dir=emitter.src_dir,
        source_kind=AdaSourceKind.spec,
        qual_name=qual_name,
        content="\n".join(lines),
    )
    emitter.add_library_interface(
        emitter.ada_file_path(emitter.src_dir, AdaSourceKind.spec, qual_name),
        generated=True,
    )


def generate_prelude_pass() -> EmitterPass:
    return EmitterPass(
        "generate prelude inline sources",
        generate_prelude,
        extra_inputs={prelude_filename, __file__},
    )
