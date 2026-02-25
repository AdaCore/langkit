import os

from langkit.compile_context import AdaSourceKind
from langkit.passes import EmitterPass


def do_emit(emitter, context):
    write_a_body = bool(os.environ.get("WRITE_A_BODY"))

    qualname = ["Libfoolang", "Extra_Src"]
    unit = ".".join(qualname)

    decl = "procedure P;" if write_a_body else "I : Integer;"
    emitter.write_ada_file(
        emitter.src_dir,
        AdaSourceKind.spec,
        qualname,
        f"package {unit} is\n{decl}\nend {unit};\n",
    )

    if write_a_body:
        emitter.write_ada_file(
            emitter.src_dir,
            AdaSourceKind.body,
            qualname,
            f"package body {unit} is\n   procedure P is null;\nend {unit};\n",
        )


def create_pass():
    return EmitterPass("emit a dummy Ada unit", do_emit)
