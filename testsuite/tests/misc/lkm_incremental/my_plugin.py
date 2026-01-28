import os.path

from langkit.compile_context import AdaSourceKind
from langkit.passes import EmitterPass


data_file = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "my_plugin_data.txt")
)


def do_emit(emitter, context):
    with open(data_file) as f:
        data = f.read().strip()

    emitter.write_ada_file(
        emitter.src_dir,
        AdaSourceKind.spec,
        ["foo"],
        f"package Foo is\n--  {data}\nend Foo;\n",
    )


def create_pass():
    return EmitterPass(
        "emit a dummy Ada unit",
        do_emit,
        # Any change to the data file or this script must trigger recompilation
        extra_inputs={data_file, __file__},
    )
