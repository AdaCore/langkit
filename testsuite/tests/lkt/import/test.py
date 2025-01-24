"""
Test that Lkt's import statements work as expected.
"""

import dataclasses
import os.path

import langkit
from langkit.compile_context import CompileCtx
import langkit.config as C
from langkit.diagnostics import DiagnosticError
import langkit.names as names
from langkit.utils import PluginLoader

from utils import python_support_dir


@dataclasses.dataclass
class Test:
    label: str
    lkt_file: str
    path: list[str]


base_path = python_support_dir
src_dir = "src"
abs_src_dir = os.path.abspath(src_dir)

root_dir = os.path.dirname(__file__)
plugin_loader = PluginLoader(root_dir)
lib_config = C.LibraryConfig(
    root_directory=root_dir,
    language_name=names.Name("Foo"),
)

for t in [
    Test("Unit not found", "other-dir.lkt", [base_path]),
    Test("Unit in path, relative", "other-dir.lkt", [base_path, src_dir]),
    Test("Unit in path, absolute", "other-dir.lkt", [base_path, abs_src_dir]),
    Test("Import loop", "loop.lkt", [base_path]),
]:
    print(f"== {t.label} ==")
    config = C.CompilationConfig(
        lkt_spec=C.LktSpecConfig(t.lkt_file, t.path),
        library=lib_config,
        warnings={"undocumented-nodes": False},
    )

    try:
        ctx = CompileCtx(config, plugin_loader)
        ctx.create_all_passes(check_only=True)
        ctx.emit()
    except DiagnosticError:
        pass
    else:
        print("Code generation was successful")
    langkit.reset()
    print("... done")
    print()

print("Done")
