from __future__ import annotations

from collections import OrderedDict
from contextlib import AbstractContextManager, contextmanager
import os.path
from typing import Iterator

from langkit.compile_context import CompileCtx
from langkit.config import LktSpecConfig
from langkit.diagnostics import (
    Location,
    diagnostic_context,
    error,
    errors_checkpoint,
    non_blocking_error,
)
import langkit.names as names

import liblktlang as L


def name_from_lower(ctx: CompileCtx, kind: str, id: L.Id) -> names.Name:
    """
    Validate "id" as a lower-case name and return the corresponding ``Name``
    instance.
    """
    with lkt_context(id):
        try:
            names.check_common(id.text)
        except ValueError as exc:
            error(str(exc))
        try:
            names.check_lower(id.text)
        except ValueError:
            error(f"lower case expected for {kind} names")
        return names.Name.from_lower(id.text)


def name_from_camel(ctx: CompileCtx, kind: str, id: L.Id) -> names.Name:
    """
    Validate "id" as a camel-case name and return the corresponding ``Name``
    instance.
    """
    with lkt_context(id):
        try:
            names.check_common(id.text)
        except ValueError as exc:
            error(str(exc))
        try:
            names.check_camel(id.text)
        except ValueError as exc:
            error(f"camel case expected for {kind} names ({exc})")
        return names.Name.from_camel(id.text)


def lkt_doc(decl: L.Decl) -> str:
    """
    Return the documentation attached to the ``decl`` node. This is an empty
    string if the docstring is missing.

    :param decl: Declaration to process.
    """
    from langkit.frontend.static import denoted_str

    full_decl = decl.parent
    assert isinstance(full_decl, L.FullDecl)
    return "" if full_decl.f_doc is None else denoted_str(full_decl.f_doc)


def lkt_context(lkt_node: L.LktNode | None) -> AbstractContextManager[None]:
    """
    Context manager to set the diagnostic context to the given node.

    :param lkt_node: Node to use as a reference for this diagnostic context. If
        it is ``None``, leave the diagnostic context unchanged.
    """
    if lkt_node is None:

        @contextmanager
        def null_ctx_mgr() -> Iterator[None]:
            yield

        return null_ctx_mgr()

    else:
        # Invalid type passed here will fail much later and only if a
        # check_source_language call fails. To ease debugging, check that
        # "lkt_node" has the right type here.
        assert isinstance(lkt_node, L.LktNode)

        return diagnostic_context(Location.from_lkt_node(lkt_node))


def load_lkt(config: LktSpecConfig) -> list[L.AnalysisUnit]:
    """
    Load a Lktlang source file and return the closure of Lkt units referenced.
    Raise a DiagnosticError if there are parsing errors.

    :param lkt_file: Name of the file to parse.
    """
    units_map = OrderedDict()
    diagnostics = []

    def process_unit(unit: L.AnalysisUnit) -> None:
        if unit.filename in units_map:
            return

        # Register this unit and its diagnostics
        units_map[unit.filename] = unit
        for d in unit.diagnostics:
            diagnostics.append((unit, d))

        # Recursively process the units it imports. In case of parsing error,
        # just stop the recursion: the collection of diagnostics is enough.
        if not unit.diagnostics:
            assert isinstance(unit.root, L.LangkitRoot)
            import_stmts = list(unit.root.f_imports)
            for imp in import_stmts:
                process_unit(imp.p_referenced_unit)

    # Give Liblktlang access to the Lkt files to analyze
    old_path = os.environ.get("LKT_PATH", "")
    os.environ["LKT_PATH"] = os.path.pathsep.join(config.source_dirs)

    # Load ``lkt_file`` and all the units it references, transitively
    ctx = L.AnalysisContext(unit_provider=L.UnitProvider.from_lkt_path())
    process_unit(ctx.get_from_file(config.entry_point))

    os.environ["LKT_PATH"] = old_path

    # Forward potential lexing/parsing errors to our diagnostics system
    for u, d in diagnostics:
        non_blocking_error(
            d.message, Location.from_sloc_range(u, d.sloc_range)
        )
    errors_checkpoint()
    return list(units_map.values())
