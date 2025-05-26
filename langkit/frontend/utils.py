from __future__ import annotations

from collections import OrderedDict
import os.path

from langkit.compile_context import CompileCtx
from langkit.config import LktSpecConfig
from langkit.diagnostics import (
    Location,
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
    try:
        names.check_common(id.text)
    except ValueError as exc:
        error(str(exc), location=id)
    try:
        names.check_lower(id.text)
    except ValueError:
        error(f"lower case expected for {kind} names", location=id)
    return names.Name.from_lower(id.text)


def name_from_camel(ctx: CompileCtx, kind: str, id: L.Id) -> names.Name:
    """
    Validate "id" as a camel-case name and return the corresponding ``Name``
    instance.
    """
    try:
        names.check_common(id.text)
    except ValueError as exc:
        error(str(exc), location=id)
    try:
        names.check_camel(id.text)
    except ValueError as exc:
        error(f"camel case expected for {kind} names ({exc})", location=id)
    return names.Name.from_camel(id.text)


def arg_name_from_expr(e: L.Expr) -> L.Id:
    """
    Return the argument name for a given argument value expression.

    This assumes that ``e`` is the ``f_value`` argument of a named ``Argument``
    node, and return the ``f_name`` field for that argument.
    """
    arg = e.parent
    assert isinstance(arg, L.Argument)
    assert arg.f_name is not None
    return arg.f_name


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
