from __future__ import annotations

import os.path

from langkit.config import LktSpecConfig
from langkit.diagnostics import (
    Location,
    error,
    errors_checkpoint,
    non_blocking_error,
)
import langkit.names as names

import liblktlang as L


def name_from_lower(kind: str, id: L.Id) -> names.Name:
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


def name_from_camel(kind: str, id: L.Id) -> names.Name:
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


def extract_lkt_module_name(filename: str) -> str:
    """
    Return the module name for the given Lkt source filename.

    Abort with a language spec error if this is not a valid Lkt source
    filename.
    """
    basename = os.path.basename(filename)
    name, ext = os.path.splitext(basename)
    valid = True
    try:
        names.check_lower(name)
    except ValueError:
        valid = False
    if not valid or ext != ".lkt":
        error(f"invalid Lkt source filename: {basename!r}", Location.nowhere)
    return name


def load_lkt(config: LktSpecConfig) -> list[L.AnalysisUnit]:
    """
    Load a Lktlang source file and return the closure of Lkt units referenced.
    Raise a DiagnosticError if there are parsing errors.

    :param lkt_file: Name of the file to parse.
    """
    processed_units: set[L.AnalysisUnit] = set()
    modules_map: dict[str, L.AnalysisUnit] = {}
    diagnostics = []

    def process_unit(unit: L.AnalysisUnit) -> None:
        # Do nothing if this unit was already loaded
        if unit in processed_units:
            return
        processed_units.add(unit)

        # Determine the module name for this unit and ensure it is unique
        module_name = extract_lkt_module_name(unit.filename)
        other_unit = modules_map.get(module_name)
        if other_unit is not None:
            error(
                f"conflicting Lkt source filenames: {unit.filename!r} and"
                f" {other_unit.filename!r}",
                Location.nowhere,
            )

        # Register this unit and its diagnostics
        modules_map[module_name] = unit
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
    return list(modules_map.values())
