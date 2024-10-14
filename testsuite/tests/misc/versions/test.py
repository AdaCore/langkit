"""
Check that version numbers are available from all APIs.
"""

from langkit.diagnostics import DiagnosticError

from utils import prepare_context


def run(label, version=None, build_date=None):
    print("== {} ==".format(label))

    ctx = prepare_context(
        lkt_file="test.lkt",
        version="1.version.number",
        build_date="build.date.number",
        types_from_lkt=True,
    )
    try:
        ctx.set_versions(version, build_date)
    except DiagnosticError:
        pass
    else:
        raise RuntimeError("error expected")
    print("")


print("")
run("Conflict on version", version="1.something")
run("Conflict on build date", build_date="something")
