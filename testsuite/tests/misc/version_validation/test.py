"""
Check that version numbers are correctly validated.
"""

import langkit

from utils import emit_and_print_errors


for version, build_date in [
    # Invalid formats
    ("invalid-version", None),
    ("1", "invalid-build-date"),
    # Simple version number alone
    ("1", None),
    # Complex version number alone
    ("98!76.54.32a10.post123.dev456", None),
    # Local version identifier computed from the version number
    ("25.0w.20230901", None),
    # Local version identifier computed from the build date
    ("1", "20230901"),
    # Local version identifier computed from both
    ("25.0w", "20230901"),
]:
    print(f"== {version} / {build_date} ==")
    cfg = {}
    if version is not None:
        cfg["version"] = version
    if build_date is not None:
        cfg["build_date"] = build_date
    ctx = emit_and_print_errors(lkt_file="foo.lkt", config={"library": cfg})
    if ctx is not None:
        print("PEP 440 version:", ctx.python_api_settings.pep440_version)
    print()
    langkit.reset()

print("Done")
