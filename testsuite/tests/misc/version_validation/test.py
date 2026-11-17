"""
Check that version numbers are correctly validated.
"""

import langkit

from utils import emit_and_print_errors


for version, build_date in [
    # Invalid formats
    ("invalid-version", None),
    ("1", "invalid-build-date"),
    ("1", "w20230901"),
    ("1", "20230901w"),
    # Simple version number alone
    ("1", None),
    # Complex version number including epoch and minor
    ("98!97.54", None),
    # Unsupported cases:
    # Pre-release.
    ("98!76.54.32a10", None),
    # Post-release
    ("98!76.54.post123", None),
    # Development release
    ("98!76.54.dev456", None),
    # Local version
    ("98!97.54+foo", None),
    # Support for build date:
    # Ignored (not a wavefront/release candidate).
    ("25.1", "20230901"),
    # Included (wavefront)
    ("25.0w", "20230901"),
    # Included (release candidate)
    ("25.1rc", "20230901"),
    # Absent (no build date given)
    ("25.1rc", None),
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
