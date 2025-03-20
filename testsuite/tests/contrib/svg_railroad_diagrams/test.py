"""
Check that the railroad diagrams pass at least works without crashing.
"""

import os

import langkit.scripts.lkm as lkm

from utils import langkit_root


lkm.main(
    [
        "generate",
        "-vnone",
        "--config",
        os.path.join(langkit_root, "contrib", "python", "langkit.yaml"),
        "--build-dir",
        os.path.abspath("build"),
        "--pass-on=emit railroad diagrams",
    ]
)
print(
    "railroad diagrams generated"
    if os.path.exists(os.path.join("build", "railroad-diagrams", "test.svg"))
    else "railroad diagrams missing"
)
