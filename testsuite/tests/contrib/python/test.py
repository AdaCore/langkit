"""
Check that Libpythonlang does not bitrot.
"""

import os

import langkit.scripts.lkm as lkm

from utils import langkit_root


lkm.main(
    [
        "make",
        "-vnone",
        "--config",
        os.path.join(langkit_root, "contrib", "python", "langkit.yaml"),
        "--build-dir",
        os.path.abspath("build"),
    ]
)
