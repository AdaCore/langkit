"""
Test that Lkt's typer resolves the Lkt spec entirely.
"""

import os
import subprocess


nodes_lkt_path = os.path.join(
    os.environ["LANGKIT_ROOT_DIR"], "lkt", "nodes.lkt"
)
subprocess.check_call(["lkt_toolbox", "-C", nodes_lkt_path])
print("Done.")
