from utils import build_and_run

import common


common  # avoid pyflakes "unused" warning

build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    java_main="Main",
    types_from_lkt=True,
)
print("Done")
