"""
Check that invalid configurations are correctly rejected.
"""

import glob

import langkit.scripts.lkm as lkm


for config in sorted(glob.glob("*.yaml")):
    if config == "test.yaml":
        continue

    print(f"== {config} ==")
    print()
    lkm.main_no_exit(["setenv", "-c", config])
    print()

print('Done')
