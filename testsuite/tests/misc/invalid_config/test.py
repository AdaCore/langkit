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
    lkm.main(["setenv", "-c", config], no_exit=True)
    print()

print('Done')
