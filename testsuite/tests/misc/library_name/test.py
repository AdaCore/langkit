"""
Check the handling of the library:library_name configuration entry. It used to
be incorrectly interpreted.
"""

import os.path

import langkit.scripts.lkm as lkm


# Build and run the custom main. That main will compile iff
# library:library_name was correctly used, because of the "with Bar;" context
# clause it has.
lkm.main(["make", "-vnone", "--version=42.0"])
lkm.main(["run", os.path.join("build", "obj-mains", "dev", "my_main")])

print("Done")
