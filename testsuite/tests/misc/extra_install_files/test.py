"""
Check that the "library.extra_install_files" configuration entry works as
expected.
"""

import os.path

import langkit.scripts.lkm as lkm


# Generate and build the dummy library
lkm.main(["make", "-vnone"])

# Install it
install_dir = "install-dir"
lkm.main(["install", "-vnone", install_dir])

# Check the layout of the directory installation tree
installed_files = []
for path, dirnames, filenames in os.walk(install_dir):
    for f in filenames:
        installed_files.append(
            os.path.relpath(os.path.join(path, f), install_dir)
            .replace("\\", "/")
        )
print("Installation tree:")
for f in sorted(installed_files):
    # Filter out the library itself to focus on extra files
    if not any(
        f.startswith(prefix + "/")
        for prefix in (
            "bin", "include", "java", "lib", "ocaml", "python", "share"
        )
    ):
        print(" *", f)

print('Done')
