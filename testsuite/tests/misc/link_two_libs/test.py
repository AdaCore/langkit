"""
Test that two generated libraries can live in the same program.
"""

import os.path
import subprocess

import langkit
import langkit.scripts.create_project as create_project
import langkit.scripts.lkm as lkm


# Generate two libraries
for lang in ("Foo", "Bar"):
    lang_lower = lang.lower()

    create_project.main([lang])
    config_file = f"{lang_lower}.yaml"
    os.rename("langkit.yaml", config_file)
    build_dir = f"build-{lang_lower}"

    lkm.main(
        [
            "make",
            "-c",
            config_file,
            "--build-dir",
            build_dir,
            "-vnone",
            # To simplify this test (in particular environment setup), do a
            # static link.
            "--library-types=static",
        ]
    )
    langkit.reset()

# Build a program that uses both and run it
subprocess.check_call(
    ["gprbuild", "-q", "-Pmain.gpr", "-p", "-XLIBRARY_TYPE=static"]
)
subprocess.check_call([os.path.join("obj", "main")])

print("Done")
