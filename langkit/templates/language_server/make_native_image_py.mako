#! /usr/bin/env python

import argparse
import os
import sys
import subprocess

def look_for_files_in_env(
    files: list[str],
    env_var_name: str,
) -> dict[str, str] | None:
    """
    Look for required `files` in directories listed in the value of the
    environment variable `env_var_name`. Return the dictionary associating each
    input file to the directory containing it. If one of the requested file
    cannot be retrieved this function returns `None` and displays error message
    about file not being found.
    """
    res = {f: None for f in files}
    for dir in os.environ.get(env_var_name, "").split(os.pathsep):
        for file in files:
            if os.path.isfile(os.path.join(dir, file)):
                res[file] = dir
                break
    one_not_found = False
    for file, dir in res.items():
        if dir is None:
            one_not_found = True
            print(f"Cannot find \"{file}\" in {env_var_name}")
    return None if one_not_found else res


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--class-path", help="Classpath to forward")
    args = parser.parse_args()

    native_image = "native-image.cmd"
    native_image_args = [
        "-H:+UnlockExperimentalVMOptions",
        "-H:-StrictQueryCodeCompilation",
    ]

    if os.name != "nt":
        native_image = "native-image"
        # On Linux, we need to provide the compiler additional arguments for
        # it to find headers and shared objects.
        headers_paths = look_for_files_in_env(
            ["${ctx.lib_name.lower}.h"],
            "C_INCLUDE_PATH",
        )
        libs_paths = look_for_files_in_env(
            ["${ctx.lib_name.lower}.so", "libz.so"],
            "LIBRARY_PATH",
        )
        if headers_paths is None or libs_paths is None:
            print("Missing lib dependencies, cannot continue")
            exit(1)

        # We also need to provide rpath-links to the compiler to allow it to
        # find libraries during linking phase.
        ld_library_path = os.environ.get('LD_LIBRARY_PATH')
        rpaths = (
            [f"-Wl,-rpath-link={p}" for p in ld_library_path.split(os.pathsep)]
            if ld_library_path else
            []
        )

        # Then we add additional options for the C compiler
        native_image_args.extend([
            *[
                f"--native-compiler-options=-I{dir}"
                for dir in headers_paths.values()
            ],
            *[
                f"--native-compiler-options=-L{dir}"
                for dir in libs_paths.values()
            ],
            *[f"--native-compiler-options={rp}" for rp in rpaths],
        ])

    script_dir = os.path.dirname(__file__)

    os.environ["C_INCLUDE_PATH"] = "{}:{}".format(
        os.path.abspath(os.path.join(script_dir, "..", "src")),
        os.environ["C_INCLUDE_PATH"]
    )


    cmd = [
        native_image,
        "--no-fallback",
        "-cp",
        args.class_path,
        "com.adacore.lklsp.${ctx.config.library.language_name.camel}Ls",
        "-o",
        os.path.join(
            script_dir,
            "target",
            "${ctx.config.library.language_name.lower}ls",
        ),
        *native_image_args,
    ]

    subprocess.check_call(cmd)
