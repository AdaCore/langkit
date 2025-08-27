#! /usr/bin/env python

## vim: filetype=makopython
"""
Python wrapper to instantiate ${ctx.config.library.language_name.camel}Ls.
"""

import os
import subprocess


if __name__ == '__main__':

    graal_home = os.environ['GRAAL_HOME']

    java = os.path.join(
        graal_home, 'bin', 'java.exe' if os.name == 'nt' else 'java'
    )

    target_dir = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "target"
    )
    # Get the library path and ensure it is a directory
    lib_dir = os.path.join(target_dir, "lib")
    if not os.path.isdir(lib_dir):
        print(f"The `lib` directory does not exist at {lib_dir}")
        exit(1)

    # Create the class path by listing all JARs in the `target/lib` directory
    # and the JAR containing the main function.
    class_path = os.pathsep.join(
        [
            os.path.join(
                target_dir,
                f'${ctx.config.library.language_name.lower}ls-0.1.jar'
            )
        ] + [
            os.path.join(lib_dir, p)
            for p in os.listdir(lib_dir)
            if p.endswith(".jar")
        ]
    )

    java_library_path = os.environ.get(
        "PATH" if os.name == 'nt' else 'LD_LIBRARY_PATH', ''
    )

    subprocess.run([
        java,
        '-cp', class_path,
        "--enable-native-access=ALL-UNNAMED",
        "--sun-misc-unsafe-memory-access=allow",
        f'-Djava.library.path={java_library_path}',
        f'com.adacore.lklsp.${ctx.config.library.language_name.camel}Ls'
    ])
