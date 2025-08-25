#! /usr/bin/env python

## vim: filetype=makopython
"""
Python wrapper to instantiate ${ctx.config.library.language_name.camel}Ls.
"""

import os
import os.path as P
import subprocess


if __name__ == '__main__':

    graal_home = os.environ['GRAAL_HOME']

    java = P.join(graal_home, 'bin', 'java.exe' if os.name == 'nt' else 'java')

    class_path = P.join(
        P.dirname(P.realpath(__file__)),
        'target',
        f'${ctx.config.library.language_name.lower}ls-0.1.jar'
    )

    java_library_path = os.environ.get(
        "PATH" if os.name == 'nt' else 'LD_LIBRARY_PATH', ''
    )

    subprocess.run([
        java,
        '-cp', class_path,
        f'-Djava.library.path={java_library_path}',
        f'--add-exports',
        f'org.graalvm.truffle/com.oracle.truffle.api.strings=ALL-UNNAMED',
        f'com.adacore.lklsp.${ctx.config.library.language_name.camel}Ls'
    ])
