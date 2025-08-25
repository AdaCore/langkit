#! /usr/bin/env python

import os
import subprocess

if __name__ == "__main__":
    script_dir = os.path.dirname(__file__)

    os.environ["C_INCLUDE_PATH"] = "{}:{}".format(
        os.path.abspath(os.path.join(script_dir, "..", "src")),
        os.environ["C_INCLUDE_PATH"]
    )


    class_path = [
        os.path.join(
            script_dir,
            "target",
            "${ctx.config.library.language_name.lower}ls-0.1.jar",
        ),
        os.path.join(
            script_dir,
            "..",
            "java","target","${ctx.lib_name}-0.1.jar-0.1.jar",
        )
    ]

    cmd = [
        "native-image.cmd" if os.name == "nt" else "native-image",
        "--macro:truffle",
        "--no-fallback",
        "-cp",
        ":".join(class_path),
        "--features=com.adacore.lklsp.RuntimeLSP4jReflectionRegistration",
        "com.adacore.lklsp.${ctx.config.library.language_name.camel}Ls",
        "-o",
        os.path.join(
            script_dir,
            "target",
            "${ctx.config.library.language_name.lower}ls",
        )
    ]

    subprocess.check_call(cmd)
