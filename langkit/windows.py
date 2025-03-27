from __future__ import annotations

import os.path
import subprocess


def parse_dumpbin_result(dumpbin_result: str) -> list[str]:
    """
    Extract all symbol names from the output of the "dumpbin" command.

    Dumpbin's results are following this format:

    .. code-block::

        Microsoft (R) COFF/PE Dumper Version 14.35.32217.1
        Copyright (C) Microsoft Corporation.  All rights reserved.


        Dump of file my_lib.dll

        File Type: DLL

          Section contains the following exports for my_lib.dll

            00000000 characteristics
            64382B47 time date stamp Thu Apr 13 18:18:15 2023
                0.00 version
                   1 ordinal base
                4 number of functions
                4 number of names

            ordinal hint RVA      name

                  1    0 0001BA4E Symbol_1
                  2    1 001AF008 Symbol_2
                  3    2 0001BB12 Symbol_3
                  4    3 0002B19E Symbol_x = Symbol_y
                  5    4 0200A51F Symbol_N

        Summary

            1000 .CRT
            6000 .bss
            2000 .data
        ...

    :param dumpbin_result: Result of a "dumpbin /exports" run.
    :return: A list containing all names.
    """
    res = []
    parse_line = False

    # For each line of the dumpbin result parse it
    for line in dumpbin_result.splitlines():
        words = line.split()

        if not parse_line:
            # Spot the heading line of the function listing
            parse_line = words == ["ordinal", "hint", "RVA", "name"]

        else:
            # Add the function name
            if len(words) >= 4:
                res.append(words[3])

            # Spot the ending line
            if len(words) == 1 and words[0] == "Summary":
                parse_line = False

    return res


def generate_lib_file(
    dll_filename: str,
    lib_filename: str,
    quiet: bool = False,
) -> None:
    """
    Run MSVC tools to generate a .lib file from a shared library (.dll).

    :param dll_filename: Shared library for which to create the .lib file.
    :param lib_filename: .lib file to create for the given shared lib.
    :param quiet: If False, forward tool outputs to stdout unconditionally. If
        True, forward them only if unsuccessful.
    """
    # Run dumpbin to get the DLL names
    dumpbin_out = subprocess.check_output(
        ["dumpbin.exe", "/exports", dll_filename],
        stdin=subprocess.DEVNULL,
    )

    # Write the result of the parsed dumpbin in an intermediate .def file
    dll_basename = os.path.basename(dll_filename)
    def_filename = os.path.join(
        os.path.dirname(lib_filename),
        os.path.splitext(dll_basename)[0] + ".def",
    )
    with open(def_filename, "w") as f:
        print("EXPORTS", file=f)
        for name in parse_dumpbin_result(dumpbin_out.decode()):
            print(name, file=f)

    # Generate the .lib file from the .def one
    p = subprocess.run(
        [
            "lib.exe",
            f"/def:{def_filename}",
            f"/out:{lib_filename}",
            "/machine:x64",
            "/nologo",
        ],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE if quiet else None,
        stderr=subprocess.STDOUT if quiet else None,
    )
    if p.returncode != 0:
        print(".lib file generation failed:")
        print(p.stdout)
        p.check_returncode()
