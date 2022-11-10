import dataclasses
import subprocess
import sys
from typing import List, Type

import libfoolang


@dataclasses.dataclass
class Testcase:
    """
    Each testcase runs the App subclass ``cls`` with the list of command line
    arguments ``args``.
    """
    label: str
    cls: Type[libfoolang.App]
    args: List[str]


class BasicApp(libfoolang.App):
    def process_unit(self, unit):
        unit.root.dump()
        print("")


class WithDefaultFiles(BasicApp):
    def default_get_files(self):
        return ["input1", "input2"]


tests = [
    # Check the interaction between the "default_get_files" method and source
    # files passed on the command line.
    Testcase("nodefaults_noargs", BasicApp, []),
    Testcase("nodefaults_args", BasicApp, ["input3"]),
    Testcase("defaults_noargs", WithDefaultFiles, []),
    Testcase("defaults_args", WithDefaultFiles, ["input3"]),
]


if len(sys.argv) == 1:
    print("main.py: Starting...")
    print("")
    for t in tests:
        print(f"== {t.label} ==")
        print("")
        sys.stdout.flush()
        subprocess.check_call(
            [sys.executable, __file__, t.cls.__name__] + t.args
        )
    print("main.py: Done.")

else:
    clsname, args = sys.argv[1], sys.argv[2:]

    cls = globals()[clsname]
    cls.run(args)
