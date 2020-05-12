"""
Test that Lkt's import statements work as expected.
"""

from collections import namedtuple
import os
import subprocess
import sys


Test = namedtuple('Test', 'label lkt_file path abspath')

base_path = os.environ['LKT_PATH'].split(os.path.pathsep)


for t in [
    Test('Unit not found', 'other-dir.lkt', [], abspath=False),
    Test('Unit in path, relative', 'other-dir.lkt', ['src'], abspath=False),
    Test('Unit in path, absolute', 'other-dir.lkt', ['src'], abspath=True),
    Test('Import loop', 'loop.lkt', [], False),
]:
    os.environ['LKT_PATH'] = os.path.pathsep.join(
        [(os.path.abspath(d) if t.abspath else d) for d in t.path] +
        base_path
    )
    subprocess.check_call([sys.executable, 'helper.py', t.label, t.lkt_file])

print('Done')
