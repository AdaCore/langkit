"""
Test that the "create-project.py" script creates a valid Langkit project by
generating it and building it.
"""

from __future__ import absolute_import, division, print_function

import os.path
import subprocess
import sys

from utils import langkit_root


def python(script, *args):
    subprocess.check_call([sys.executable, script] + list(args))


create_project_py = os.path.join(langkit_root, 'scripts', 'create-project.py')
manage_py = os.path.join('mylang', 'manage.py')

python(create_project_py, 'Mylang')
python(manage_py, '-vnone', 'make', '-P')

print('Done')
