"""
Test that the "create-project.py" script creates a valid Langkit project by
generating it and building it.
"""

import os.path
import subprocess
import sys

import langkit


def python(script, *args):
    subprocess.check_call([sys.executable, script] + list(args))


create_project_py = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(langkit.__file__))),
    'scripts', 'create-project.py'
)
manage_py = os.path.join('mylang', 'manage.py')

python(create_project_py, 'Mylang')
python(manage_py, '-vnone', 'make')

print 'Done'
