"""
Test that two generated libraries can live in the same program.
"""

import os.path
import subprocess
import sys

from e3.os.fs import which

import langkit


scripts_dir = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(langkit.__file__))),
    'scripts')


def python(script, *args):
    subprocess.check_call([sys.executable, script] + list(args))


def locate_script(name):
    return which(name, default=os.path.join(scripts_dir, name))


create_project_py = locate_script('create-project.py')


# Generate two libraries
for lang in ('Foo', 'Bar'):
    manage_py = os.path.join(lang.lower(), 'manage.py')
    subprocess.check_call([create_project_py, lang])
    python(
        manage_py,
        'make',
        '-vnone',

        # To simplify this test (in particular environment setup), do a static
        # link.
        '--library-types=static',
    )

# Build a program that uses both and run it
subprocess.check_call(['gprbuild', '-q', '-Pmain.gpr', '-p',
                       '-XLIBRARY_TYPE=static'])
subprocess.check_call([os.path.join('obj', 'main')])

print('Done')
