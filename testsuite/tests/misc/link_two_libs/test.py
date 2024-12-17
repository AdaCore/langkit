"""
Test that two generated libraries can live in the same program.
"""

import os.path
import subprocess
import sys


def python(*args):
    subprocess.check_call([sys.executable] + list(args))


# Generate two libraries
for lang in ('Foo', 'Bar'):
    manage_py = os.path.join(lang.lower(), 'manage.py')
    python("-m", "langkit.scripts.create_project", lang)
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
