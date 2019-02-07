"""
Test that two generated libraries can live in the same program.
"""

from __future__ import absolute_import, division, print_function

import os.path
import subprocess
import sys

import langkit

from utils import add_gpr_path


def python(script, *args):
    subprocess.check_call([sys.executable, script] + list(args))


scripts_dir = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(langkit.__file__))),
    'scripts')

create_project_py = os.path.join(scripts_dir, 'create-project.py')
build_lksp = os.path.join(scripts_dir, 'build-langkit_support.py')


# Build Langkit_Support and make it available to gprbuild
python(build_lksp, '--build-dir=lksp', '-vnone', 'generate')
python(build_lksp, '--build-dir=lksp', '-vnone', 'build')
add_gpr_path(os.path.abspath(os.path.join('lksp', 'lib', 'gnat')))


# Generate two libraries
for lang in ('Foo', 'Bar'):
    manage_py = os.path.join(lang.lower(), 'manage.py')
    python(create_project_py, lang)
    python(manage_py, '-vnone',
           # To simplify this test (in particular environment setup), do a
           # static link.
           '--disable-shared', '--enable-static',

           # Don't generate a project for Langkit_Support as we want to use the
           # one we built earlier.
           '--no-langkit-support',

           '--build-dir={}/build'.format(lang.lower()),

           'make', '-P')

# Build a program that uses both and run it
subprocess.check_call(['gprbuild', '-q', '-Pmain.gpr', '-p',
                       '-XLIBRARY_TYPE=static'])
subprocess.check_call([os.path.join('obj', 'main')])

print('Done')
