#! /usr/bin/env python

"""Setup configuration file for the Langkit framework."""

from distutils.core import setup
import os


ROOT_DIR = os.path.dirname(__file__)

if ROOT_DIR != '':
    os.chdir(ROOT_DIR)

# Run the setup tools
setup(
    name='Langkit',
    version='0.1.0',
    author='AdaCore',
    author_email='report@adacore.com',
    url='https://www.adacore.com',
    description='A Python framework to generate language parsers',
    install_requires=['Mako', 'PyYAML', 'funcy', 'docutils', 'e3-core'],
    packages=['langkit',
              'langkit.expressions',
              'langkit.gdb',
              'langkit.lexer',
              'langkit.scripts',
              'langkit.stylechecks',
              'langkit.utils'],
    package_data={'langkit': [
        'coverage.css', 'support/*.adb', 'support/*.ads', 'support/*.gpr',
        'support/*.adc', 'templates/*.mako', 'templates/*/*.mako', 'py.typed',
        'adasat/src/*.ads', 'adasat/src/*.adb'
    ]},
    entry_points={
        "console_scripts": [
            "create-project.py = langkit.scripts.create_project:main",
            "generate-msvc-lib-file.py ="
            " langkit.scripts.generate_msvc_lib_file:main",
            "lkm = langkit.scripts.lkm:main",
        ]
    },
)
