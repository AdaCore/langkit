#! /usr/bin/env python

"""Setup configuration file for the Langkit framework."""

from __future__ import absolute_import, division, print_function

from distutils.core import setup
import os


ROOT_DIR = os.path.dirname(__file__)

if ROOT_DIR != '':
    os.chdir(ROOT_DIR)

# Run the setup tools
setup(
    name='Langkit',
    version='0.1-dev',
    author='AdaCore',
    author_email='report@adacore.com',
    url='https://www.adacore.com',
    description='A Python framework to generate language parsers',
    install_requires=['Mako', 'PyYAML', 'enum', 'enum34', 'funcy'],
    packages=['langkit',
              'langkit.expressions',
              'langkit.gdb',
              'langkit.stylechecks',
              'langkit.utils'],
    package_data={'langkit': [
        'support/*.adb', 'support/*.ads', 'support/*.gpr',
        'templates/*.mako', 'templates/*/*.mako'
    ]},
    scripts=[os.path.join('scripts', 'create-project.py')]
)
