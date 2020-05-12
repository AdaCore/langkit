#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Langkit testsuite.
"""

from testsuite_support import LangkitTestsuite


if __name__ == '__main__':
    LangkitTestsuite().testsuite_main()
