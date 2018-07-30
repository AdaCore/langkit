from __future__ import absolute_import, division, print_function


def valgrind_cmd(argv):
    """
    Wrap a command-line to run the program under Valgrind.
    """
    return ['valgrind', '-q', '--leak-check=full',
            '--error-exitcode=2'] + argv
