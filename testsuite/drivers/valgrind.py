import os.path


suppression_dir = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "valgrind_supp")
)


def suppression_file(name):
    """
    Return an absolute path to the Valgrind suppression file "`name`.supp" in
    the "valgrind_supp" testsuite directory.
    """
    return os.path.join(suppression_dir, "{}.supp".format(name))


def valgrind_cmd(argv, suppressions=None):
    """
    Wrap a command-line to run the program under Valgrind.

    :param argv: Command line to wrap.
    :param suppressions: List of names for suppression files to pass to
        Valgrind (see drivers.valgrind.suppression_file).
    """
    result = ["valgrind", "-q", "--leak-check=full", "--error-exitcode=2"]
    for s in suppressions or []:
        result.append("--suppressions={}".format(suppression_file(s)))
    return result + argv
