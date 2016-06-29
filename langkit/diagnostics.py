import enum
from os import path
import traceback

from langkit.utils import Colors, assert_type, col


class Diagnostics(object):
    """
    Holder class that'll store the language definition source dir. Meant to
    be called by manage before functions depending on knowing the language
    source dir can be called.
    """
    lang_source_dir = "<invalid dir>"
    has_pending_error = False

    @classmethod
    def set_lang_source_dir(cls, lang_source_dir):
        """
        Set the language definition source directory.
        :type lang_source_dir: str
        """
        cls.lang_source_dir = lang_source_dir


class Location(object):
    """
    Holder for a location in the source code.
    """

    def __init__(self, file, line, text):
        self.file = file
        self.line = line
        self.text = text

    def __repr__(self):
        return "<Location {} {}>".format(self.file, self.line)

    def __str__(self):
        return 'File "{}", line {}'.format(self.file, self.line)


def extract_library_location():
    """
    Use traceback to extract the location of the definition of an entity in
    the language definition using langkit. This relies on
    LangSourceDir.lang_source_dir being set.

    :rtype: Location
    """
    l = [Location(t[0], t[1], t[3])
         for t in traceback.extract_stack()
         if Diagnostics.lang_source_dir in path.abspath(t[0])
         and "manage.py" not in t[0]]
    return l[-1] if l else None


context_stack = []
"""
:type: list[(str, Location)]
"""


class Context(object):
    """
    Like "context", but can be used more than once.
    """

    def __init__(self, message, location):
        self.message = message
        self.location = location

    def __enter__(self):
        context_stack.append((self.message, self.location))

    def __exit__(self, value, typ, traceback):
        del value
        del typ
        del traceback
        context_stack.pop()


class DiagnosticError(Exception):
    pass


class Severity(enum.IntEnum):
    """
    Severity of a diagnostic. For the moment we have two levels, warning and
    error. A warning won't end the compilation process, and error will.
    """
    warning = 1
    error = 2
    non_blocking_error = 3

SEVERITY_COLORS = {
    Severity.warning:            Colors.YELLOW,
    Severity.error:              Colors.RED,
    Severity.non_blocking_error: Colors.RED,
}


def format_severity(severity):
    """
    :param Severity severity:
    """
    msg = ('Error'
           if severity == Severity.non_blocking_error else
           severity.name.capitalize())
    return col(msg, Colors.BOLD + SEVERITY_COLORS[severity])


def check_source_language(predicate, message, severity=Severity.error):
    """
    Check predicates related to the user's input in the input language
    definition. Show error messages and eventually terminate if those error
    messages are critical.

    :param bool predicate: The predicate to check.
    :param str message: The base message to display if predicate happens to
        be false.
    :param Severity severity: The severity of the diagnostic.
    """
    # PyCharm's static analyzer thinks Severity.foo is an int because of the
    # class declaration, it it's really an instance of Severity instead.
    severity = assert_type(severity, Severity)
    if not predicate:
        for ctx_msg, ctx_loc in context_stack:
            print ('File "{file}", line {line}, {msg}'.format(
                file=col(ctx_loc.file, Colors.CYAN),
                line=col(ctx_loc.line, Colors.CYAN),
                msg=ctx_msg
            ))
        print "{}{}: {}".format(
            "    " if context_stack else '',
            format_severity(severity),
            message
        )
        if severity == Severity.error:
            raise DiagnosticError()
        elif severity == Severity.non_blocking_error:
            Diagnostics.has_pending_error = True


def check_multiple(predicates_and_messages, severity=Severity.error):
    """
    Helper around check_source_language, check multiple predicates at once.

    :param list[(bool, str)] predicates_and_messages: List of diagnostic
        tuples.
    :param Severity severity: The severity of the diagnostics.
    """
    for predicate, message in predicates_and_messages:
        check_source_language(predicate, message, severity)


def errors_checkpoint():
    """
    If there was a non-blocking error, exit the compilation process.
    """
    if Diagnostics.has_pending_error:
        raise DiagnosticError()
