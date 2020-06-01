## vim: filetype=makopython

"""
Helpers to deal with Python2/Python3 compatibility.

This module is a Libadalang implementation detail, so please do not use it
outside of Libadalang.
"""

import sys


python2 = sys.version_info.major == 2

if python2:
    bytes_type = str
    text_type = unicode
    is_int = lambda value: isinstance(value, (int, long))
    from StringIO import StringIO
else:
    bytes_type = bytes
    text_type = str
    is_int = lambda value: isinstance(value, int)
    from io import StringIO


def text_to_bytes(text):
    """
    If ``text`` is a Python3 string, return its conversion to bytes using the
    system's encoding. Return ``string`` unmodified otherwise.
    """
    if not python2 and isinstance(text, str):
        return text.encode()
    else:
        return text


def bytes_to_text(raw_bytes):
    """
    If ``raw_bytes`` is a Python3 bytes object, return its conversion to text
    using the system's encoding. Return ``raw_bytes`` unmodified otherwise.
    """
    if not python2 and isinstance(raw_bytes, bytes):
        return raw_bytes.decode()
    else:
        return raw_bytes


def unicode_character(char_code):
    """
    Given a character number, return the corresponding Unicode character (str
    instance in Python3, unicode instance in Python2).
    """
    if python2:
        return unichr(char_code)
    else:
        return chr(char_code)


def text_repr(unistr):
    """
    Given a unicode (Python2) or str (Python3) object, return a consistent
    human-readable representation for it.
    """
    if python2:
        # Remove the 'u' prefix
        assert isinstance(unistr, unicode)
        return repr(unistr)[1:]
    else:
        assert isinstance(unistr, str)
        return ascii(unistr)


def bytes_repr(bstr):
    """
    Given a str (Python2) or bytes (Python3) object, return a consistent
    human-readable representation for it.
    """
    if python2:
        # Add the 'b' prefix
        assert isinstance(bstr, str)
        return 'b' + repr(bstr)
    else:
        assert isinstance(bstr, bytes)
        return repr(bstr)
