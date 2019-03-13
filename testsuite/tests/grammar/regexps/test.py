"""
Test the parsing of regular expressions in lexer specifications.
"""

from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.lexer.char_set import CharSet
from langkit.lexer.regexp import RegexpCollection


# Disable ellipsis for CharSet.__repr__, as we need full output in this
# testcase to check parsing.
CharSet._repr_ellipsis = False


for regexp in [
    # Mere escape sequences
    '\\',
    r'\u',
    r'\u0',
    r'\u1234',
    r'\U00012345',
    r'\U00012345a',

    # Ranges
    r'[]',
    r'[a]',
    r'[a-c]',
    r'[^a-c]',
    r'[^]',
    r'[a^]',
    r'[a-]',
    r'[-b]',
    r'[a-c-]',

    # Escape sequences in ranges
    r'[\]]',
    r'[\u1234]',
    r'[\u1234-\u1243]',
]:

    print('== {} =='.format(regexp))

    lexer = RegexpCollection()
    try:
        parser = lexer._parse(regexp)
    except DiagnosticError:
        pass
    else:
        print(parser)
    print('')

print('Done')
