"""
Test the parsing of regular expressions in lexer specifications.
"""

from langkit.diagnostics import DiagnosticError, Location, diagnostic_context
from langkit.lexer.char_set import CharSet
from langkit.lexer.regexp import RegexpCollection


# Disable ellipsis for CharSet.__repr__, as we need full output in this
# testcase to check parsing.
CharSet._repr_ellipsis = False


for regexp in [
    # Mere escape sequences
    "\\",
    r"\u",
    r"\u0",
    r"\u1234",
    r"\U00012345",
    r"\U00012345a",
    # Ranges
    r"[]",
    r"[a]",
    r"[a-c]",
    r"[^a-c]",
    r"[^]",
    r"[a^]",
    r"[a-]",
    r"[-b]",
    r"[a-c-]",
    # Escape sequences in ranges
    r"[\]]",
    r"[\u1234]",
    r"[\u1234-\u1243]",
    # Invalid named pattern reference
    r"{}",
    r"{foo}",
    r"{a,}",
    # Repetition
    r"ab{2,5}",
    r"a{2,}",
    r"a{2,a}",
    r"a{2,a}",
    r"a{0,0}",
    r"a{0,1}",
    r"a{1,0}",
    r"a{1,1}",
    r"a{2,2}",
]:

    print("== {} ==".format(regexp))

    with diagnostic_context(Location.nowhere):
        lexer = RegexpCollection()
        try:
            parser = lexer._parse(regexp)
        except DiagnosticError:
            pass
        else:
            print(parser)
    print("")

print("Done")
