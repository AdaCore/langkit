"""
This package allows us to transform testcase expressions
into regular expressions.

Testcase expressions consist of semi-literal text with embedded
Python regular expressions. They are meant to be used when you
are trying to match a string with some variable parts. In such
cases, the testcase expressions may be easier to read than
Python regular expression.

As an example, consider the writing of gdb test where this following
ouput should be checked:

    (gdb) print val_from_addr
    $1 = (a => 7, b => 23)

To match the output "(a => 7, b => 23)", the following testcase expression
can be used:

    result = gdb.send("print val_from_addr")
    exp = compile_expression("(a => 7, b => 23)")
    if exp.match(result):
        print "PASSED"

This is equivalent to the following python regular expression:

    result = gdb.send("print val_from_addr")
    exp = re.compile("\(a\s*=>\s*7,\s*b\s*=>\s*23\)")
    if exp.match(result):
        print "PASSED"

... but the testcase expression is much easier to read, of course. In
the testcase expression, a space (" ") matches any space chain ("\s*"),
including new-line characters; this nicely solves the problems of
ignoring the formatting differences in the output. On the other
hand, the new-line character does NOT match whitespace characters.


Anonymous regexps:
******************

As said previously, you can also embed regexps into a testcase expression.
Suppose that, in the previous example, you just need to check that the
first member of the record ("a") equals 7 and that you do not care about the
rest of the output. The corresponding testcase expression would be:

    result = gdb.send("print val_from_addr")
    exp = compile_expression("a => 7 @/.*/")
    if exp.match(result):
        print "PASSED"

The embedded regexp consists of the escape character "@", followed by a
delimitor (can be any character except an alphanumeric character
or "_", e.g., "/"), the expression, and finally by the same delimitor.
Roughly, @xEXPRx.

The equivalent python expression would be:

    result = gdb.send("print val_from_addr")
    exp = re.compile("a\s*=>\s*7,.*")
    if exp.match(result):
        print "PASSED"

Named regexps:
**************

Some regular expressions can be used several times in the same testcase;
it can be clearer to use named regexps in this case. Suppose that, in the
example, you actually need to check that "a" and "b" are numbers, whatever
their value. You can build a named regexp named NUMBER, register it and
use it in your testcase expression. For example,

    add_named_regexp(NUMBER, "[0-9]+")
    result = gdb.send("print val_from_addr")
    exp = compile_expression("(a => @NUMBER, b => @NUMBER)")
    if exp.match(result):
        print "PASSED"

The equivalent python expression would be:

    result = gdb.send("print val_from_addr")
    exp = re.compile("\(a\s*=>\s*[0-9]+,\s*b\s*=>\s*[0-9]+\)")
    if exp.match(result):
        print "PASSED"

If you wish to follow a named regexp with an alphanumeric character, enclose
the name in set braces, as in

    exp = compile_expression("(a => @{HEX}0)")

which matches strings in which the value of a ends in '0'.


Default named regexps:
**********************

Some useful named regexps are already defined by default (see variable
named_regexp_repository below for the complete list).  For instance,
The "NUMBER" named regexp is defined as one-or-more decimal digits.
So, in the previous example, the call to add_named_regexp is not needed
and can be removed:

    result = gdb.send("print val_from_addr")
    exp = compile_expression("(a => @NUMBER, b => @NUMBER)")
    if exp.match(result):
        print "PASSED"


The @... regexp:
****************

The special regular expression @... matches any text (possibly none),
including new-line characters, as if defined as @/(.|\s)*/, but also
"merges" with adjacent newline characters (if any) in a particular way.
Specifically,

    * the sequence \n@... matches either the end of the input, or else
      arbitrary text beginning with a newline;
    * the sequence @...\n matches either the beginning of the input or else
      arbitrary text that ends with a newline;
    * the sequence \n@...\n matches arbitrary text that begins and ends with
      a newline.

For example, after

    exp = compile_expression("Line1\n@...\nLine2"),

exp will match any string that begins with the complete line 'Line1' followed
by the complete line 'Line2', separated by zero or more intervening lines.
Likewise, after

    exp = compile_expression("@...\nLine\n@..."),

exp will match any string that contains the complete line 'Line'.

Formal description:
*******************

The testcase expressions can be roughly formalized as followed:

test_expression ::= (simple_expression | named_regexp | dots
                     | anonymous regexp)*

<escape_character> ::= '@'
<escaped_escape_character> :== <escape_character> <escape_character>

 <simple expression>  ::= any string without <escape_character>

 <embedded_regexp> ::= (<named_regexp> | <anonymous_regexp>)

 <name_character>   ::= [a-zA-Z0-9_]
 <name>             ::= (<name_character>)+

 <delimitor>        ::= any character other than a <name_character>, the
                        <escape character>, or '.'
                        <anonymous_regexp> ::=
                            <escape_character> <D : delimitor>
                            <Python regexp not containing <D>> <D>

                        <named_regexp>     ::= <escape_character> {name}

                        <dots>             ::= <escape_character> [.]
"""

from __future__ import annotations

import re
from typing import List, Match, Pattern, Union

default_escape_character = '@'

named_regexp_repository = {
    "ARB":     r"(.|\n)*?",
    "NUMBER":  r"[0-9]+",
    "HEX":     r"[0-9a-f]+",
    "INTEGER": r"-?[0-9]+",
    "FLOAT":   r"[-+0-9.NnaINFinfe(a-fA-Fx)]+",
    "ADDRESS": r"(0x)?([0-9a-fA-F])+",

    # The following regexp matches an address on the heap as displayed
    # by GDB. On most platorms, the @ADDRESS named regexp would work,
    # but on some platforms, there is a symbol related to the address
    # being printed, and recent versions of GDB now print that symbol
    # name next to it.  By default, just match an address.  Once the
    # target is known, initialize_testcase routine will adjust the
    # regexp if necessary.
    "HEAP_ADDR": r"(0x)?([0-9a-fA-F])+",

    # Same thing for an address on the stack
    "STACK_ADDR": r"(0x)?([0-9a-fA-F])+",

    # The following regexp matches a NULL address as display by GDB.
    # On most systems, GDB displays null addresses as 0x0. But on
    # some systems, the executable defines a symbol at this NULL
    # address, and causes GDB to print the name of that symbol next
    # to the null address.  By default, just match 0x0.  Once the target
    # is known, initialize_testcase routine will adjust the regexp,
    # as necessary.
    "NULL_ADDR": r'0x0',
}


def add_named_regexp(name: str, value: str) -> None:
    """
    Add a named regexp to the repository.
    """
    named_regexp_repository[name] = value


def compile_expression(expression: str) -> Pattern[str]:
    """
    Convert the test expression EXPRESSION to a python regular expression,
    compile it and return the result.
    """
    regexp_expression = convert_expression(expression)
    return re.compile(regexp_expression)


def convert_expression(expression: str, anchored: bool = True) -> str:
    """Convert the test expression to a python regular expression.

    PARAMETERS
        expression: The quotemeta expression to convert into a regexp.
        anchored: If True (the default), the regular expression returned
            will match a string iff it matches the entire string. See
            REMARKS below for more details.

    REMARKS
        In most cases, the expression is meant to be used to verify
        that some output matches it exactly.  In particular, a string
        should match iff it matches from start to finish.  That's why
        the regular expression returned should be anchored by default.

        There are situation, however, where this is not desirable.
        We might want to convert part of an expression by itself, for
        instance, and insert it inside a larger expression.  In that
        case, the anchors would get in the way. In this situation,
        set "anchored" to False.

        Note that we also used to produce a regexp that would ignore
        any output before and after the EXPRESSION given by the user
        (by adding ".*" at both ends of the result).  But this prevents
        us from being able to verify that a given GDB command returns
        nothing.  So, we do not do that anymore.
    """
    result = ''
    rest_of_expression = expression

    if anchored:
        result += r'\A'
    while rest_of_expression:
        mat = regexp_pattern.search(rest_of_expression)
        assert mat
        result += convert_simple_expression(rest_of_expression[:mat.start(0)])
        rest_of_expression = rest_of_expression[mat.end(0):]
        result += convert_embedded_regexp(mat)
    if anchored:
        result += r'\Z'
    return result

# Private part
# The following procedures should not be needed outside of this package.

regexp_pattern = \
    re.compile(default_escape_character + r'(?:'
               r'\.\.\.(\n?)'         # Group 1: @...
               r'|\{(\w+)\}'          # Group 2: @{NAME}
               r'|(\w+)'              # Group 3: @NAME
               r'|(' + default_escape_character + ')'
                                      # Group 4: @@
               r'|([^.\w{}])(.+?)\5'  # Group 6: @/.../
               r')'
               r'|(\n)' + default_escape_character + r'\.\.\.(\n?)'
                                      # Group 7: \n@...
               r'|\Z',                # so .search always matches something
               re.DOTALL)


def convert_embedded_regexp(mat: Match[str]) -> str:
    """
    Return regular expression matched by match object MAT, which was produced
    from regexp_pattern.
    """
    if mat.group(1) is not None or mat.group(7):
        return convert_dotted_regexp(mat.group(7) or '',
                                     (mat.group(1) or '')
                                     + (mat.group(8) or ''))
    elif mat.group(2) is not None or mat.group(3) is not None:
        return convert_named_regexp((mat.group(2) or '') +
                                    (mat.group(3) or ''))
    elif mat.group(4) is not None:
        return default_escape_character
    elif mat.group(6) is not None:
        return "(?:" + mat.group(6) + ")"
    else:
        return ''


def convert_named_regexp(name: str) -> str:
    """
    Return regular expression for @NAME.
    """
    assert name in named_regexp_repository, "Unknown named regexp: " + name
    return '(?:' + named_regexp_repository[name] + ')'


def convert_dotted_regexp(fore: str, aft: str) -> str:
    """
    Return regular expression for FORE@...AFT.  FORE and AFT are either
    empty strings or newlines.
    """
    if fore:
        if aft:
            return r'\n(?:.*?\n)*'
        else:
            return r'(?:\Z|\n(?:.|\n)*?)'
    elif aft:
        return r'(?:\A|(?:.*\n)*?)'
    else:
        return r'(?:.|\n)*?'


def convert_simple_expression(text: str) -> str:
    """Convert simple expression (e.g. "bla bla") into a Python regexp.
    """
    regexp = re.escape(text)
    regexp = re.sub(r"(\\ )+", r"\\s*", regexp)
    return regexp


def flat_quotemeta(expr: Union[str, List[str]]) -> str:
    """Return the equivalent quotemeta expression.

    PARAMETERS
        expr: This parameter must be either a string or a list of strings.

    RETURN VALUE
        If expr is list, then join all its elements with a "\n" and return
        the resulting string.  Otherwise, return expr as is.
    """
    if isinstance(expr, list):
        expr = "\n".join(expr)
    return expr
