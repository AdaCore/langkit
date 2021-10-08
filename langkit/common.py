from collections import defaultdict
import itertools
import string
from typing import DefaultDict, Iterator, Set, Union, overload

from langkit import names


ada_keywords = set("""
    abort abs abstract accept access aliased all and array at
    begin body
    case constant
    declare delay delta digits do
    else elsif end entry exception exit
    for function
    generic goto
    if in interface is
    limited loop
    mod
    new not null
    of or others out overriding
    package pragma private procedure protected
    raise range record rem renames requeue
    return reverse
    select separate some subtype synchronized
    tagged task terminate then type
    until use
    when while with xor
""".split())


def string_repr(string: str) -> str:
    """
    Return a representation of string as a literal, usable in the generated
    code.

    :param string: The string to represent.
    :return: A string literal representation of string.
    """
    return '"{0}"'.format(repr(string)[1:-1].replace('"', '""'))


# Build the set of characters that can appear as-is in the Ada source file (all
# printable ASCII characters except control codes).
ada_printable_bytes = set(
    b
    for b in string.printable.encode("ascii")
    if b >= 20
)
ada_printable_chars = set(chr(b) for b in ada_printable_bytes)


@overload
def common_string_repr(
    content: bytes,
    printable_chars: Set[int],
    newline: int,
    quote: int,
    indent: str = "",
) -> str: ...


@overload
def common_string_repr(
    content: str,
    printable_chars: Set[str],
    newline: str,
    quote: str,
    char_type: str,
    indent: str = "",
) -> str: ...


def common_string_repr(
    content: Union[str, bytes],
    printable_chars: Union[Set[str], Set[int]],
    newline: Union[str, int],
    quote: Union[str, int],
    char_type: str,
    indent: str = "",
) -> str:
    """
    Helper for ``bytes_repr`` and ``text_repr``.

    :param content: The string to represent.
    :param printable_chars: Set of characters that can be emitted as-is in the
        generated code.
    :param newline: Line feed character.
    :param quote: Double quote character.
    :param char_type: Name of the Ada character type for the string literal.
    :param indent: Indentation to include in each line in the generated code,
        except the first one.
    :return: The corresponding string literal or concatenation of literals.
    """
    result = indent + '"'
    open_string = True

    for c in content:

        # If we are about to write on a new line, add the indentation first
        if result[-1] == "\n":
            result += indent

        # If we have a printable character, include it as-is in the result
        if c in printable_chars:
            if not open_string:
                result += ' & "'
                open_string = True
            result += (chr(c) if isinstance(c, int) else c)
            if c == quote:
                result += '"'

        # Otherwise, use the Character'Val trick to generate the corresponding
        # character.
        else:
            if open_string:
                result += '"'
                open_string = False
            char_code = c if isinstance(c, int) else ord(c)
            result += f" & {char_type}'Val ({char_code})"

            # For readability, split at line feed
            if c == newline:
                result += "\n"

    if open_string:
        result += '"'

    return result


def bytes_repr(content: bytes, indent: str = "") -> str:
    """
    Return a representation of a bytes string as an Ada literal, usable in the
    generated code. Note that this may actually generate a concatenation if
    ``string`` contains non-printable characters.

    :param content: The string to represent.
    :param indent: Indentation to include in each line in the generated code,
        except the first one.
    :return: The corresponding string literal or concatenation of literals.
    """
    return common_string_repr(
        content, ada_printable_bytes, ord("\n"), ord('"'), "Character", indent
    )


def text_repr(content: str, indent: str = "") -> str:
    """
    Return a representation of a Unicode string as an Ada literal, usable in
    the generated code. Note that this may actually generate a concatenation if
    ``string`` contains non-printable characters.

    :param content: The string to represent.
    :param indent: Indentation to include in each line in the generated code,
        except the first one.
    :return: The corresponding string literal or concatenation of literals.
    """
    return common_string_repr(
        content, ada_printable_chars, "\n", '"', "Character_Type", indent
    )


def comment_box(label: str, column: int = 3) -> str:
    """
    Return an Ada comment for the given label.

    :param label: Single-line label.
    :param column: Indentation level.
    """
    return ('{line}\n'
            '{indent}-- {label} --\n'
            '{indent}{line}'.format(line='-' * (6 + len(label)),
                                    indent=' ' * column,
                                    label=label))


def is_keyword(name: Union[str, names.Name]) -> bool:
    """
    Returns wether `name` is an Ada keyword.

    :param name: The name we want to test.
    """

    str_name = name.lower if isinstance(name, names.Name) else name
    assert isinstance(str_name, str)

    return str_name.lower() in ada_keywords


__next_ids: DefaultDict[str, Iterator[int]] = (
    defaultdict(lambda: itertools.count(0))
)


def gen_name(var_name: Union[str, names.Name]) -> names.Name:
    """
    Generates a unique name from var_name.

    :param str|names.Name var_name: The base name. If it is a string,
        it needs to be a lower case with underscores string.
    :rtype: names.Name
    """
    if isinstance(var_name, str):
        var_name = names.Name.from_lower(var_name)

    var_id = next(__next_ids[var_name.lower])
    return var_name + names.Name(str(var_id))
