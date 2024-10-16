from __future__ import annotations

from collections import defaultdict
import itertools
import string
from typing import Iterator, overload

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
    content: str,
    printable_chars: set[str],
    newline: str,
    quote: str,
    char_type: str,
    indent: str = "",
) -> str: ...


@overload
def common_string_repr(
    content: bytes,
    printable_chars: set[int],
    newline: int,
    quote: int,
    char_type: str,
    indent: str = "",
) -> str: ...


def common_string_repr(
    content: str | bytes,
    printable_chars: set[str] | set[int],
    newline: str | int,
    quote: str | int,
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


def ascii_repr(content: str) -> str:
    """
    Return a representation of an ASCII string as an Ada literal (String),
    usable in the generated code. Note that this may actually generate a
    concatenation if ``string`` contains non-printable characters.

    :param string: The string to represent. It is a Python Unicode string, but
        must contain only codepoints in the ASCII range, as it targets Ada's
        String type.
    :return: The corresponding string literal or concatenation of literals.
    """
    return bytes_repr(content.encode("ascii"))


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


def is_keyword(name: str | names.Name) -> bool:
    """
    Returns wether `name` is an Ada keyword.

    :param name: The name we want to test.
    """

    str_name = name.lower if isinstance(name, names.Name) else name
    assert isinstance(str_name, str)

    return str_name.lower() in ada_keywords


__next_ids: dict[str, Iterator[int]] = defaultdict(lambda: itertools.count(0))


def gen_name(var_name: str | names.Name) -> names.Name:
    """
    Generates a unique name from var_name.

    :param var_name: The base name. If it is a string, it needs to be a lower
        case with underscores string.
    """
    if isinstance(var_name, str):
        var_name = names.Name.from_lower(var_name)

    var_id = next(__next_ids[var_name.lower])
    return names.Name(var_name.camel_with_underscores + str(var_id))


def ada_block_with_parens(
    lines: list[str],
    column: int,
    indent_first: bool = False,
    separator: str = ","
) -> str:
    """
    Format an Ada-style parenthetized multi-line block.

    For instance::

       ada_block_with_parens(["a => 1", "b => 2", column=0)

       "  (a => 1,
           b => 2);"

       ada_block_with_parens(["a => 1", "b => 2", column=3)

       "     (a => 1,
              b => 2);"

    :param lines: Individual lines to include in the generated block.
    :param column: Indentation level for that block.
    :param indent_first: Whether the result should have its first line
        indented.
    :param separator: Separator between elements.
    """
    indent = " " * column
    result = []
    for i, line in enumerate(lines):
        if i == 0:
            result.append(f"{indent if indent_first else ''}  ({line}")
        else:
            result[-1] += separator
            result.append(f"{indent}   {line}")
    result[-1] += ")"
    return "\n".join(result)


def ada_enum_type_decl(
    type_name: str | names.Name,
    value_names: list[str | names.Name],
    column: int,
    convention_c: bool = False,
) -> str:
    """
    Generate an Ada type declaration for an enumerated type.

    :param type_name: Name for this type declaration.
    :param value_names: Sequence of names for enumeration values.
    :param column: Indentation level for this type declaration.
    :param convention_c: Whether the type should have the "Convention => C"
        aspect.
    """
    result = f"type {type_name} is\n" + ada_block_with_parens(
        [str(v) for v in value_names], column, indent_first=True,
    )
    if convention_c:
        result += "\n" + " " * column + "with Convention => C"
    return result + ";"


def ada_pipe_list(lines: list[str], column: int) -> str:
    """
    Generate an Ada-style |-separated list of lines.

    :param lines: Lines to format.
    :param column: Indentation level for this list.
    """
    indent = " " * column
    return "\n".join(
        [
            (
                line
                if i == 0 else
                f"{indent}| {line}"
            ) for i, line in enumerate(lines)
        ]
    )
