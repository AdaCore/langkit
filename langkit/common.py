from collections import defaultdict
import itertools

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


def string_repr(string):
    """
    Return a representation of string as a literal, usable in the generated
    code.
    :param str string: The string to represent.
    :return: A string literal representation of string.
    """
    return '"{0}"'.format(repr(string)[1:-1].replace('"', '""'))


def comment_box(label, column=3):
    """
    Return an Ada comment for the given label.

    :param str label: Single-line label.
    :rtype: str
    """
    return ('{line}\n'
            '{indent}-- {label} --\n'
            '{indent}{line}'.format(line='-' * (6 + len(label)),
                                    indent=' ' * column,
                                    label=label))


def is_keyword(name):
    """
    Returns wether `name` is an Ada keyword.

    :param str|names.Name name: The name we want to test.
    :rtype: bool
    """

    str_name = name.lower if isinstance(name, names.Name) else name
    assert isinstance(str_name, str)

    return str_name.lower() in ada_keywords


__next_ids = defaultdict(lambda: itertools.count(0))


def gen_name(var_name):
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
