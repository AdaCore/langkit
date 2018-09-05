from __future__ import absolute_import, division, print_function

from collections import defaultdict
import itertools

from langkit import names

LANGUAGE = "ada"

languages_extensions = {
    "ada": "adb",
    "cpp": "cpp",
}

keywords = {
    "cpp": set("""
        align as alignof and and_eq asm auto
        bitand bitor bool break
        case catch char char16_t char32_t class compl concept const constexpr
        const_cast continue
        decltype default delete do double dynamic_cast
        else enum explicit export extern
        false float for friend
        goto
        if inline int
        long
        mutable
        namespace new noexcept not not_eq nullptr
        operator or or_eq
        private protected public
        register reinterpret_cast requires return
        short signed sizeof static static_assert static_cast struct switch
        template this thread_local throw true try typedef typeid typename
        union unsigned using
        virtual void volatile
        wchar_t while
        xor xor_eq
    """.split()),

    "ada": set("""
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
    """.split()),
}


def string_repr(string):
    """
    Return a representation of string as a literal, usable in the generated
    code.
    :param str string: The string to represent.
    :return: A string literal representation of string.
    """
    return '"{0}"'.format(repr(string)[1:-1].replace('"', '""'))


def is_keyword(name):
    """
    Returns wether `name` is a keyword given the chosen global language.

    :param str|names.Name name: The name we want to test.
    :rtype: bool
    """

    str_name = name.lower if isinstance(name, names.Name) else name
    assert isinstance(str_name, basestring)

    return str_name.lower() in keywords[LANGUAGE]


__next_ids = defaultdict(lambda: itertools.count(0))


def gen_name(var_name):
    """
    Generates a unique name from var_name.

    :param str|names.Name var_name: The base name. If it is a string,
        it needs to be a lower case with underscores string.
    :rtype: names.Name
    """
    if isinstance(var_name, basestring):
        var_name = names.Name.from_lower(var_name)

    var_id = next(__next_ids[var_name.lower])
    return var_name + names.Name(str(var_id))
