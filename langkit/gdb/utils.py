from __future__ import absolute_import, division, print_function

import gdb

from langkit.names import Name
from langkit.utils import Colors, col


system_address = gdb.lookup_type('system__address')


def ptr_to_int(ptr_value):
    """
    Convert an access GDB value into the corresponding Python integer.
    """
    return int(ptr_value.cast(system_address))


def tagged_field(record_value, field_name):
    """
    Helper to look for a field in a tagged record.

    This is useful because we see tagged records in GDB as a record that
    contains another record (for fields from the parent type) that contains
    another record, etc.
    """
    while True:
        try:
            return record_value[field_name]
        except gdb.error:
            pass
        try:
            record_value = record_value['_parent']
        except gdb.error:
            raise gdb.error('There is no member {}'.format(field_name))


def record_to_tag(record_value):
    """
    Assuming "record_value" is a tagged record value, return the corresponding
    tag as a Python integer.
    """
    return ptr_to_int(tagged_field(record_value, '_tag'))


def expr_repr(expr):
    """
    Return a colored repr for an expression.

    :type expr: langkit.gdb.state.ExpressionEvaluation
    :rtype: str
    """
    return col(expr.expr_repr, Colors.CYAN)


def name_repr(expr):
    """
    Return a colored repr for a binding name.

    :type expr: langkit.gdb.state.Binding
    :rtype: str
    """
    return col(expr.dsl_name, Colors.GREEN)


def prop_repr(prop):
    """
    Return a colored repr for a property name.

    :type prop: langkit.gdb.debug_info.Property
    :rtype: str
    """
    return col(col(prop.name, Colors.RED), Colors.BOLD)


def adaify_name(context, name):
    """
    Turn a symbol name like a__b into an Ada-like name such as A.B.
    Also strip the $LIB_NAME.Analysis prefix, if present.

    :type name: str
    :rtype: str
    """
    pfx = context.analysis_prefix
    if name.startswith(pfx):
        name = name[len(pfx):]
    chunks = name.split('__')
    return '.'.join(Name.from_lower(c).camel_with_underscores
                    for c in chunks)
