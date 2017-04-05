from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb


system_address = gdb.lookup_type('system.address')


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
