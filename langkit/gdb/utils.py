from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb


system_address = gdb.lookup_type('system.address')


def ptr_to_int(ptr_value):
    """
    Convert an access GDB value into the corresponding Python integer.
    """
    return int(ptr_value.cast(system_address))


def record_to_tag(record_value):
    """
    Assuming "record_value" is a tagged record value, return the corresponding
    tag as a Python integer.
    """
    # First, get the ultimate parent record (i.e. record for base type)
    while True:
        try:
            record_value = record_value['_parent']
        except gdb.error:
            break

    # Now, the tag must be there
    return ptr_to_int(record_value['_tag'])
