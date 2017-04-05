from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb
import gdb.printing

from langkit.gdb.utils import record_to_tag


class GDBPrettyPrinters(gdb.printing.PrettyPrinter):
    """
    Holder for all pretty printers.
    """

    def append(self, subprinter):
        self.subprinters.append(subprinter)

    def __call__(self, val):
        """
        If there is one enabled pretty-printer that matches `val`, return an
        instance of PrettyPrinter tied to this value. Return None otherwise.
        """
        for printer in self.subprinters:
            if printer.enabled and printer.matches(val):
                return printer.instantiate(val)
        return None


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    """Holder for PrettyPrinter subclasses."""

    def __init__(self, cls, **kwargs):
        self.cls = cls
        for key, value in kwargs.iteritems():
            setattr(self, key, value)
        super(GDBSubprinter, self).__init__(cls.name)

    def matches(self, value):
        """Return whether this pretty-printer matches `value`, a GDB value."""
        return self.cls.matches(value, self)

    def instantiate(self, val):
        return self.cls(val, self)


class BasePrinter(object):
    """
    Base class for pretty-printers.

    Instances must comply to GDB's Pretty Printing API
    (https://sourceware.org/gdb/onlinedocs/gdb/Pretty-Printing-API.html).
    """

    name = None
    """
    Human-readable string to describe this pretty-printer.

    Subclasses must override this attribute.
    """

    def __init__(self, value, subprinter):
        self.subprinter = subprinter
        self.value = value

    @classmethod
    def matches(cls, value, subprinter):
        """
        Return whether this pretty-printer matches `value`, a GDB value.

        :param GDBSubprinter subprinter: The GDBSubprinter instance that does
        this query.
        :rtype: bool
        """
        raise NotImplementedError()

    def to_string(self):
        raise NotImplementedError()


class ASTNodePrinter(BasePrinter):
    """
    Pretty-printer for AST nodes.
    """

    @classmethod
    def matches(cls, value, subprinter):
        return (value.type.code == gdb.TYPE_CODE_PTR
                and value.type.target().code == gdb.TYPE_CODE_STRUCT
                and (value.type.target().name
                     in subprinter.astnode_struct_names))

    @property
    def kind(self):
        tag = record_to_tag(self.value.dereference())
        return self.subprinter.tags_mapping.get(tag, '???')

    def to_string(self):
        return '<{}>'.format(self.kind)
