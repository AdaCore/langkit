from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

import gdb
import gdb.printing

from langkit.gdb.tdh import TDH
from langkit.gdb.units import AnalysisUnit
from langkit.gdb.utils import record_to_tag, tagged_field


class GDBPrettyPrinters(gdb.printing.PrettyPrinter):
    """
    Holder for all pretty printers.
    """

    def __init__(self, context):
        super(GDBPrettyPrinters, self).__init__(context.lib_name, [])
        self.context = context

    def append(self, printer_cls):
        self.subprinters.append(GDBSubprinter(printer_cls, self.context))

    def __call__(self, value):
        """
        If there is one enabled pretty-printer that matches `value`, return an
        instance of PrettyPrinter tied to this value. Return None otherwise.
        """
        for printer in self.subprinters:
            if printer.enabled and printer.matches(value):
                return printer.instantiate(value)
        return None


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    """Holder for PrettyPrinter subclasses."""

    def __init__(self, cls, context):
        super(GDBSubprinter, self).__init__(cls.name)
        self.cls = cls
        self.context = context

    def matches(self, value):
        """Return whether this pretty-printer matches `value`, a GDB value."""
        return self.cls.matches(value, self.context)

    def instantiate(self, value):
        return self.cls(value, self.context)


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

    def __init__(self, value, context):
        self.context = context
        self.value = value

    @classmethod
    def matches(cls, value, context):
        """
        Return whether this pretty-printer matches `value`, a GDB value.
        """
        raise NotImplementedError()

    def to_string(self):
        raise NotImplementedError()


class ASTNodePrinter(BasePrinter):
    """
    Pretty-printer for AST nodes.
    """

    @classmethod
    def matches(cls, value, context):
        return (value.type.code == gdb.TYPE_CODE_PTR
                and value.type.target().code == gdb.TYPE_CODE_STRUCT
                and (value.type.target().name
                     in context.astnode_struct_names))

    @property
    def kind(self):
        tag = record_to_tag(self.value.dereference())
        return self.context.tags_mapping.get(tag, '???')

    @property
    def unit(self):
        return AnalysisUnit(tagged_field(self.value, 'unit'))

    def to_string(self):
        filename = self.unit.filename
        if filename:
            filename = os.path.basename(filename)

        tdh = TDH(tagged_field(self.value, 'unit')['tdh'])
        start = int(tagged_field(self.value, 'token_start_index'))
        end = int(tagged_field(self.value, 'token_end_index'))
        return '<{} {}{}-{}>'.format(
            self.kind,
            '{}:'.format(filename) if filename else '',
            tdh.token(start).sloc_range.start,
            tdh.token(end).sloc_range.end
        )
