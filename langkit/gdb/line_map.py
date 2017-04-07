"""
Data structures for mapping from generated library source lines to the
properties DSL level.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb


class ParseError(Exception):
    def __init__(self, line_no, message):
        super(ParseError, self).__init__(
            'line {}: {}'.format(line_no, message)
        )


class LineMap(object):
    """
    Data structure for the mapping from generated library source lines and
    variables to the properties DSL level.
    """

    def __init__(self, context):
        self.context = context

        self.filename = None
        """
        :type: str|None

        Absolute path for the "$-analysis.adb" file, or None if we haven't
        found it.
        """

        self.properties = []
        """
        :type: list[Property]
        """

    @classmethod
    def parse(cls, context):
        """
        Try to parse the $-analysis.adb source file to extract mapping
        information from its GDB helpers directives. Print error messages on
        standard output if anything goes wrong, but always return a LineMap
        instance anyway.

        :rtype: LineMap
        """

        result = cls(context)

        # Look for the "$-analysis.adb" file using some symbol that is supposed
        # to be defined there.
        has_unit_sym = gdb.lookup_global_symbol(
            '{}__analysis__has_unit'.format(context.lib_name)
        )
        if not has_unit_sym:
            return result

        result.filename = has_unit_sym.symtab.fullname()
        with open(result.filename, 'r') as f:
            try:
                cls._parse_file(result, f)
            except ParseError as exc:
                print('Error while parsing directives in {}:'.format(
                    result.filename
                ))
                print(str(exc))

        return result

    def _parse_file(self, f):
        """
        Internal method. Read GDB helpers directives from the "f" source file
        and fill self according to it. Raise a ParseError if anything goes
        wrong.

        :param file f: Readable file for the $-analysis.adb source file.
        :rtype: None
        """
        self.properties = []
        scope_stack = []

        for line_no, line in enumerate(f, 1):
            line = line.strip()
            if not line.startswith('--#'):
                continue
            line = line[3:].strip()
            chunks = line.split(None, 1)
            if len(chunks) == 0:
                raise ParseError(line_no, 'directive name is missing')
            name = chunks[0]
            args = chunks[1] if len(chunks) > 1 else ''
            d = Directive.parse(line_no, name, args)

            if d.is_a(PropertyStart):
                if scope_stack:
                    raise ParseError(line_no, 'property-start directive not'
                                     ' allowed inside another property')
                p = Property(LineRange(d.line_no, None), d.name, d.dsl_sloc)
                self.properties.append(p)
                scope_stack.append(p)

            elif d.is_a(ScopeStart):
                if not scope_stack:
                    raise ParseError(line_no, 'scope-start directive not'
                                              ' allowed outside a property')
                scope_stack.append(Scope(LineRange(d.line_no, None)))

            elif d.is_a(End):
                if not scope_stack:
                    raise ParseError(line_no, 'no scope to end')
                ended_scope = scope_stack.pop()
                ended_scope.line_range.last_line = d.line_no
                if scope_stack:
                    scope_stack[-1].events.append(ended_scope)

            elif d.is_a(BindDirective):
                if not scope_stack:
                    raise ParseError(line_no, 'no scope for binding')
                scope_stack[-1].events.append(Bind(d.line_no, d.dsl_name,
                                                   d.gen_name))

            else:
                raise NotImplementedError('Unknown directive: {}'.format(d))

        if scope_stack:
            raise ParseError(line_no, 'end of scope expected before end of'
                                      ' file')

    def lookup_property(self, line_no):
        """
        Look for a property that covers the given source line number. Return
        None if there is no such property.

        :param int line_no: Line number to lookup.
        :rtype: None|Property
        """
        for p in self.properties:
            if line_no < p.line_range.first_line:
                break
            elif line_no in p.line_range:
                return p


class LineRange(object):
    """
    Range of lines in the $-analysis.adb source file.
    """

    def __init__(self, first_line, last_line):
        self.first_line = first_line
        self.last_line = last_line

    def __contains__(self, line_no):
        assert isinstance(line_no, int)
        return self.first_line <= line_no <= self.last_line

    def __repr__(self):
        return '<LineRange {}>'.format(str(self))

    def __str__(self):
        return '{}-{}'.format(self.first_line, self.last_line)


class Scope(object):
    def __init__(self, line_range, label=None):
        self.line_range = line_range
        self.label = label
        self.events = []

    @property
    def subscopes(self):
        return [e for e in self.events if isinstance(e, Scope)]

    def __repr__(self):
        return '<{}{} {}>'.format(
            type(self).__name__,
            ' {}'.format(self.label) if self.label else '',
            self.line_range
        )


class Property(Scope):
    def __init__(self, line_range, name, dsl_sloc):
        super(Property, self).__init__(line_range, name)
        self.name = name
        self.dsl_sloc = dsl_sloc


class Event(object):
    def __init__(self, line_no, entity=None):
        self.line_no = line_no
        self.entity = entity

    def __repr__(self):
        return '<Event line {}>'.format(self.line_no)


class Bind(Event):
    def __init__(self, line_no, dsl_name, gen_name):
        super(Bind, self).__init__(line_no, dsl_name)
        self.dsl_name = dsl_name
        self.gen_name = gen_name

    def __repr__(self):
        return '<Bind {}, line {}>'.format(self.dsl_name, self.line_no)


class Directive(object):
    """
    Holder for GDB helper directives as parsed from source files.
    """

    name_to_cls = {}

    def __init__(self, line_no):
        self.line_no = line_no

    def is_a(self, cls):
        return isinstance(self, cls)

    @classmethod
    def parse(cls, line_no, name, args):
        """
        Try to parse a GDB helper directive. Raise a ParseError if anything
        goes wrong.

        :param int line_no: Line number on which this directive appears in the
            source file.
        :param str name: Name of the directive.
        :param str args: Arguments for this directive.
        :rtype: Directive
        """
        try:
            subcls = cls.name_to_cls[name]
        except KeyError:
            raise ParseError(line_no, 'invalid directive: {}'.format(name))
        return subcls.parse(line_no, args)


class PropertyStart(Directive):
    def __init__(self, name, dsl_sloc, line_no):
        super(PropertyStart, self).__init__(line_no)
        self.name = name
        self.dsl_sloc = dsl_sloc

    @classmethod
    def parse(cls, line_no, args):
        name, dsl_sloc = args.split(None, 1)
        return cls(name, dsl_sloc, line_no)


class ScopeStart(Directive):
    @classmethod
    def parse(cls, line_no, args):
        return cls(line_no)


class BindDirective(Directive):
    def __init__(self, dsl_name, gen_name, line_no):
        super(BindDirective, self).__init__(line_no)
        self.dsl_name = dsl_name
        self.gen_name = gen_name

    @classmethod
    def parse(cls, line_no, args):
        dsl_name, gen_name = args.split(None, 1)
        return cls(dsl_name, gen_name, line_no)


class End(Directive):
    @classmethod
    def parse(cls, line_no, args):
        return cls(line_no)


Directive.name_to_cls.update({
    'property-start': PropertyStart,
    'scope-start': ScopeStart,
    'bind': BindDirective,
    'end': End,
})
