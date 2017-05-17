from __future__ import absolute_import, division, print_function

import os.path
import re

import gdb
import gdb.printing

from langkit.gdb.tdh import TDH
from langkit.gdb.units import AnalysisUnit
from langkit.gdb.utils import adaify_name, tagged_field
from langkit.utils import memoized


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


class AnalysisUnitPrinter(BasePrinter):
    """
    Pretty-printer for AST nodes.
    """

    name = 'AnalysisUnit'

    @classmethod
    def matches(cls, value, context):
        return (
            value.type.code == gdb.TYPE_CODE_PTR
            and value.type.target().code == gdb.TYPE_CODE_STRUCT
            and (value.type.target().name ==
                 '{}__analysis__analysis_unit_type'.format(context.lib_name))
        )

    def to_string(self):
        if not self.value:
            return 'null'

        unit = AnalysisUnit(self.value.dereference())
        filename = unit.filename
        return '<AnalysisUnit{}>'.format(' {}'.format(filename)
                                         if filename else '')


class ASTNodePrinter(BasePrinter):
    """
    Pretty-printer for AST nodes.
    """

    name = 'ASTNode'

    tag_re = re.compile(r'0x[0-9a-f]+ <([a-z_]+).*>( \(.*\))?')

    @classmethod
    def matches(cls, value, context):
        t = value.type
        while t.code == gdb.TYPE_CODE_TYPEDEF:
            t = t.target()
        return (t.code == gdb.TYPE_CODE_PTR
                and t.target().code == gdb.TYPE_CODE_STRUCT
                and (t.target().name
                     in context.astnode_struct_names))

    @property
    def kind(self):
        result = None
        tag = tagged_field(self.value.dereference(), '_tag')
        m = self.tag_re.match(str(tag))
        if m:
            record_type_name = m.group(1)
            result = (
                self.context.astnode_struct_names.get(record_type_name).camel
            )
        return result or '???'

    @property
    def unit(self):
        return AnalysisUnit(tagged_field(self.value, 'unit'))

    def to_string(self):
        if not self.value:
            return 'null'

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
            tdh.token(end).sloc_range.end if end else None
        )


class LexicalEnvPrinter(BasePrinter):
    """
    Pretty-printer for lexical environments.
    """

    name = 'LexicalEnv'

    @classmethod
    def matches(cls, value, context):
        return (
            value.type.code == gdb.TYPE_CODE_PTR
            and value.type.target().code == gdb.TYPE_CODE_STRUCT
            and (
                value.type.target().name
                == '{}__analysis__ast_envs__lexical_env_type'.format(
                    context.lib_name
                )
            )
        )

    @property
    def node(self):
        return self.value['node']

    @property
    def ref_count(self):
        return self.value['ref_count']

    def to_string(self):
        if not self.value:
            return 'null'

        empty_env = gdb.lookup_global_symbol(
            '{}__analysis__ast_envs__empty_env'.format(self.context.lib_name)
        )

        if self.value == empty_env.value():
            return '<LexicalEnv empty>'
        elif self.node:
            return '<LexicalEnv for {}>'.format(self.node)
        elif self.ref_count == -1:
            return '<LexicalEnv root>'.format(self.node)
        else:
            return '<LexicalEnv synthetic>'


class EnvGetterPrinter(BasePrinter):
    """
    Pretty-printer for env getters.
    """

    name = 'Env_Getter'

    @classmethod
    def matches(cls, value, context):
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and (
                value.type.name
                == '{}__analysis__ast_envs__env_getter'.format(
                    context.lib_name
                )
            )
        )

    def to_string(self):
        if self.value['dynamic']:
            return '<EnvGetter dynamic>'
        else:
            # With GNAT encodings, GDB exposes the variant part as a field that
            # is an union. Sometimes it's half-decoded...
            try:
                union = self.value['dynamic___XVN']
                variant = union['O']
            except gdb.error:
                variant = self.value['S']
            return str(variant['env'])


class ReferencedEnvPrinter(BasePrinter):
    """
    Pretty-printer for referenced environments.
    """

    name = 'Referenced_Env'

    @classmethod
    def matches(cls, value, context):
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and (
                value.type.name
                == '{}__analysis__ast_envs__referenced_env'.format(
                    context.lib_name
                )
            )
        )

    @property
    def resolver_name(self):
        """
        If we can determine the name of the property for this resolver, return
        it. Return None otherwise.
        """
        resolver = self.value['resolver']
        m = re.match(r'0x[a-f0-9]+ <.*\.([a-z_]+)>', str(resolver))
        if m:
            return m.group(1)

    def to_string(self):
        from_node = self.value['from_node']
        resolver = self.value['resolver']

        resolver_name = self.resolver_name
        if resolver_name:
            return '{}.{}'.format(from_node, resolver_name)
        else:
            return '{} ({})'.format(resolver, from_node)


class EntityPrinter(BasePrinter):
    """
    Pretty-printer for environment elements.
    """

    name = 'Entity'

    @classmethod
    def matches(cls, value, context):
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and (
                value.type.name
                == '{}__analysis__ast_envs__entity'.format(
                    context.lib_name
                )
            )
        )

    @property
    def node(self):
        return self.value['el']

    def to_string(self):
        return ('<Entity for {}>'.format(self.node)
                if self.node else 'null')


class ArrayPrettyPrinter(BasePrinter):
    """
    Pretty-printer for array nodes from properties.
    """

    name = 'Array'

    typename_suffix = '_array_record'

    @classmethod
    def element_typename(cls, struct_typename, context):
        """
        Given the name of the structure that implements this array, return the
        type name for the element that this array contains. Return None if this
        is not an array.
        """
        prefix = context.analysis_prefix
        suffix = cls.typename_suffix

        if (struct_typename.startswith(prefix)
                and struct_typename.endswith(suffix)):
            return struct_typename[len(prefix):-len(suffix)]

    @property
    def length(self):
        return int(self.value['n'])

    @classmethod
    def matches(cls, value, context):
        if (value.type.code != gdb.TYPE_CODE_PTR
                or value.type.target().code != gdb.TYPE_CODE_STRUCT):
            return False

        return bool(cls.element_typename(value.type.target().name, context))

    def display_hint(self):
        return 'array'

    def to_string(self):
        return '{} array of length {}'.format(
            adaify_name(self.context,
                        self.element_typename(self.value.type.target().name,
                                              self.context)),
            self.length
        )

    def children(self):
        if self.length <= 0:
            return

        # For some reason, GDB thinks all access to "items" elements are
        # out-of-bounds, even though the bounds information seems correct. Do
        # some casting anyway to avoid noisy and useless warnings.
        items = self.value['items']
        items = items.cast(items.type.target().array(1, self.length))
        for i in range(1, self.length + 1):
            yield ('[{}]'.format(i), items[i])


class LangkitVectorPrinter(BasePrinter):
    """
    Pretty-printer for all Langkit vectors instantiations.
    """

    name = 'Langkit_Vectors'

    def __init__(self, value, context):
        self.context = context
        self.value = value

    @classmethod
    def matches(cls, value, context):
        return (value.type.code == gdb.TYPE_CODE_STRUCT
                and value.type.name.endswith('__vector')
                and (set(f.name for f in value.type.fields())
                     == {'_tag', 'e', 'size', 'capacity', 'sv'}))

    @property
    @memoized
    def fields(self):
        """
        Return a mapping: (field name -> field value) for all fields in this
        vector record.
        """
        return {f.name: self.value[f.name]
                for f in self.value.type.fields()}

    @property
    def element_type(self):
        """
        Return the type of elements this vector contains.
        """
        array_access_type = self.fields['e'].type

        # Peel the typedef and then the access type
        assert (array_access_type.code == gdb.TYPE_CODE_TYPEDEF
                and array_access_type.name.endswith('__elements_array_access')
                and array_access_type.target().code == gdb.TYPE_CODE_PTR)
        array_type = array_access_type.target().target()

        # Peel the typedef and then the array type
        assert (array_type.code == gdb.TYPE_CODE_TYPEDEF
                and array_type.name.endswith('__internal_elements_array')
                and array_type.target().code == gdb.TYPE_CODE_ARRAY)
        return array_type.target().target()

    @property
    def package(self):
        """
        Return the name of the instantiation package for this vector.
        """
        type_name = self.value.type.name
        suffix = '__vector'
        assert type_name.endswith(suffix)
        return type_name[:-len(suffix)]

    @property
    def length(self):
        return int(self.fields['size'])

    def display_hint(self):
        return 'array'

    def to_string(self):
        return '{} vector of length {}'.format(
            adaify_name(self.context, self.element_type.name),
            self.length
        )

    def children(self):
        if self.length <= 0:
            return

        symbol_name = '{}__small_vector_capacity'.format(self.package)
        symbol = gdb.lookup_global_symbol(symbol_name)
        small_vector_capacity = int(symbol.value())

        if self.length <= small_vector_capacity:
            items = self.fields['sv']
        else:
            items = self.fields['e'].dereference()

            array_type = items.type
            if array_type.code == gdb.TYPE_CODE_TYPEDEF:
                array_type = array_type.target()

            # Rectify the bound information for the array of elements, as GDB
            # lost it some way during the dereference.
            items = items.cast(array_type.target().array(1, self.length))

        for i in range(1, self.length + 1):
            yield ('[{}]'.format(i), items[i])
