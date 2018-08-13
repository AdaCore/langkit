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
                 context.implname('analysis_unit_type'))
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

    @property
    def synthetic(self):
        """
        Return whether this node is synthetic.

        :rtype: bool
        """
        return int(tagged_field(self.value, 'token_start_index')) == 0

    def sloc(self, with_end=True):
        """
        Return the source location for this node as a string.

        This must not be called if the node is synthetic.

        :rtype: str
        """
        filename = self.unit.filename
        if filename:
            filename = os.path.basename(filename)

        tdh = TDH(tagged_field(self.value, 'unit')['tdh'])
        start = int(tagged_field(self.value, 'token_start_index'))
        end = int(tagged_field(self.value, 'token_end_index'))
        return '{}{}{}'.format(
            '{}:'.format(filename) if filename else '',

            tdh.token(start).sloc_range.start,

            '-{}'.format(tdh.token(end).sloc_range.end)
            if with_end and end else ''
        )

    @property
    def parent(self):
        """
        Return the parent node, or None if it's the root one.

        :rtype: gdb.Value
        """
        return tagged_field(self.value, 'parent')

    def node_to_string(self):
        if not self.value:
            return 'null'

        loc = ('synthetic from {}'.format(self.parent)
               if self.synthetic else self.sloc())
        return '{} {}'.format(self.kind, loc)

    def to_string(self):
        return '<{}>'.format(self.node_to_string())


class LexicalEnv(object):
    """
    Wrapper for Lexical_Env/Lexical_Env_Access values.
    """

    def __init__(self, value, context):
        self.context = context

        if self.matches_wrapper(value, context):
            self.value = value['env']
        else:
            self.value = value
        if not self.matches_access(self.value, context):
            raise ValueError('Invalid lexical env: {}'.format(self.value))

    @classmethod
    def matches_wrapper(cls, value, context):
        """
        Return whether `value` is a Lexical_Env value.

        :rtype: bool
        """
        return (value.type.code == gdb.TYPE_CODE_STRUCT and
                value.type.name == context.implname('ast_envs__lexical_env'))

    @classmethod
    def matches_access(cls, value, context):
        """
        Return whether `value` is a Lexical_Env_Access value.

        :rtype: bool
        """
        typ = value.type
        if typ.code == gdb.TYPE_CODE_TYPEDEF:
            return typ.name == context.implname('ast_envs__lexical_env_access')

        return (typ.code == gdb.TYPE_CODE_PTR and
                typ.target().code == gdb.TYPE_CODE_STRUCT and
                (typ.target().name
                 == context.implname('ast_envs__lexical_env_type')))

    @property
    def _variant(self):
        """
        Return the record variant that applies to this env getter.
        """
        # With GNAT encodings, GDB exposes the variant part as a field that is
        # an union. Sometimes it's half-decoded...
        try:
            outer_union = self.value['kind___XVN']
        except gdb.error:
            return self.value['S']

        if self.kind == 'primary':
            return outer_union['S0']

        inner_union = outer_union['O']['kind___XVN']
        field = {'orphaned': 'S1',
                 'grouped': 'S2',
                 'rebound': 'S3'}[self.kind]
        return inner_union[field]

    @property
    def kind(self):
        if not self.value:
            return 'primary'
        result = str(self.value['kind'])

        # For some reason, GDB can return here the qualified name for the
        # enumerator. Strip that.
        chunks = result.split('__')
        return chunks[-1]

    @property
    def node(self):
        return self._variant['node']

    @property
    def ref_count(self):
        return self.value['ref_count']

    def to_string(self):
        if not self.value:
            return 'null'

        if self.kind == 'primary':
            empty_env = gdb.lookup_global_symbol(
                self.context.implname('ast_envs__empty_env_record')
            )

            if self.value == empty_env.value().address:
                return '<LexicalEnv empty>'
            elif self.node:
                return '<LexicalEnv (primary) for {}>'.format(self.node)
            else:
                return '<LexicalEnv root>'

        elif self.kind == 'orphaned':
            return '<LexicalEnv (orphaned)>'

        elif self.kind == 'grouped':
            return '<LexicalEnv (grouped)>'

        elif self.kind == 'rebound':
            return '<LexicalEnv (rebound)>'

        else:
            return '<LexicalEnv (corrupted)]'


class LexicalEnvPrinter(BasePrinter):
    """
    Pretty-printer for lexical environments.
    """

    name = 'LexicalEnv'

    @classmethod
    def matches(cls, value, context):
        return LexicalEnv.matches_wrapper(value, context)

    @property
    def env(self):
        return LexicalEnv(self.value, self.context)

    def display_hint(self):
        kind = self.env.kind
        if kind == 'primary':
            return ''
        elif kind == 'grouped':
            return 'array'
        else:
            return 'map'

    def to_string(self):
        return self.env.to_string()

    def children(self):
        env = self.env
        if env.kind == 'orphaned':
            yield ('key', 'orphaned')
            yield ('value', env._variant['orphaned_env'])

        elif env.kind == 'grouped':
            # Manually decode the fat pointer that GDB gives us...
            grouped_envs = env._variant['grouped_envs']
            lower_bound = int(grouped_envs['P_BOUNDS']['LB0'])
            upper_bound = int(grouped_envs['P_BOUNDS']['UB0'])
            array_ptr = grouped_envs['P_ARRAY'].dereference()
            element_type = array_ptr.type.target()
            array = array_ptr.cast(element_type.array(lower_bound,
                                                      upper_bound))
            for i in range(lower_bound, upper_bound + 1):
                e = array[i]
                yield (str(i), e)

        elif env.kind == 'rebound':
            yield ('key', 'rebindings')
            yield ('value', env._variant['rebindings'])

            yield ('key', 'rebound_env')
            yield ('value', env._variant['rebound_env'])


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

    @property
    def is_dynamic(self):
        return self.value['dynamic']

    @property
    def _variant(self):
        """
        Return the record variant that applies to this env getter.
        """
        # With GNAT encodings, GDB exposes the variant part as a field that is
        # an union. Sometimes it's half-decoded...
        try:
            union = self.value['dynamic___XVN']
            return union['S1'] if self.is_dynamic else union['O']
        except gdb.error:
            return self.value['S']

    @property
    def env(self):
        """
        If this env getter is static, return the env it references. Otherwise,
        return None.
        """
        return self._variant['env'] if not self.is_dynamic else None

    @property
    def node(self):
        """
        If this env getter is dynamic, return the node use to resolve the
        reference. Otherwise, return None.
        """
        return self._variant['node'] if self.is_dynamic else None

    @property
    def resolver(self):
        """
        If this env getter is dynamic, return the corresponding resolver.
        Otherwise, return None.
        """
        return self._variant['resolver'] if self.is_dynamic else None

    @property
    def resolver_name(self):
        """
        If we can determine the name of the property for this resolver, return
        it. Return None otherwise.
        """
        m = re.match(r'0x[a-f0-9]+ <.*\.([a-z_]+)>', str(self.resolver))
        if m:
            return m.group(1)

    @property
    def image(self):
        """
        Return a description of the env getter as a string.
        """
        if self.is_dynamic:
            node = self.node
            resolver = self.resolver
            resolver_name = self.resolver_name
            return ('{}.{}'.format(node, resolver_name)
                    if resolver_name else
                    '{} ({})'.format(resolver, node))
        else:
            return str(self.env)

    def to_string(self):
        return 'EnvGetter ({})'.format(self.image)


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

    def to_string(self):
        return EnvGetterPrinter(self.value['getter'], self.context).image


class EntityPrinter(BasePrinter):
    """
    Pretty-printer for environment elements.
    """

    name = 'Entity'

    @classmethod
    def matches(cls, value, context):
        return (value.type.code == gdb.TYPE_CODE_STRUCT
                and value.type.name in context.entity_struct_names)

    @property
    def node(self):
        return self.value['node']

    def to_string(self):
        if not self.node:
            return 'null'

        node = ASTNodePrinter(self.node, self.context)
        rebindings = RebindingsPrinter(
            self.value['info']['rebindings'], self.context
        )
        return '<| {}{} |>'.format(
            node.node_to_string(),
            '' if rebindings.is_null else ' {}'.format(rebindings.inner)
        )


class RebindingsPrinter(BasePrinter):
    """
    Pretty-printer for lexical environments rebindings.
    """

    name = 'Rebindings'

    @classmethod
    def matches(cls, value, context):
        return (
            value.type.code == gdb.TYPE_CODE_PTR
            and value.type.target().code == gdb.TYPE_CODE_STRUCT
            and (value.type.target().name
                 == context.implname('ast_envs__env_rebindings_type'))
        )

    @property
    def is_null(self):
        """
        Return whether this contains no rebinding.
        """
        return not self.value

    @property
    def inner(self):
        """
        Return the description of the rebindings list as a string.
        """
        if self.is_null:
            return 'null'

        def rebinding_img(value):
            new_env = LexicalEnv(value['new_env'], self.context)
            return ASTNodePrinter(new_env.node,
                                  self.context).sloc(with_end=False)

        # Gather all Env_Rebindings_Type records, parents last
        rebindings = []
        r = self.value
        while r:
            rebindings.append(r)
            r = r['parent']

        return '[{}]'.format(', '.join(rebinding_img(r)
                                       for r in reversed(rebindings)))

    def to_string(self):
        return "<Rebindings {}>".format(self.inner)


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

        struct_typename = value.type.target().name
        return bool(struct_typename and
                    cls.element_typename(struct_typename, context))

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
