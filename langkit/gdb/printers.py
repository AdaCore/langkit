from __future__ import annotations

import abc
import os.path
import re
from typing import Dict, Iterator, Optional, TYPE_CHECKING, Tuple, Type, Union

import gdb
import gdb.printing

from langkit.gdb.tdh import TDH
from langkit.gdb.units import AnalysisUnit
from langkit.gdb.utils import adaify_name
from langkit.utils import memoized


if TYPE_CHECKING:
    from langkit.gdb.context import Context


def match_struct_ptr(value: gdb.Value, struct_name: str) -> bool:
    """
    Return whether "value" is a pointer to a struct type of the given name.
    """
    t = value.type.strip_typedefs()
    return (
        t.code == gdb.TYPE_CODE_PTR
        and t.target().code == gdb.TYPE_CODE_STRUCT
        and t.target().name == struct_name
    )


class GDBPrettyPrinters(gdb.printing.PrettyPrinter):
    """
    Holder for all pretty printers.
    """

    def __init__(self, context: Context):
        super().__init__(context.lib_name, [])
        self.context = context

    def append(self, printer_cls: Type[BasePrinter]) -> None:
        self.subprinters.append(GDBSubprinter(printer_cls, self.context))

    def __call__(self, value: gdb.Value) -> Optional[BasePrinter]:
        """
        If there is one enabled pretty-printer that matches `value`, return an
        instance of PrettyPrinter tied to this value. Return None otherwise.
        """
        for printer in self.subprinters:
            assert isinstance(printer, GDBSubprinter)
            if printer.enabled and printer.matches(value):
                return printer.instantiate(value)
        return None


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    """Holder for PrettyPrinter subclasses."""

    def __init__(self, cls: Type[BasePrinter], context: Context):
        super().__init__(cls.name)
        self.cls = cls
        self.context = context

    def matches(self, value: gdb.Value) -> bool:
        """Return whether this pretty-printer matches `value`, a GDB value."""
        return self.cls.matches(value, self.context)

    def instantiate(self, value: gdb.Value) -> BasePrinter:
        return self.cls(value, self.context)


class BasePrinter(abc.ABC):
    """
    Base class for pretty-printers.

    Instances must comply to GDB's Pretty Printing API
    (https://sourceware.org/gdb/onlinedocs/gdb/Pretty-Printing-API.html).
    """

    name: str
    """
    Human-readable string to describe this pretty-printer.

    Subclasses must override this attribute.
    """

    def __init__(self, value: gdb.Value, context: Context):
        self.context = context
        self.value = value

    @classmethod
    @abc.abstractmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        """
        Return whether this pretty-printer matches `value`, a GDB value.
        """
        ...

    def display_hint(self) -> str | None:
        return None

    def children(self) -> Iterator[Tuple[str, Union[str, gdb.Value]]]:
        return iter([])

    @abc.abstractmethod
    def to_string(self) -> str:
        ...


class AnalysisUnitPrinter(BasePrinter):
    """
    Pretty-printer for AST nodes.
    """

    name = 'AnalysisUnit'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return match_struct_ptr(value, context.implname('analysis_unit_type'))

    def to_string(self) -> str:
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

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return match_struct_ptr(value, context.node_record)

    @property
    def kind(self) -> str:
        kind = int(self.value['kind'])
        try:
            node_name = self.context.astnode_kinds[kind]
        except KeyError:
            return '???'
        else:
            return node_name.camel

    @property
    def unit(self) -> AnalysisUnit:
        return AnalysisUnit(self.value['unit'])

    @property
    def synthetic(self) -> bool:
        """
        Return whether this node is synthetic.

        :rtype: bool
        """
        return int(self.value['token_start_index']) == 0

    def sloc(self, with_end: bool = True) -> str:
        """
        Return the source location for this node as a string.

        This must not be called if the node is synthetic.

        :rtype: str
        """
        filename = self.unit.filename
        if filename:
            filename = os.path.basename(filename)

        tdh = TDH(self.value['unit']['tdh'])
        start = int(self.value['token_start_index'])
        end = int(self.value['token_end_index'])
        return '{}{}{}'.format(
            '{}:'.format(filename) if filename else '',

            tdh.token(start).sloc_range.start,

            '-{}'.format(tdh.token(end).sloc_range.end)
            if with_end and end else ''
        )

    @property
    def parent(self) -> gdb.Value:
        """
        Return the parent node, or None if it's the root one.

        :rtype: gdb.Value
        """
        return self.value['parent']

    def node_to_string(self) -> str:
        loc = ('synthetic from {}'.format(self.parent)
               if self.synthetic else self.sloc())
        return '{} {}'.format(self.kind, loc)

    def to_string(self) -> str:
        if not self.value:
            return 'null'

        return '<{}>'.format(self.node_to_string())


class RecordAccessMatcher:
    """
    Helper to match GDB values that follow the record/access pattern.
    """

    def __init__(self,
                 record_type_name: str,
                 access_type_name: Optional[str]) -> None:
        self.record_type_name = record_type_name
        self.access_type_name = access_type_name

    def matches_record(self, value: gdb.Value, context: Context) -> bool:
        """
        Return whether ``value`` matches the record type.
        """
        return (value.type.code == gdb.TYPE_CODE_STRUCT
                and value.type.name == context.implname(self.record_type_name))

    def matches_access(self, value: gdb.Value, context: Context) -> bool:
        """
        Return whether ``value`` matches the access type.
        """
        assert self.access_type_name is not None
        typ = value.type
        if typ.code == gdb.TYPE_CODE_TYPEDEF:
            return typ.name == context.implname(self.access_type_name)

        return match_struct_ptr(value, context.implname(self.record_type_name))


class LexicalEnv:
    """
    Wrapper for Lexical_Env/Lexical_Env_Access values.
    """

    wrapper_type_name = 'langkit_support.lexical_envs.lexical_env'
    internal_matcher = RecordAccessMatcher('ast_envs.lexical_env_record',
                                           'ast_envs.lexical_env_access')

    def __init__(self, value: gdb.Value, context: Context):
        self.context = context

        if self.matches_wrapper(value, context):
            inner_record_name = context.implname(
                self.internal_matcher.record_type_name
            )
            inner_record_ptr = gdb.lookup_type(inner_record_name).pointer()
            self.value = value['env'].cast(inner_record_ptr)

        elif not self.matches_access(self.value, context):
            raise ValueError('Invalid lexical env: {}'.format(self.value))

        else:
            self.value = value

    @classmethod
    def matches_wrapper(cls, value: gdb.Value, context: Context) -> bool:
        """
        Return whether `value` is a Lexical_Env value.
        """
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and value.type.name == cls.wrapper_type_name
        )

    @classmethod
    def matches_access(cls, value: gdb.Value, context: Context) -> bool:
        """
        Return whether `value` is a Lexical_Env_Access value.

        :rtype: bool
        """
        return cls.internal_matcher.matches_access(value, context)

    @property
    def kind(self) -> str:
        if not self.value:
            return 'primary'
        result = str(self.value['kind'])

        # For some reason, GDB can return here the qualified name for the
        # enumerator. Strip that.
        chunks = result.split('__')
        return chunks[-1]

    @property
    def node(self) -> gdb.Value:
        return self.value['node']

    @property
    def ref_count(self) -> gdb.Value:
        return self.value['ref_count']

    def to_string(self) -> str:
        if not self.value:
            return 'null'

        if self.kind in ('static_primary', 'dynamic_primary'):
            empty_env = gdb.lookup_global_symbol(
                self.context.implname('ast_envs.empty_env_record')
            )
            assert empty_env is not None

            if self.value.address == empty_env.value().address:
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
            return '<LexicalEnv (corrupted)>'


class LexicalEnvPrinter(BasePrinter):
    """
    Pretty-printer for lexical environments.
    """

    name = 'LexicalEnv'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return LexicalEnv.matches_wrapper(value, context)

    @property
    def env(self) -> LexicalEnv:
        return LexicalEnv(self.value, self.context)

    def display_hint(self) -> str:
        kind = self.env.kind
        if kind == 'primary':
            return ''
        elif kind == 'grouped':
            return 'array'
        else:
            return 'map'

    def to_string(self) -> str:
        return self.env.to_string()

    def children(self) -> Iterator[Tuple[str, Union[str, gdb.Value]]]:
        env = self.env
        if env.kind == 'orphaned':
            yield ('key', 'orphaned')
            yield ('value', env.value['orphaned_env'])

        elif env.kind == 'grouped':
            # Manually decode the fat pointer that GDB gives us...
            grouped_envs = env.value['grouped_envs']
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
            yield ('value', env.value['rebindings'])

            yield ('key', 'rebound_env')
            yield ('value', env.value['rebound_env'])


class EnvGetterPrinter(BasePrinter):
    """
    Pretty-printer for env getters.
    """

    name = 'Env_Getter'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and (
                value.type.name
                == '{}.analysis.ast_envs.env_getter'.format(
                    context.lib_name
                )
            )
        )

    @property
    def is_dynamic(self) -> gdb.Value:
        return self.value['dynamic']

    @property
    def _variant(self) -> gdb.Value:
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
    def env(self) -> Optional[gdb.Value]:
        """
        If this env getter is static, return the env it references. Otherwise,
        return None.
        """
        return self._variant['env'] if not self.is_dynamic else None

    @property
    def node(self) -> Optional[gdb.Value]:
        """
        If this env getter is dynamic, return the node use to resolve the
        reference. Otherwise, return None.
        """
        return self._variant['node'] if self.is_dynamic else None

    @property
    def resolver(self) -> Optional[gdb.Value]:
        """
        If this env getter is dynamic, return the corresponding resolver.
        Otherwise, return None.
        """
        return self._variant['resolver'] if self.is_dynamic else None

    @property
    def resolver_name(self) -> Optional[str]:
        """
        If we can determine the name of the property for this resolver, return
        it. Return None otherwise.
        """
        m = re.match(r'0x[a-f0-9]+ <.*\.([a-z_]+)>', str(self.resolver))
        return m.group(1) if m else None

    @property
    def image(self) -> str:
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

    def to_string(self) -> str:
        return 'EnvGetter ({})'.format(self.image)


class ReferencedEnvPrinter(BasePrinter):
    """
    Pretty-printer for referenced environments.
    """

    name = 'Referenced_Env'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and (
                value.type.name
                == '{}.analysis.ast_envs.referenced_env'.format(
                    context.lib_name
                )
            )
        )

    def to_string(self) -> str:
        return EnvGetterPrinter(self.value['getter'], self.context).image


class EntityPrinter(BasePrinter):
    """
    Pretty-printer for environment elements.
    """

    name = 'Entity'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return (value.type.code == gdb.TYPE_CODE_STRUCT
                and value.type.name in context.entity_struct_names)

    @property
    def node(self) -> gdb.Value:
        return self.value['node']

    def to_string(self) -> str:
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
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return match_struct_ptr(
            value, "langkit_support.lexical_envs.env_rebindings_type"
        )

    @property
    def is_null(self) -> bool:
        """
        Return whether this contains no rebinding.
        """
        return not self.value

    @property
    def inner(self) -> str:
        """
        Return the description of the rebindings list as a string.
        """
        if self.is_null:
            return 'null'

        def rebinding_img(value: gdb.Value) -> str:
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

    def to_string(self) -> str:
        return "<Rebindings {}>".format(self.inner)


class StringPrettyPrinter(BasePrinter):
    """
    Pretty-printer for strings from properties.
    """

    name = 'String'

    @property
    def length(self) -> int:
        return int(self.value['length'])

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return match_struct_ptr(
            value, f"{context.analysis_prefix}string_record"
        )

    def to_string(self) -> str:
        return self.value["content"].format_string()


class SymbolPrettyPrinter(BasePrinter):
    """
    Pretty-printer for "fat" symbols.
    """

    name = 'Symbol'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return (
            value.type.code == gdb.TYPE_CODE_STRUCT
            and value.type.name == 'langkit_support.symbols.symbol_type'
        )

    def to_string(self) -> str:
        # Simply extract the thin symbol and the symbol table from the fat
        # symbol and perform the lookup.
        index = int(self.value["ts"])
        if index:
            table = self.value["table"].dereference()
            symbols = table["symbols"]["e"].dereference()
            return str(symbols[index])
        else:
            return "No_Symbol"


class ArrayPrettyPrinter(BasePrinter):
    """
    Pretty-printer for array nodes from properties.
    """

    name = 'Array'

    typename_suffix = '_array_record'

    @classmethod
    def element_typename(cls,
                         struct_typename: str,
                         context: Context) -> Optional[str]:
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
        else:
            return None

    @classmethod
    def value_to_element_typename(
        cls,
        value: gdb.Value,
        context: Context
    ) -> Optional[str]:
        """
        Like ``element_typename``, but working on an array value.
        """
        ptr = value.type.strip_typedefs()
        if (
            ptr.code != gdb.TYPE_CODE_PTR
            or ptr.target().code != gdb.TYPE_CODE_STRUCT
        ):
            return None

        struct = ptr.target()
        return (None
                if struct.name is None
                else cls.element_typename(struct.name, context))

    @property
    def length(self) -> int:
        return int(self.value['n'])

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return cls.value_to_element_typename(value, context) is not None

    def display_hint(self) -> str:
        return 'array'

    def to_string(self) -> str:
        elt_typename = self.value_to_element_typename(self.value, self.context)
        assert elt_typename is not None
        return '{} array of length {}'.format(
            adaify_name(self.context, elt_typename),
            self.length
        )

    def children(self) -> Iterator[Tuple[str, gdb.Value]]:
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

    def __init__(self, value: gdb.Value, context: Context):
        self.context = context
        self.value = value

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return (value.type.code == gdb.TYPE_CODE_STRUCT
                and value.type.name.endswith('.vector')
                and (set(f.name for f in value.type.fields())
                     == {'_tag', 'e', 'size', 'capacity', 'sv'}))

    @property  # type: ignore
    @memoized
    def fields(self) -> Dict[str, gdb.Value]:
        """
        Return a mapping: (field name -> field value) for all fields in this
        vector record.
        """
        return {f.name: self.value[f.name]
                for f in self.value.type.fields()
                if f.name}

    @property
    def element_type(self) -> gdb.Type:
        """
        Return the type of elements this vector contains.
        """
        array_access_type = self.fields['e'].type

        # Peel the typedef and then the access type
        assert (array_access_type.code == gdb.TYPE_CODE_TYPEDEF
                and array_access_type.name.endswith('.elements_array_access')
                and array_access_type.target().code == gdb.TYPE_CODE_PTR)
        array_type = array_access_type.target().target()

        # Peel the optional typedef and then the array type
        assert array_type.name.endswith('.internal_elements_array')
        if array_type.code == gdb.TYPE_CODE_TYPEDEF:
            array_type = array_type.target()
        assert array_type.code == gdb.TYPE_CODE_ARRAY
        return array_type.target()

    @property
    def package(self) -> str:
        """
        Return the name of the instantiation package for this vector.
        """
        type_name = self.value.type.name
        suffix = '.vector'
        assert type_name.endswith(suffix)
        return type_name[:-len(suffix)]

    @property
    def length(self) -> int:
        return int(self.fields['size'])

    def display_hint(self) -> str:
        return 'array'

    def to_string(self) -> str:
        return '{} vector of length {}'.format(
            adaify_name(self.context, self.element_type.name),
            self.length
        )

    def children(self) -> Iterator[Tuple[str, gdb.Value]]:
        if self.length <= 0:
            return

        symbol_name = '{}.small_vector_capacity'.format(self.package)
        symbol = gdb.lookup_global_symbol(symbol_name)
        assert symbol is not None
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


class TokenReferencePrinter(BasePrinter):
    """
    Pretty-printer for token references.
    """

    name = 'TokenReference'

    @classmethod
    def matches(cls, value: gdb.Value, context: Context) -> bool:
        return (value.type.code == gdb.TYPE_CODE_STRUCT
                and value.type.name == context.comname('token_reference'))

    def to_string(self) -> str:
        if not self.value['tdh']:
            return 'No_Token'

        tdh = TDH(self.value['tdh'])
        index = self.value['index']
        token = int(index['token'])
        trivia = int(index['trivia'])
        return str(tdh.get(token, trivia))
