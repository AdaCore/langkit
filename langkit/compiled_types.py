from __future__ import annotations

from collections import OrderedDict
from dataclasses import dataclass
import difflib
from itertools import count, takewhile
import pipes
from typing import (
    Callable, ClassVar, ContextManager, Dict, Iterator, List, Optional as Opt,
    Sequence, Set, TYPE_CHECKING, Tuple, Union, ValuesView
)

from langkit import names
from langkit.c_api import CAPISettings, CAPIType
from langkit.common import is_keyword
from langkit.compile_context import (CompileCtx, get_context,
                                     get_context_or_none)
from langkit.diagnostics import (
    Location, Severity, WarningSet, check_source_language, diagnostic_context,
    extract_library_location
)
from langkit.utils import issubtype, memoized, not_implemented_error
from langkit.utils.text import (append_paragraph, first_line_indentation,
                                indent)
from langkit.utils.types import TypeSet


if TYPE_CHECKING:
    from langkit.dsl import Annotations
    from langkit.envs import EnvSpec
    from langkit.expressions import (
        AbstractExpression, PropertyDef, ResolvedExpression,
    )
    from langkit.lexer import TokenAction
    from langkit.parsers import Parser, _Transform
    from langkit.unparsers import NodeUnparser


def gdb_helper(*args):
    """
    Format given arguments into a special Ada comment for GDB helpers.

    :param list[str] args: Elements of the special comment.
    :rtype: str
    """
    return ('--# {}'.format(' '.join(pipes.quote(a) for a in args))
            if get_context().emitter.generate_gdb_hook else '')


def precise_types_doc(types: List[CompiledType]) -> str:
    """
    Helper to format documentation about precise types.
    """
    return ', '.join(sorted(f':typeref:`{t.type_repo_name}`' for t in types))


@CompileCtx.register_template_extensions
def template_extensions(ctx):
    capi = ctx.c_api_settings
    root_entity = ctx.root_grammar_class.entity

    return {
        'names':                 names,
        'grammar_rule_type':     T.GrammarRule.c_type(capi).name,
        'default_grammar_rule':  capi.get_name('default_grammar_rule'),
        'root_entity':           root_entity,
        'entity_array':          root_entity.array.api_name,
        'ctx':                   ctx,
        'ada_lib_name':          ctx.ada_api_settings.lib_name,
        'T':                     T,
        'ada_api':               ctx.ada_api_settings,
        'capi':                  capi,
        'bool_type':             T.Bool.c_type(capi).name,
        'analysis_context_type': CAPIType(capi, 'analysis_context').name,
        'analysis_unit_type':    T.AnalysisUnit.c_type(capi).name,
        'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
        'node_type':             ctx.root_grammar_class.c_type(capi).name,
        'entity_type':           T.entity.c_type(capi).name,
        'symbol_type':           T.Symbol.c_type(capi).name,
        'string_type':           T.String.c_type(capi).name,
        'env_rebindings_type':   T.EnvRebindings.c_type(capi).name,
        'unit_kind_type':        T.AnalysisUnitKind.c_type(capi).name,

        'event_handler_type':   CAPIType(capi, 'event_handler').name,
        'event_handler_destroy_type':
            CAPIType(capi, 'event_handler_destroy_callback').name,
        'event_handler_unit_requested_type':
            CAPIType(capi, 'event_handler_unit_requested_callback').name,
        'event_handler_unit_parsed_type':
            CAPIType(capi, 'event_handler_unit_parsed_callback').name,

        'file_reader_type':      CAPIType(capi, 'file_reader').name,
        'file_reader_destroy_type':
            CAPIType(capi, 'file_reader_destroy_callback').name,
        'file_reader_read_type':
            CAPIType(capi, 'file_reader_read_callback').name,

        'unit_provider_type':    CAPIType(capi, 'unit_provider').name,
        'unit_provider_destroy_type':
            CAPIType(capi, 'unit_provider_destroy_callback').name,
        'unit_provider_get_unit_filename_type':
            CAPIType(capi, 'unit_provider_get_unit_filename_callback').name,
        'unit_provider_get_unit_from_name_type':
            CAPIType(capi,
                     'unit_provider_get_unit_from_name_callback').name,
        'token_kind':            CAPIType(capi, 'token_kind').name,
        'tdh_ptr_type':          CAPIType(capi, 'token_data_handler').name,
        'token_type':            CAPIType(capi, 'token').name,
        'sloc_type':             CAPIType(capi, 'source_location').name,
        'sloc_range_type':
            T.SourceLocationRange.c_type(capi).name,
        'text_type':             CAPIType(capi, 'text').name,
        'big_integer_type':      CAPIType(capi, 'big_integer').name,
        'diagnostic_type':       CAPIType(capi, 'diagnostic').name,
        'exception_type':        CAPIType(capi, 'exception').name,
        'exception_kind_type':   CAPIType(capi, 'exception_kind').name,

        'Field': Field,
        'TypeSet': TypeSet,
    }


class CompiledTypeRepo:
    """
    Namespace class: Repository for every compiled type. This is used to have a
    comprehensive list of every compiled type instance, so that you can use a
    TypeRepo instance to refer to any compiled type.
    """

    type_dict: Dict[str, CompiledType] = {}
    """
    Mapping: type name -> CompiledType instance. Used in TypeRepo for type
    lookup by name.
    """

    enum_types: List[EnumType] = []
    """
    List of EnumType instances. This list is updated every time a new instance
    is created.
    """

    astnode_types: List[ASTNodeType] = []
    """
    List of ASTNodeType instances. This list is updated every time a new
    instance is created.
    """

    struct_types: List[StructType] = []
    """
    List of all StructType instances.
    """

    pending_list_types: List[ASTNodeType] = []
    """
    Set of ASTNodeType instances for list types that are created while there
    is no context.
    """

    array_types: Set[ArrayType] = set()
    """
    Set of all created ArrayType instances.
    """

    iterator_types: List[IteratorType] = []
    """
    Set of all created IteratorType instances.
    """

    root_grammar_class: Opt[ASTNodeType] = None
    """
    The ASTNodeType instances used as a root type. Every other ASTNodeType
    instances must derive directly or indirectly from that class.
    """

    env_metadata: StructType
    """
    The StrucType instances used as metadata for the lexical environments
    values.
    """

    entity_info = None
    """
    The StructType instances to contain all entity information, except the node
    itself.

    :type: StructType
    """

    @classmethod
    def reset(cls):
        """
        Make this holder empty again. Useful to use Langkit multiple times in a
        process.
        """
        cls.type_dict = {}
        cls.enum_types = []
        cls.astnode_types = []
        cls.struct_types = []
        cls.pending_list_types = []
        cls.array_types = set()
        cls.iterator_types = []
        cls.root_grammar_class = None
        cls.env_metadata = None
        cls.entity_info = None


class AbstractNodeData:
    """
    This class defines an abstract base class for fields and properties on
    AST nodes.

    It defines the basis of what is needed to bind them in other languages
    bindings: a type and a name.
    """

    # Hack: the field declarations order in AST nodes matters.  The simple and
    # very handy syntax we use here for such declarations doesn't preserve this
    # order in Python2, however.  Waiting for the move to Python3, we use a
    # hack here: the following counter will help us to recover the declaration
    # order (assuming it is the same as the Field instantiation order).
    _counter: ClassVar[Iterator[int]] = iter(count(0))

    is_property: ClassVar[bool] = False
    """
    Whether this class is Property (to be overriden in the Property subclass).
    """

    is_user_field: ClassVar[bool] = False
    """
    Whether this class is UserField.
    """

    PREFIX_FIELD: ClassVar[names.Name] = names.Name('F')
    PREFIX_PROPERTY: ClassVar[names.Name] = names.Name('P')
    PREFIX_INTERNAL: ClassVar[names.Name] = names.Name('Internal')

    # Name to use for the implicit entity information argument in field
    # accessors.
    entity_info_name: ClassVar[names.Name] = names.Name('E_Info')

    def __init__(self,
                 name: Opt[names.Name] = None,
                 public: bool = True,
                 access_needs_incref: bool = False,
                 internal_name: Opt[names.Name] = None,
                 access_constructor: Opt[
                     Callable[
                         [
                             ResolvedExpression,
                             AbstractNodeData,
                             List[Opt[ResolvedExpression]],
                             Opt[AbstractExpression],
                         ],
                         ResolvedExpression,
                     ]
                 ] = None,
                 prefix: Opt[names.Name] = None,
                 final: bool = False):
        """
        :param name: Name for this field. Most of the time, this is initially
            unknown at field creation, so it is filled only at struct creation
            time.

        :param public: Whether this AbstractNodeData instance is supposed to be
            public or not.

            In the context of properties only, None is also allowed: in this
            case, inherit vibility from parents. If there is no property to
            override and None is passed, make the property private. This is
            computed in the "compute" pass.

        :param access_needs_incref: If True, field access evaluation does not
            create an ownership share: callers must call Inc_Ref themselves.
            See the eponym property.

        :param internal_name: If provided, override the default name to use in
            code generation for this node data.

        :param access_constructor: Optional callback to create a resolved
            expression that implements a DSL access to this field. If
            provided, this overrides the default code generation of
            ``FieldAccess``.

        :param prefix: Optional prefix to the name of a node data.

        :param final: If True, this field/property cannot be overriden. This is
            possible only for concrete fields/properties.
        """

        self._serial = next(self._counter)
        self._is_public = public

        self.location = extract_library_location()

        self.prefix: Opt[names.Name] = prefix
        self._name = name
        self._indexing_name = name.lower if name else None
        self._original_name = self._indexing_name

        assert internal_name is None or isinstance(internal_name, names.Name)
        self._internal_name = internal_name

        self.struct: Opt[StructType] = None
        """
        StructType subclass that declared this field. Initialized when creating
        StructType subclasses.
        """

        self.arguments: List[Argument] = []
        """
        Code generation-wise, all node data can be considered as functions
        which take at least a mandatory Self argument and return the
        corresponding data.

        This is a list that describes all other arguments. Note that only
        Property instances accept other arguments.
        """

        self._uses_entity_info = False
        self._has_self_entity = False
        self.optional_entity_info = False
        self._access_needs_incref = access_needs_incref
        self.abstract_default_value: Opt[AbstractExpression] = None
        self.default_value = None
        self.access_constructor = access_constructor

        self._abstract = False
        self.final = final

    @property
    def abstract(self) -> bool:
        """
        Return whether this field is abstract. This can be true only for
        properties and syntax fields.
        """
        return self._abstract

    @property
    def base(self) -> Opt[AbstractNodeData]:
        """
        If this field overrides an inherited one in a base class, return the
        inherited one, otherwise return None.
        """
        assert self._name and self.struct and self.struct.is_ast_node

        # Look for a potential field which has the same name as `self` in the
        # base struct.
        name_key = self._original_name
        parent_cls = self.struct.base
        parent_fields = (parent_cls.get_abstract_node_data_dict()
                         if parent_cls else {})
        return parent_fields.get(name_key, None)

    @property
    def root(self) -> AbstractNodeData:
        """
        Return the ultimate field that ``self`` inherits.

        This returns ``self`` itself if it is not overriding.
        """
        cursor = self
        while True:
            base = cursor.base
            if base is None:
                return cursor
            cursor = base

    @property
    def is_overriding(self) -> bool:
        """
        Return whether this field overrides an inheritted one in a base class.
        """
        return self.base is not None

    @property
    def uses_entity_info(self) -> bool:
        """
        Return whether evaluating this field requires entity info.
        """
        assert self._uses_entity_info is not None
        return self._uses_entity_info

    @property
    def diagnostic_context(self) -> ContextManager[None]:
        return diagnostic_context(self.location)

    @property
    def is_public(self) -> bool:
        """
        Whether this field is private.
        """
        assert self._is_public is not None
        return self._is_public

    @property
    def is_private(self) -> bool:
        """
        Whether this field is public.

        This is a shortcut for::

            not self.is_public
        """
        return not self.is_public

    @property
    def is_internal(self) -> bool:
        """
        Whether this property is internal.
        """
        return self.prefix == AbstractNodeData.PREFIX_INTERNAL

    @property
    def type(self) -> CompiledType:
        """
        Type of the abstract node field.
        """
        raise not_implemented_error(self, type(self).type)

    @type.setter
    def type(self, t: CompiledType) -> None:
        raise not_implemented_error(self, type(self).type)

    @property
    def public_type(self) -> CompiledType:
        return self.type.public_type

    def c_type_or_error(self, capi: CAPISettings) -> CAPIType:
        """
        Within a diagnostic context for this field, return its C API type.
        """
        with self.diagnostic_context:
            return self.public_type.c_type(capi)

    def _prefixed_name(self, name: names.Name) -> names.Name:
        """
        Decorate `name` with this AbstractNodeData's prefix.
        """
        assert name

        # If this is an internal property, the name has an underscore
        # prefix that we want to get rid of for code generation.
        radix = (names.Name(name.base_name[1:])
                 if self.is_internal else
                 name)

        return self.prefix + radix if self.prefix else radix

    @property
    def name(self) -> names.Name:
        assert self._name is not None
        return self._name

    @name.setter
    def name(self, name: names.Name) -> None:
        assert isinstance(name, names.Name)

        # Make sure that the original name is preserved
        assert self._original_name

        self._name = name

    @property
    def internal_name(self) -> Opt[names.Name]:
        """
        Name of the field in the generated code.
        """
        if self._internal_name:
            return self._internal_name
        else:
            return self.name

    @property
    def original_name(self) -> str:
        """
        Name for this property as specified in the DSL.

        For internal properties, we generally use impossible syntax (brackets,
        leading underscore, ...) to avoid name clashes with user-defined
        properties.
        """
        assert self._original_name
        return self._original_name

    @property
    def indexing_name(self) -> str:
        """
        Name to use for this node data in structure field dicts.

        For user fields created by users, this should be the lower case form
        that appears in the DSL. For fields created by the compilation process,
        this is arbitrary.
        """
        assert self._indexing_name
        return self._indexing_name

    @property
    def qual_impl_name(self) -> Union[None, str, names.Name]:
        """
        Fully qualified name for the implementation of this property.

        This is useful during code generation to avoid name clashes.
        """
        from langkit.expressions import PropertyDef

        if isinstance(self, PropertyDef):
            return '{}.Implementation{}.{}'.format(
                get_context().ada_api_settings.lib_name,
                '.Extensions' if self.user_external else '',
                self.internal_name
            )
        else:
            return self.name

    @property  # type: ignore
    @memoized
    def api_name(self) -> names.Name:
        """
        Return the name to use for this node data in public APIs.
        """
        assert self.is_public
        assert self._original_name

        # Unlike internal fields, we know that public fields have valid
        # original names, so it is safe to use them with Name.
        original_name = names.Name.from_lower(self.original_name)

        return self._prefixed_name(original_name)

    @property
    def qualname(self) -> str:
        """
        Return the qualified name for this field, i.e. the name of the owning
        type plus the name of the field itself. This is useful for diagnostic
        messages.

        Note that if expansion renamed this property, this will return the
        original (DSL-level) name.
        """
        struct_name = (
            self.struct.dsl_name if self.struct is not None else '<unresolved>'
        )
        field_name = (
            self.original_name if self._original_name else '<unresolved>'
        )
        return f"{struct_name}.{field_name}"

    def __repr__(self) -> str:
        return '<{} {}>'.format(
            type(self).__name__,
            self.qualname
        )

    @property
    def doc(self) -> str:
        """
        Documentation for the abstract node field.
        """
        raise not_implemented_error(self, type(self).doc)

    @property
    def accessor_basename(self) -> names.Name:
        """
        Return the base name for the accessor we generate for this field.

        Note that this is available only for fields attached to parse nodes.
        """
        assert isinstance(self.struct, ASTNodeType)
        return self.struct.kwless_raw_name + self.api_name

    @property
    def natural_arguments(self) -> List[Argument]:
        """
        Return the subset of "self.arguments" that are non-artificial
        arguments, that is to say the subset that users actually handle in
        expressions.

        This property makes sense in user-facing layers.
        """
        return self.arguments

    @property
    def access_needs_incref(self) -> bool:
        """
        Return whether field access evaluation does not create an ownership
        share. In this case, users must call Inc_Ref themselves. This returns
        always False for node data whose type is not ref-counted.
        """
        return self.type.is_refcounted and self._access_needs_incref


class CompiledType:
    """
    Descriptor for a type in the generated code.
    """

    def __init__(self,
                 name: Union[str, names.Name],
                 location: Opt[Location] = None,
                 doc: str = '',
                 base: Opt[CompiledType] = None,
                 is_ptr: bool = True,
                 has_special_storage: bool = False,
                 is_list_type: bool = False,
                 is_entity_type: bool = False,
                 exposed: bool = False,
                 c_type_name: Opt[str] = None,
                 external: bool = False,
                 null_allowed: bool = False,
                 is_ada_record: bool = False,
                 is_refcounted: bool = False,
                 nullexpr: Opt[str] = None,
                 py_nullexpr: Opt[str] = None,
                 java_nullexpr: Opt[str] = None,
                 element_type: Opt[CompiledType] = None,
                 hashable: bool = False,
                 has_equivalent_function: bool = False,
                 type_repo_name: Opt[str] = None,
                 api_name: Union[str, names.Name, None] = None,
                 dsl_name: Opt[str] = None,
                 conversion_requires_context: bool = False) -> None:
        """
        :param name: Type name. If a string, it must be camel-case.

        :param location: Location of the declaration of this compiled type, or
            None if this type does not come from a language specficication.

        :param str doc: User documentation for this type.

        :param base: If this type derives from another type T, this is T.

        :param is_ptr: Whether this type is handled through pointers only in
            the generated code.

        :param has_special_storage: Whether this type uses a special type for
            storage in structs and AST nodes.  If this is true, the following
            methods may be overriden:

              * storage_type_name;
              * storage_nullexpr;
              * extract_from_storage_expr;
              * convert_to_storage_expr.

        :param is_list_type: Whether this type is an AST node that is a list of
            AST nodes.

        :param is_entity_type: Whether this type represents an entity type.

        :param exposed: Whether the type should be exposed to the C and Python
            APIs. Note that all types are exposed anyway when the current
            context has the "library_all_fields_public" attribute set to True.

        :param c_type_name: Name for the type as it will appear in the C API.
            If left to None, `name` will be used instead.

        :param external: See CAPIType.__init__: this will be forwarded to it.

        :param null_allowed: Whether a client is allowed to create a null value
            for this type. Note that a type might have a nullexpr and still
            have null disallowed, because nullexpr might make sense for
            initialization purposes in the parsers, but not as a real null
            value.

        :param is_ada_record: Whether the type used in the C API is implemented
            as an Ada record. If so, we must pass them by reference in for C
            API interface functions.

        :param is_refcounted: Return whether this type matters for the
            ref-counting mechanism. See the "is_refcounted" method.

        :param nullexpr: Null expression to use in Ada for this type.  See the
            "nullexpr" method.

        :param py_nullexpr: Null expression to use in Python for this type. See
            the "py_nullexpr" method.

        :param java_nullexpr: Null expression to use in Java for this type.
            See the java_nullexpr method.

        :param element_type: If this is a collection type, must be the
            corresponding element type. Must be None otherwise.

        :param hashable: Whether this type has a Hash primitive, so that it can
            be used as a key in hashed maps/sets.

        :param has_equivalent_function: Whether testing equivalence for two
            values of this type must go through an Equivalent function. If not,
            code generation will use its "=" operator.

        :param type_repo_name: Name to use for registration in TypeRepo. The
            camel-case of "name" is used if left to None.

        :param api_name: If not None, must be the name of the type to use in
            the public Ada API. Strings are interpreted as camel case.

        :param dsl_name: If provided, name used to represent this type at the
            DSL level. Useful to format diagnostics.

        :param conversion_requires_context: Whether converting this type from
            public to internal values requires an analysis context.
        """
        if isinstance(name, str):
            name = names.Name.from_camel(name)
        if isinstance(api_name, str):
            api_name = names.Name.from_camel(api_name)

        self._name = name
        self.location = location
        self._doc = doc
        self._base = base
        self.is_ptr = is_ptr
        self.has_special_storage = has_special_storage
        self.is_list_type = is_list_type
        self.is_entity_type = is_entity_type
        self.c_type_name = c_type_name
        self.external = external
        self.exposed = exposed
        self.null_allowed = null_allowed
        self.is_ada_record = is_ada_record
        self._is_refcounted = is_refcounted
        self._nullexpr = nullexpr
        self._py_nullexpr = py_nullexpr
        self._java_nullexpr = java_nullexpr
        self._element_type = element_type
        self.hashable = hashable
        self._has_equivalent_function = has_equivalent_function
        self._requires_hash_function = False
        self._api_name = api_name
        self._dsl_name = dsl_name

        self.type_repo_name = type_repo_name or dsl_name or name.camel
        assert self.type_repo_name is not None
        CompiledTypeRepo.type_dict[self.type_repo_name] = self

        self._conversion_requires_context = conversion_requires_context

        # If this type does not have public/internal converters, these are not
        # used. Otherwise, they indicate whether these should be generated.
        self.to_public_converter_required = False
        self.to_internal_converter_required = False

        self.used_in_public_struct = False
        """
        Whether this type is used as a field in a struct that belongs to the
        public API.
        """

        self.derivations: Set[CompiledType] = set()
        """
        Set of types that derive from ``self``.
        """

        self._abstract_node_data_dict_cache: Dict[
            Tuple[bool, AbstractNodeData],
            Tuple[str, BaseField]
        ] = {}
        """
        Cache for the get_abstract_node_data_dict class method.
        """

        self._fields: Dict[str, BaseField] = OrderedDict()
        """
        List of AbstractNodeData fields for this type.
        """

        # If "self" derives from another type, register it
        if self._base is not None:
            self._base.derivations.add(self)

        self._iterator: Opt[IteratorType] = None
        """
        Iterator for "self". Created on-demand (see the "create_iterator"
        method and "iterator" property).
        """

    @property
    def base(self) -> Opt[CompiledType]:
        """
        Return the base of this type, or None if there is no derivation
        involved.
        """
        return self._base

    def get_inheritance_chain(self) -> List[CompiledType]:
        """
        Return the chain of types following the `base` link as a list.
        Root-most types come first.
        """
        t: Opt[CompiledType] = self
        result = []
        while t is not None:
            result.append(t)
            t = t.base
        return list(reversed(result))

    @property
    def mypy_type_hint(self) -> str:
        """
        Type hint to use for this type when generating Mypy stub files for the
        Python bindings.
        """
        return get_context().python_api_settings.type_public_name(self)

    def __lt__(self, other):
        assert isinstance(other, CompiledType)
        return self.name < other.name

    @property
    def public_type(self):
        """
        Return the type to use when exposing values in public APIs.

        This returns `self` for most types, but some (such as bare nodes) are
        automatically wrapped as entity.

        :rtype: langkit.compiled_types.CompiledType
        """
        return self

    @property
    def conversion_requires_context(self):
        return self._conversion_requires_context

    @property
    def has_equivalent_function(self):
        return self._has_equivalent_function

    @property
    def requires_hash_function(self):
        """
        Return whether code generation must produce a Hash function for this
        type.

        :rtype: bool
        """
        return self != T.env_md and self._requires_hash_function

    def require_hash_function(self):
        """
        Tag this type as requiring a hash function.
        """
        self._requires_hash_function = True

    def add_as_memoization_key(self, context):
        """
        Add `self` to the set of types that are used as keys in the hashed maps
        used to implement properties memoization. It has to be hashable.
        """
        assert self.hashable, 'Trying to use {} as hashable type'.format(
            self.dsl_name
        )
        context.memoization_keys.add(self)
        self.require_hash_function()

    def add_as_memoization_value(self, context):
        """
        Add `self` to the set of types that are used as values in the hashed
        maps used to implement properties memoization.
        """
        context.memoization_values.add(self)

    @property
    def memoization_kind(self):
        """
        Return the enumerator name that corresponds to this type for the
        discriminated record to materialize memoization keys/values.

        :rtype: str
        """
        return (names.Name('Mmz') + self.name).camel_with_underscores

    @property
    def name(self) -> names.Name:
        """
        Name of the type for general values in the Ada generated code.

        :rtype: names.Name
        """
        return self._name

    @property
    def api_name(self):
        """
        Name of public type. By default, it is the same as `name`.

        :rtype: names.Name
        """
        return self.name if self._api_name is None else self._api_name

    @property
    def dsl_name(self):
        """
        Type name as it appears in the DSL. To be used in diagnostics.

        :rtype: str
        """
        return self._dsl_name or self.name.camel

    @property
    def to_public_converter(self):
        """
        If this type requires a conversion in public properties, return the
        name of the function that takes an internal value and returns a public
        one. Return None otherwise.

        :rtype: names.Name|None
        """
        return None

    @property
    def to_internal_converter(self):
        """
        If this type requires a conversion in public properties, return the
        name of the function that takes a public value and returns an internal
        one. Return None otherwise.

        :rtype: names.Name|None
        """
        return None

    def to_public_expr(self, internal_expr):
        """
        Given ``internal_expr``, an expression that computes an internal value,
        for this type return another expression that converts it to a public
        value.

        :type internal_expr: str
        :rtype: str
        """
        if self.to_public_converter:
            return '{} ({})'.format(self.to_public_converter, internal_expr)
        else:
            # By default, assume public and internal types are identical, i.e.
            # that we can return the internal value as-is.
            assert self.name == self.api_name, (
                'Trying to avoid conversion while internal type is {} and'
                ' public type is {}'.format(self.name, self.api_name)
            )
            return internal_expr

    def to_internal_expr(self, public_expr, context=None):
        """
        Given ``public_expr``, an expression that computes a public value, for
        this type return another expression that converts it to an internal
        value.

        :type public_expr: str
        :param str|None context: If this type requires the context for this
            conversion, this must be an expression that yields the context.
        :rtype: str
        """
        requires_context = self.conversion_requires_context
        assert not (requires_context and context is None)

        if self.to_internal_converter:
            return '{} ({}{})'.format(
                self.to_internal_converter,
                public_expr,
                (', ' + context) if requires_context else ''
            )
        else:
            # By default, assume public and internal types are identical, i.e.
            # that we can return the internal value as-is.
            assert self.name == self.api_name
            return public_expr

    def __repr__(self):
        return '<CompiledType {}>'.format(self.name.camel)

    @property
    def diagnostic_context(self):
        return diagnostic_context(self.location)

    @property
    def doc(self):
        """
        Return the user documentation for this type, or None if there is no
        documentation.

        :rtype: None|str
        """
        return self._doc

    @property
    def is_analysis_unit_type(self):
        """
        Return whether this is the analysis unit type.
        """
        return self == T.AnalysisUnit

    @property
    def is_analysis_unit_kind(self):
        """
        Return whether this is the analysis unit kind type.
        """
        return self == T.AnalysisUnitKind

    @property
    def is_array_type(self):
        """
        Return whether this is an instance of ArrayType.
        """
        return isinstance(self, ArrayType)

    @property
    def is_iterator_type(self) -> bool:
        """
        Return whether this is an instance of IteratorType.
        """
        return isinstance(self, IteratorType)

    @property
    def is_bool_type(self):
        """
        Return whether this is the boolean type.

        :rtype: bool
        """
        return self == T.Bool

    @property
    def is_int_type(self):
        """
        Return whether this is the integer type.

        :rtype: bool
        """
        return self == T.Int

    @property
    def is_big_int_type(self):
        """
        Return whether this is the big integer type.

        :rtype: bool
        """
        return self == T.BigInt

    @property
    def is_enum_type(self):
        """
        Return whether this is an enumeration type.

        :rtype: bool
        """
        return isinstance(self, EnumType)

    @property
    def is_collection(self):
        """
        Return whether this is a collection type.

        :rtype: bool
        """
        return self._element_type is not None

    @property
    def is_env_rebindings_type(self):
        """
        Return whether this is the env rebindings type.

        :rtype: bool
        """
        return self == T.EnvRebindings

    @property
    def is_equation_type(self):
        """
        Return whether this is the equation type.

        :rtype: bool
        """
        return self == T.Equation

    @property
    def is_lexical_env_type(self):
        """
        Return whether this is the lexical environment type.

        :rtype: bool
        """
        return self == T.LexicalEnv

    @property
    def is_logic_var_type(self):
        """
        Return whether this is an instance of LogicVarType.

        :rtype: bool
        """
        return isinstance(self, LogicVarType)

    @property
    def is_long_type(self):
        """
        Return whether this is a integer type.

        :rtype: bool
        """
        return self == T.Int

    @property
    def is_character_type(self):
        """
        Return whether this is a character type.

        :rtype: bool
        """
        return self == T.Character

    @property
    def is_string_type(self):
        """
        Return whether this is the string type.

        :rtype: bool
        """
        return self == T.String

    @property
    def is_symbol_type(self):
        """
        Return whether this is a symbol type.

        :rtype: bool
        """
        return self == T.Symbol

    @property
    def is_token_type(self):
        """
        Return whether this is a token type.

        :rtype: bool
        """
        return self == T.Token

    @property
    def is_big_integer_type(self):
        """
        Return whether this is a big integer type.

        :rtype: bool
        """
        return self == T.BigInt

    @property
    def is_root_node(self) -> bool:
        """
        Return whether this is the root node.
        """
        return isinstance(self, ASTNodeType) and self._is_root_node

    @property
    def element_type(self):
        """
        Assuming this is a collection type (array, iterator, list) or an
        entity, return the corresponding element type.

        :rtype: CompiledType
        """
        assert self._element_type
        return self._element_type

    @property
    def is_refcounted(self):
        """
        Return whether this type matters for the ref-counting mechanism.

        This returns whether this type is dynamically allocated and thus
        ref-counted, or whether it owns ref-counted values. "dynamically" in
        this context means: with a lifetime that is different from the owning
        analysis unit.

        If it is ref-counted, there must exist Inc_Ref and Dec_Ref primitives.
        The Dec_Ref primitive must handle null values correctly (i.e. ignore
        them).

        :rtype: bool
        """
        return self._is_refcounted

    @property
    def storage_type_name(self):
        """
        Return the name of the type that is used to store instances of this
        type in structs and AST nodes. See documentation for
        has_special_storage.

        :rtype: str
        """
        return self.name

    def extract_from_storage_expr(self, node_expr, base_expr):
        """
        Turn a storage value into a public value. See documentation for
        has_special_storage.

        :param str node_expr: Expression that yields the node that owns the
            storage value.
        :param str base_expr: Expression that yields the storage value.
        :return: An expression that yields the public value.
        :rtype: str
        """
        return base_expr

    def convert_to_storage_expr(self, node_expr, base_expr):
        """
        Turn a public value into a storage value. See documentation for
        has_special_storage.

        :param str node_expr: Expression that yields the node that will own the
            resulting storage value.
        :param str base_expr: Expression that yields the public value:
        :return: An expression that yields the storage value.
        :rtype: str
        """
        return base_expr

    @property
    def nullexpr(self):
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.

        :rtype: str
        """
        if self._nullexpr is None:
            raise not_implemented_error(self, type(self).nullexpr)
        else:
            return self._nullexpr

    @property
    def py_nullexpr(self):
        """
        Return a string to be used in Python code gen for "null" expressions.

        Must be overridden in subclasses... for which we need the Python null
        expression.

        :rtype: str
        """
        if self._py_nullexpr is None:
            raise not_implemented_error(self, type(self).py_nullexpr)
        else:
            return self._py_nullexpr

    @property
    def java_nullexpr(self) -> str:
        """
        Return a string representing the null expression in generated
        Java code.

        Must be overridden in subclasses... for which we need the Java null
        expression.
        """
        if self._java_nullexpr is None:
            raise not_implemented_error(self, type(self).java_nullexpr)
        else:
            return self._java_nullexpr

    @property
    def storage_nullexpr(self):
        """
        Return the nullexpr that is used for fields of this type in structs and
        ASTNodes. See documentation for has_special_storage.

        :rtype: str
        """
        return self.nullexpr

    def c_type(self, c_api_settings):
        """
        Return a CAPIType instance for this type.

        :param CAPISettings c_api_settings: The settings for the C API.
        """
        return CAPIType(c_api_settings, self.c_type_name or self.name,
                        external=self.external)

    def unify(self, other, error_msg=None):
        """
        If `self` and `other` are types that match, return the most general
        type to cover both. Create an error diagnostic if they don't match.

        :param CompiledType other: Type to unify with `self`.
        :param str|None error_msg: Diagnostic message for mismatching types. If
            None, a generic one is used, otherwise, we call .format on it with
            the `self` and `other` keys being the names of mismatching types.
        :rtype: CompiledType
        """

        # ASTNodeType instances (and thus entities) always can be unified:
        # just take the most recent common ancestor.
        if self.is_entity_type and other.is_entity_type:
            return ASTNodeType.common_ancestor(self.element_type,
                                               other.element_type).entity
        elif self.is_ast_node and other.is_ast_node:
            return ASTNodeType.common_ancestor(self, other)

        # Otherwise, we require a strict subtyping relation
        check_source_language(
            self.matches(other),
            (error_msg or 'Mismatching types: {self} and {other}').format(
                self=self.dsl_name, other=other.dsl_name
            )
        )
        return self

    def matches(self, formal):
        """
        Return whether `self` matches `formal`.

        This is mere equality for all types but AST nodes, in which `self` is
        allowed to be a subtype for `formal`.

        :param CompiledType formal: Type to match. `self` is checked to be a
            subtype of it.
        :rtype: bool
        """

        if self.is_entity_type and formal.is_entity_type:
            return self.element_type.matches(formal.element_type)

        if formal.is_ast_node and self.is_ast_node:
            return formal in self.get_inheritance_chain()
        else:
            return self is formal

    # Memoize so that we have only one array type for each element type
    @property  # type: ignore
    @memoized
    def array(self) -> ArrayType:
        """
        Create an array type whose element type is `self`.
        """
        return ArrayType(self)

    def create_iterator(self, used: bool) -> IteratorType:
        """
        Create, if needed, the iterator type for "self".

        This creates the iterator type if it does not exist yet, and return the
        existing one otherwise.

        :param used: Whether the returned iterator type must be considered as
            used (i.e. required for code generation).
        """
        if self._iterator is None:
            self._iterator = IteratorType(self)

        # Only one usage of this iterator type is enough to consider it used
        if used:
            self._iterator._is_used = True

        return self._iterator

    @property
    def iterator(self) -> IteratorType:
        """
        Create an iterator type whose element type is `self`.
        """
        return self.create_iterator(used=True)

    @property
    def is_base_struct_type(self):
        return isinstance(self, BaseStructType)

    @property
    def is_struct_type(self):
        return isinstance(self, StructType)

    @property
    def is_ast_node(self):
        return isinstance(self, ASTNodeType)

    @property
    def is_array(self):
        return isinstance(self, ArrayType)

    @property
    def public_requires_boxing(self):
        """
        Whether the public type in the Ada API for this requires some boxing to
        be embedded in a record. This is true for all unbounded types (i.e.
        arrays).

        :rtype: bool
        """
        return False

    @property
    def exposed_types(self):
        """
        Return the list of types that ``self`` exposes when it is itself
        exposed.

        :rtype: list[CompiledType]
        """
        return []

    def new(self, *args, **kwargs):
        """
        Shortcut to the New expression, allowing type.new(..) syntax.

        :rtype: AbstractExpression
        """
        from langkit.expressions.structs import New
        return New(self, *args, **kwargs)

    def _init_fields(self, fields):
        """
        Bind input fields to `self` and initialize their name.

        :param list[(str|names.Name, AbstractNodeData)] fields: List of (name,
            field) for this struct's fields. Inheritted fields must not appear
            in this list.
        """
        for f_n, f_v in fields:
            if isinstance(f_n, str):
                name = names.Name.from_lower(f_n)
                str_name = f_n
            else:
                name = f_n
                str_name = f_n.lower

            # Remember the original name for public APIs
            f_v._original_name = str_name
            f_v._indexing_name = f_v.original_name

            # In nodes, use a qualified name for code generation to avoid name
            # conflicts between homonym properties. There is one exception:
            # built-in properties (those with no prefix) must not be decorated
            # for convenience in template code.
            f_v._name = (
                name
                if (not self.is_ast_node or
                    (f_v.is_property and f_v.prefix is None)) else
                self.kwless_raw_name + f_v._prefixed_name(name)
            )

            self.add_field(f_v)

    def add_field(self, field):
        """
        Append a field to this Struct/AST node.

        :param AbstractNodeData field: Field to append.
        """
        self._fields[field.indexing_name] = field
        field.struct = self

        # Invalidate the field lookup cache for this node and all derivations,
        # as this new field can be looked up by derivations.

        def reset_cache(t: CompiledType):
            t._abstract_node_data_dict_cache = {}
            for dt in t.derivations:
                reset_cache(dt)

        reset_cache(self)

    def get_user_fields(self, predicate=None, include_inherited=True):
        """
        Return the list of all the user fields `self` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[UserField]
        """
        return self.get_abstract_node_data(predicate, include_inherited,
                                           field_class=UserField)

    def get_fields(self, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `self` has, including its parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[BaseField]
        """
        return self.get_abstract_node_data(predicate, include_inherited,
                                           field_class=BaseField)

    def get_abstract_node_data(self, predicate=None, include_inherited=True,
                               field_class=AbstractNodeData):
        """
        Get all BaseField instances for the class.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(AbstractNodeData) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :param type field_class: The field class to use to filter fields.

        :rtype: list[AbstractNodeData]
        """
        return sorted(
            filter(
                predicate or (lambda f: True),
                self.get_abstract_node_data_dict(include_inherited,
                                                 field_class).values()
            ),
            key=lambda f: f._serial
        )

    def get_properties(self, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `self` has.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[langkit.expressions.base.PropertyDef]
        """
        return self.get_abstract_node_data(
            lambda f: f.is_property and (predicate is None or predicate(f)),
            include_inherited
        )

    def get_abstract_node_data_dict(self, include_inherited=True,
                                    field_class=AbstractNodeData):
        """
        Get all BaseField instances for the class.

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :param type field_class: The field class to use to filter fields.

        :rtype: dict[str, AbstractNodeData]
        """
        assert issubclass(field_class, AbstractNodeData)

        # First, see if we have a cached result for this
        key = (include_inherited, field_class)
        try:
            return self._abstract_node_data_dict_cache[key]
        except KeyError:
            pass

        # No cached result, we have to compute it
        if field_class == AbstractNodeData:
            # If we don't filter by class (i.e. if we want the most general
            # class field: AbstractNodeData), do the base class recursion.
            if include_inherited and self.is_ast_node:
                result = OrderedDict()
                for base_class in self.get_inheritance_chain():
                    result.update(base_class._fields)
            else:
                result = OrderedDict(self._fields)

        # Otherwise, just rely on the potentially already cached whole list of
        # fields and do filtering.
        else:
            all_fields = self.get_abstract_node_data_dict(include_inherited)
            result = OrderedDict(
                filter(lambda kv: isinstance(kv[1], field_class),
                       all_fields.items())
            )

        self._abstract_node_data_dict_cache[key] = result
        return result

    @property
    def has_early_decl(self) -> bool:
        """
        Return whether declarations for this type in the internal Ada API
        happen earlier than usual.

        Some types (for instance the metadata struct) have special constraints
        regarding code generation, so their type declaration must happen
        earlier than if they were more regular types.
        """
        return False


class NoCompiledType(CompiledType):
    """
    Special CompiledType singleton. Used to mean the absence of type. This is
    useful in special resolved expressions.
    """

    def is_refcounted(self):
        raise NotImplementedError()


no_compiled_type = NoCompiledType('NoCompiledType')


class LogicVarType(CompiledType):
    """
    Singleton for the logic variable type.

    This type represents a logical variable. A logical variable is a reference,
    that can take a specific value when it is bound to a specific Equation, and
    that equation is solved for a certain problem set.

    It is a special type in Langkit at this point, in that its value is
    initially undefined, and is mutated after the solve of an equation to a
    specific value. It is thus the only type in Langkit for the moment for
    which variables of the type can be mutated after creation.

    TODO: For the moment a logic variable is necessarily a holder around an
    ASTNodeType. At some point we might want to make it generic, like list and
    array types.
    """

    def __init__(self):
        super().__init__(
            name='LogicVar',
            nullexpr='null',
            is_ptr=False,
            has_special_storage=True,
            c_type_name='logic_var_type',
        )

    @property
    def storage_type_name(self):
        return names.Name('Logic_Var_Record')

    @property
    def storage_nullexpr(self):
        return 'Null_Var_Record'

    def extract_from_storage_expr(self, node_expr, base_expr):
        del node_expr
        return "{}'Unrestricted_Access".format(base_expr)

    def convert_to_storage_expr(self, node_expr, base_expr):
        raise not_implemented_error(self, type(self.convert_to_storage_expr))


class EnvRebindingsType(CompiledType):
    """
    Singleton for the environment rebinding type.
    """

    def __init__(self):
        # Even though env rebindings are "exposed" through the C API (entities
        # are structs, and they have an env rebindings field), they are not
        # meant to be part of any public API: the C API acts only as a common
        # bindings gateway, not a true public API. So keep env rebindings
        # "exposed=False" for the DSL.
        super().__init__(
            name='EnvRebindings',
            null_allowed=True,
            nullexpr='null',
            c_type_name='env_rebindings_type',
            is_refcounted=False,
            py_nullexpr='None',
            java_nullexpr='PointerWrapper.nullPointer()',
            hashable=True,
        )


class TokenType(CompiledType):
    """
    Singleton for the token data type.
    """

    def __init__(self):
        super().__init__(
            name='TokenReference',
            dsl_name='Token',
            exposed=True,
            is_ptr=False,
            null_allowed=True,
            nullexpr='No_Token',
            is_ada_record=True,
            has_special_storage=True,
            c_type_name='token'
        )

    @property
    def storage_type_name(self):
        return 'Token_Index'

    @property
    def storage_nullexpr(self):
        return 'No_Token_Index'

    def extract_from_storage_expr(self, node_expr, base_expr):
        return 'Token ({}, {})'.format(node_expr, base_expr)

    def convert_to_storage_expr(self, node_expr, base_expr):
        return 'Stored_Token ({}, {})'.format(node_expr, base_expr)

    @property
    def mypy_type_hint(self) -> str:
        return "Opt[Token]"


class Argument:
    """
    Holder for properties arguments.
    """

    def __init__(self, name, type, is_artificial=False, default_value=None,
                 abstract_var=None, source_name=None):
        """
        :param names.Name name: Argument name.
        :param CompiledType type: Argument type.
        :param bool is_artificial: Whether the argument was automatically
            created by Langkit, i.e. the language specification did not mention
            it.
        :param AbstractExpression|None default_value: If None, there is no
            default value associated to this argument. Otherwise, it must be a
            compile-time known abstract expression to be used when generating
            code for the corresponding property argument.
        :param AbstractVariable|None abstract_var: For properties only. If
            provided, use it as the abstract variable to reference this
            argument. If not provided, an AbstractVariable instance is
            automatically created.
        """
        from langkit.expressions.base import (AbstractVariable,
                                              construct_compile_time_known)

        self.name = name
        self.var = (abstract_var
                    or AbstractVariable(name, type, source_name=source_name))
        self.is_artificial = is_artificial

        # Make sure that, if present, the default value is a compile-time known
        # constant.
        self.abstract_default_value = default_value
        self.default_value = (
            None
            if default_value is None else
            construct_compile_time_known(default_value, type)
        )

    @property
    def type(self):
        return self.var.type

    @property
    def public_type(self):
        return self.type.public_type

    @property
    def public_default_value(self):
        """
        Assuming this argument has a default value, return the default value to
        use in public APIs, according to the type exposed in public.

        :rtype: ResolvedExpression
        """
        from langkit.expressions import NullExpr

        assert self.default_value is not None

        if not self.type.is_ast_node:
            return self.default_value

        if isinstance(self.default_value, NullExpr):
            return NullExpr(self.public_type)
        else:
            assert False, 'Unsupported default value'

    @property
    def dsl_name(self):
        return self.name.lower

    def __repr__(self):
        return '<Argument {} : {}>'.format(self.dsl_name, self.type.dsl_name)


class BaseField(AbstractNodeData):
    """
    Base class for node fields and structure fields.
    """

    concrete: ClassVar[bool] = False
    """
    Field used to prevent instantiation of the class. Concrete descendants
    of BaseField must put that field to True in their definition.
    """

    def __init__(self,
                 repr: bool = True,
                 doc: str = '',
                 type: Opt[CompiledType] = None,
                 access_needs_incref: bool = False,
                 internal_name: Opt[names.Name] = None,
                 prefix: Opt[names.Name] = AbstractNodeData.PREFIX_FIELD,
                 null: bool = False):
        """
        Create an AST node field.

        :param repr: If true, the field will be displayed when
            pretty-printing the embedding AST node.
        :param doc: User documentation for this field.
        :param access_needs_incref: See AbstractNodeData's constructor.
        :param internal_name: See AbstractNodeData's constructor.
        :param null: Whether this field is always supposed to be null.
        """

        assert self.concrete, 'BaseField itself cannot be instantiated'

        super().__init__(
            public=True,
            access_needs_incref=access_needs_incref,
            internal_name=internal_name,
            prefix=prefix,
        )

        self.repr = repr
        self._name = None
        self._doc = doc

        self.should_emit = True
        """
        Subclasses can change that variable to trigger wether the field
        should be emitted or not.
        """

        self._type = type
        """
        Type of the field. If not set, it will be set to a concrete
        CompiledType subclass after type resolution. If set, it will be
        verified at type resolution time.
        """

        self._null = null

    @property
    def has_type(self) -> bool:
        """
        Return whether the type for this field is known.

        If this returns ``False``, evaluating the ``type`` field is illegal.
        """
        return self._type is not None

    @property
    def type(self) -> CompiledType:
        self._type = resolve_type(self._type)
        assert isinstance(self._type, CompiledType)
        return self._type

    # TODO: RA22-015: Remove this, and make AbstractNodeData.type read-only,
    # when transition to the DSL is done.
    @type.setter
    def type(self, typ: CompiledType):
        self._type = typ

    def __repr__(self) -> str:
        return '<ASTNode {} Field({})>'.format(self._serial, self.qualname)

    @property
    def doc(self) -> str:
        return self._doc

    @property
    def null(self) -> bool:
        """
        Return whether this field is always supposed to be null.
        """
        return self._null


class Field(BaseField):
    """
    Fields that are meant to store parsing results. Can be used only on
    subclasses of ASTNodeType.
    """
    concrete = True

    def __init__(self,
                 repr: bool = True,
                 doc: str = "",
                 type: Opt[CompiledType] = None,
                 abstract: bool = False,
                 null: bool = False):
        super().__init__(repr, doc, type, null=null)

        assert not abstract or not null
        self._abstract = abstract
        self._null = null

        self._overriding_computed = False
        self._overriding: Opt[Field] = None
        self._concrete_fields: List[Field] = []

        self.parsers_from_transform: List[Parser] = []
        """
        List of parsers that provide a value for this field. Such parsers are
        children of Transform parsers.
        """

        self.types_from_synthesis = TypeSet()
        """
        Set of types coming from node synthetization in properties.
        """

        self._precise_types: Opt[TypeSet] = None
        """
        Cache for the precise_types property.
        """

        self._precise_element_types: Opt[TypeSet] = None
        """
        Cache for the precise_element_types property.
        """

        self._is_optional: Opt[bool] = None
        """
        See the ``is_optional`` property. This field is computed in a dedicated
        compilation pass.
        """

        self._index: Opt[int] = None
        """
        0-based index for this parsing field in the owning AST node's children
        list. This is -1 for abstract or null fields.
        """

    @property
    def precise_types(self) -> TypeSet:
        """
        Return the precise set of types that this field can contain.

        This is the same as ``TypeSet([self.type])`` except for nodes created
        during parsing: for these, ``self.type`` might be too general.
        """
        assert self._precise_types is not None
        return self._precise_types

    @property
    def precise_element_types(self) -> TypeSet:
        """
        For fields that contain lists, return the precise set of types that
        these list can contain.
        """
        assert self.type.is_list_type
        assert self._precise_element_types is not None
        return self._precise_element_types

    @property
    def is_optional(self) -> bool:
        """
        Return whether it is possible for this field, when there is no parsing
        error, to contain a null node. Note that this is always ``False`` for
        fields that contain list nodes, as parsers always create an empty list
        instead of null list nodes.
        """
        assert self._is_optional is not None
        return self._is_optional

    def _compute_precise_types(self) -> None:
        from langkit.parsers import Null

        etypes = None
        is_list = self.type.is_list_type

        assert isinstance(self.struct, ASTNodeType)

        if self.null:
            # Null fields have their type automatically computed from the
            # abstract field they override.
            types = TypeSet([self.type])
            if is_list:
                etypes = TypeSet([self.type.element_type])

        elif self.abstract:
            # Abstract fields can contain anything the corresponding concrete
            # one accept, thanks to the laws of inheritance.
            types = TypeSet()
            if is_list:
                etypes = TypeSet()
            for f in self.concrete_fields:
                f._compute_precise_types()
                types.update(f.precise_types)
                if is_list:
                    assert etypes
                    etypes.update(f.precise_element_types)

        elif self.struct.synthetic:
            types = self.types_from_synthesis
            assert types.matched_types
            if is_list:
                etypes = TypeSet([self.type.element_type])

        else:
            # For fields in regular nodes
            types = TypeSet()
            if is_list:
                etypes = TypeSet()
            all_null = True
            for p in self.parsers_from_transform:
                if not isinstance(p, Null):
                    all_null = False
                types.update(p.precise_types)
                if is_list:
                    assert etypes
                    etypes.update(p.precise_element_types)

            # Only null fields are allowed to get only null parsers. Things are
            # different for fields that are lists, as we do create list nodes
            # for them.
            if all_null:
                if is_list:
                    # If this field contains list nodes and only null parsers
                    # build it, just use types from the field declaration.
                    types.include(self.type)
                    etypes.include(self.type.element_type)
                else:
                    with self.diagnostic_context:
                        check_source_language(
                            not all_null,
                            "According to the grammar, this field always"
                            " contain null nodes: please tag it a null field"
                        )

            # Due to inheritance, even fields of regular nodes can appear in a
            # synthetic node, so take types from node synthesis into account.
            types.update(self.types_from_synthesis)
            if is_list:
                assert etypes
                for t in self.types_from_synthesis.matched_types:
                    if t.is_list_type:
                        etypes.include(t.element_type)

        self._precise_types = types
        self._precise_element_types = etypes

    @property
    def doc(self) -> str:
        # If parsers build this field, add a precise list of types it can
        # contain: the field type might be too generic.

        result = super().doc

        # Synthetic nodes are not built by parsers, so for now we don't have
        # precise type information for them.
        assert isinstance(self.struct, ASTNodeType)
        if self.struct.synthetic:
            return result

        precise_types = self.precise_types.minimal_matched_types

        # If the field always contains a list, and that we know it holds one or
        # several types more precise that its element type, give precise types
        # for the list items.
        if len(precise_types) == 1 and self.type.is_list_type:
            precise_element_types = list(self.precise_element_types
                                         .minimal_matched_types)
            if (
                len(precise_element_types) > 1
                or precise_element_types[0] != self.type.element_type
            ):

                return append_paragraph(
                    result,
                    'This field contains a list that itself contains one of '
                    'the following nodes: '
                    f'{precise_types_doc(precise_element_types)}'
                )

        if len(precise_types) > 1:
            return append_paragraph(
                result,
                'This field can contain one of the following nodes: '
                f'{precise_types_doc(precise_types)}'
            )

        return result

    @property
    def overriding(self) -> Opt[Field]:
        """
        If this field overrides an abstract field, return the abstract field.
        return None otherwise.
        """
        assert self._overriding_computed, (
            '"overriding" not computed for {}'.format(self.qualname))
        return self._overriding

    @overriding.setter
    def overriding(self, overriding: Field) -> None:
        assert not self._overriding_computed
        self._overriding_computed = True
        self._overriding = overriding
        if overriding:
            overriding._concrete_fields.append(self)

    @property
    def concrete_fields(self) -> List[Field]:
        """
        Assuming this field is abstract, return the list of concrete fields
        that override it.
        """
        assert self.abstract and self._overriding_computed
        return self._concrete_fields

    @property
    def index(self) -> int:
        """
        Return the 0-based index of this parsing field in the owning AST node's
        children list. Only non-null concrete fields have an index.
        """
        assert self._index is not None, (
            'Index for {} is not computed'.format(self.qualname))
        assert self._index != -1, (
            'Trying to get index of abstract/null field {}'
            .format(self.qualname))
        return self._index


class UserField(BaseField):
    """
    Fields that are not meant to store parsing results. Can be used on any
    Node type, will be ignored by the parsing code.
    """

    is_user_field = True
    concrete = True

    def __init__(self,
                 type: CompiledType,
                 repr: bool = False,
                 doc: str = '',
                 public: bool = True,
                 default_value: Opt[AbstractExpression] = None,
                 access_needs_incref: bool = True,
                 internal_name: Opt[names.Name] = None,
                 prefix: Opt[names.Name] = None):
        """
        See inherited doc. In this version we just ensure that a type is
        passed because it is mandatory for data fields. We also set repr to
        False because most of the time you don't want User fields to show up in
        the pretty printer.

        :param is_public: Whether this field is public in the generated APIs.
        :param access_needs_incref: See AbstractNodeData's constructor.
        :param internal_name: See AbstractNodeData's constructor.
        :param default_value: Default value for this field, when omitted from
            New expressions.
        """
        super().__init__(
            repr,
            doc,
            type,
            access_needs_incref=access_needs_incref,
            internal_name=internal_name,
            prefix=prefix,
        )
        self._is_public = public

        # We cannot construct the default value yet, as not all types are
        # known. Do this in CompileCtx.compute_types instead.
        self.abstract_default_value = default_value


class MetadataField(UserField):
    """
    Field for metadata struct. Can be used only there, and requires the user to
    specify whether the field shall be taken into account when processing
    identity related properties.
    """

    def __init__(
        self,
        type: CompiledType,
        use_in_equality: bool,
        repr: bool = False,
        doc: str = '',
        public: bool = True,
        default_value: Opt[AbstractExpression] = None,
        access_needs_incref: bool = True,
        internal_name: Opt[names.Name] = None,
        prefix: Opt[names.Name] = None
    ):
        self.use_in_equality = use_in_equality
        super().__init__(
            type, repr, doc, public, default_value, access_needs_incref,
            internal_name, prefix
        )


class BuiltinField(UserField):
    """
    A built-in field is just like a UserField, except that its name has no
    prefix. It is typically used for fields of built-in structs.
    """

    def __init__(self,
                 type: CompiledType,
                 repr: bool = False,
                 doc: str = '',
                 public: bool = True,
                 default_value: Opt[AbstractExpression] = None,
                 access_needs_incref: bool = True,
                 internal_name: Opt[names.Name] = None,
                 prefix: Opt[names.Name] = None):
        super().__init__(
            type=type,
            repr=repr,
            doc=doc,
            public=public,
            default_value=default_value,
            access_needs_incref=access_needs_incref,
            internal_name=internal_name,
            prefix=prefix,
        )
        self.should_emit = False


class BaseStructType(CompiledType):
    """
    Base class to share common behavior between StructType and ASTNodeType.
    """

    def __init__(self, name, location, doc, is_builtin_type=False, **kwargs):
        """
        :param is_builtin_type: Whether this type is built-in. If it is,
            dsl_unparse.py should skip it.

        See CompiledType.__init__ for a description of other arguments.
        """
        self.is_builtin_type = is_builtin_type

        kwargs.setdefault('type_repo_name', name.camel)
        if is_keyword(name):
            name = name + names.Name('Node')

        super().__init__(name, location, doc, **kwargs)

    @property
    def py_nullexpr(self):
        return self._py_nullexpr or '{}({})'.format(self.name.camel, ', '.join(
            f.type.py_nullexpr for f in self.get_fields()
        ))

    @property
    def required_fields_in_exprs(self):
        """
        Return all fields that must be involved in the property DSL.

        This returns a mapping from DSL field names to fields for all fields
        that must be considered when building structs in the property DSL.

        :rtype: dict[str, Field]
        """
        def is_required(f):
            if f._original_name is None:
                # If this field does not have an original name, it does not
                # come from sources (it's automatic/internal), and thus users
                # are not supposed to access/set it.
                return False

            elif isinstance(f, BuiltinField):
                # BuiltinFields are actually stored fields only for structure
                # types (not for nodes).
                return self.is_struct_type

            elif isinstance(f, Field):
                return not f.null

            else:
                return isinstance(f, UserField)

        return {f.original_name: f
                for f in self.get_abstract_node_data()
                if is_required(f)}

    def add_internal_user_field(
        self,
        name: names.Name,
        type: CompiledType,
        default_value: Opt[AbstractExpression],
        doc: str = "",
    ) -> UserField:
        """
        Create an internal (not public) UserField for this struct type.

        This method takes care of creating the UserField instance, registering
        it as a field of ``self`` and then returns the instance.

        See UserField's constructor for argument semantics.

        Note that ``default_value`` is a mandatory argument that can be None as
        we need to allow None values because None is the exception rather than
        the rule: users cannot assign a value to these fields, and thus we need
        most of the time to assign a default value to them.
        """
        result = UserField(type=type,
                           doc=doc,
                           public=False,
                           default_value=default_value)
        result._name = name
        result._indexing_name = '[internal]{}'.format(name.lower)
        self.add_field(result)
        return result


class StructType(BaseStructType):
    """
    POD composite type.
    """

    def __init__(self, name, location, doc, fields, **kwargs):
        """
        :param name: See CompiledType.__init__.

        :param list[(str|names.Name, AbstractNodeData)] fields: List of (name,
            field) for this struct's fields. Inherited fields must not appear
            in this list.
        """
        internal_name = names.Name('Internal') + name
        super().__init__(
            internal_name, location, doc,
            is_ptr=False,
            null_allowed=True,
            nullexpr=(names.Name('No') + name).camel_with_underscores,
            is_ada_record=True,
            exposed=False,
            hashable=True,
            api_name=name,
            type_repo_name=name.camel,
            dsl_name=name.camel,

            **kwargs
        )
        self._init_fields(fields)
        CompiledTypeRepo.struct_types.append(self)

    @property
    def conversion_requires_context(self):
        return any(f.type.conversion_requires_context
                   for f in self.get_fields())

    def add_as_memoization_key(self, context):
        super().add_as_memoization_key(context)
        for f in self.get_fields():
            f.type.add_as_memoization_key(context)

    @property
    def has_equivalent_function(self):
        return any(f.type.has_equivalent_function for f in self.get_fields())

    def require_hash_function(self):
        super().require_hash_function()
        for f in self.get_fields():
            f.type.require_hash_function()

    @property
    def is_refcounted(self):
        return any(f.type.is_refcounted for f in self._fields.values())

    @property
    def is_empty(self):
        return len(self.get_fields()) == 0

    @property
    def has_early_decl(self) -> bool:
        # The env metadata and inner env assoc structs are emitted separately
        # as they need to be fully declared before the
        # Langkit_Support.Lexical_Env generic package instantiation, while
        # regular structs are declared after that instantiation.
        return self in (T.env_md, T.inner_env_assoc)

    @property
    def is_predeclared(self) -> bool:
        """
        Whether this struct type is to be considered as pre-declared in
        templates for the internal Ada API.

        Note that except for the type declaration itself ("type ... is") and
        the "nullexpr" constant, declarations for this type must still be
        emitted (Hash function, ...).
        """
        assert CompiledTypeRepo.root_grammar_class
        return self in (
            # It's the Langkit_Support.Lexical_Env generic package
            # instantiation that declares entity, entity info and inner env
            # assoc structs.
            CompiledTypeRepo.root_grammar_class.entity_info(),
            CompiledTypeRepo.root_grammar_class.entity,
        )

    def c_inc_ref(self, capi):
        """
        Name of the C API function to inc-ref structure value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.name + names.Name('Inc_Ref'))

    def c_dec_ref(self, capi):
        """
        Name of the C API function to dec-ref structure value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.name + names.Name('Dec_Ref'))

    @property
    def emit_c_type(self):
        """
        Return whether to emit a C type for this type.

        This is used to filter out all entity types except the root one. All
        entity types are compatible from an ABI point of view, so this reduces
        the amount of code emitted for them.

        :rtype: bool
        """
        return not self.is_entity_type or self == T.entity

    @property
    def public_record_type(self):
        """
        Name of the Ada record type used to expose this struct in the public
        API.

        :rtype: names.Name
        """
        return self.name + names.Name('Record')

    @property
    def contains_boxed_fields(self):
        """
        Return if at least one field requires boxing in the public API.

        :rtype: bool
        """
        return any(f.type.public_requires_boxing for f in self.get_fields())

    @property
    def to_public_converter(self):
        return names.Name('To_Public') + self.api_name

    @property
    def to_internal_converter(self):
        return names.Name('To_Internal') + self.api_name

    @property
    def exposed_types(self):
        # Entity types are exposed as opaque types, so don't expose their
        # internals.
        return ([] if self.is_entity_type else
                [f.type for f in self.get_fields()])


class EntityType(StructType):
    """
    Subclass of StructType dedicated to entity types.
    """

    def __init__(self, astnode):
        self.astnode = astnode

        name = names.Name('Entity')
        if not self.astnode.is_root_node:
            name += self.astnode.kwless_raw_name

        super().__init__(
            name, None, None,
            [('node', BuiltinField(self.astnode, doc='The stored AST node')),
             ('info', BuiltinField(self.astnode.entity_info(),
                                   access_needs_incref=True,
                                   doc='Entity info for this node'))],
        )
        self.is_entity_type = True
        self._element_type = astnode

        if self.astnode.is_root_node:
            # The root entity is always exposed in public APIs. Some things are
            # automatically emitted for all derived types (without checking
            # _exposed), but we also rely on this flag to be set only for
            # entity types that are used in public properties.
            self.exposed = True

    @property
    def dsl_name(self):
        return '{}.entity'.format(self.element_type.dsl_name)

    def c_type(self, capi):
        # Emit only one C binding type for entities. They are all ABI
        # compatible, so this reduces the amount of types emitted.
        return CAPIType(capi, 'base_entity')

    @property
    def is_root_type(self):
        """
        Return whether this entity type correspond to the AST node root type.

        :rtype: bool
        """
        return self.astnode.is_root_node

    @property
    def base(self):
        """
        Return the entity type that `self` overrides, or None for the root.

        :rtype: EntityType
        """
        return None if self.is_root_type else self.astnode.base.entity

    @property
    def api_name(self):
        """
        Type name to use in the public API.

        :rtype: names.Name
        """
        return self.astnode.kwless_raw_name

    @property
    def constructor_name(self):
        """
        Name of the internal Ada functions to instantiate this array.

        :rtype: names.Name
        """
        return names.Name('Create') + self.name

    def to_public_expr(self, internal_expr):
        # Wrap the bare node into a public entity
        result = 'Wrap_Node ({}.Node, {}.Info)'.format(internal_expr,
                                                       internal_expr)

        # If needed, convert the result expression to the appropriate public
        # entity derivation.
        if not self.element_type.is_root_node:
            result += '.As_{}'.format(self.api_name)

        return result

    def to_internal_expr(self, public_expr, context=None):
        return ('({internal_node}, {public_entity}.Internal.Info)'.format(
            internal_node=self.element_type.to_internal_expr(public_expr),
            public_entity=public_expr
        ))


class ASTNodeType(BaseStructType):
    """
    Type for an AST node.
    """

    kwless_raw_name: names.Name

    # If this is an enum onde, list of descriptions for its enum alternatives
    _alternatives: List[ASTNodeType]

    # If this is an enum node, mapping from alternative camel name to
    # ASTNodeType instance for the alternative.
    _alternatives_map: Dict[str, ASTNodeType]

    def __init__(
        self,
        name: names.Name,
        location: Opt[Location],
        doc: Opt[str],
        base: Opt[ASTNodeType],
        fields: Sequence[Tuple[Union[str, names.Name], AbstractNodeData]],
        env_spec: Opt[EnvSpec] = None,
        element_type: Opt[ASTNodeType] = None,
        annotations: Opt[Annotations] = None,
        is_generic_list_type: bool = False,
        is_abstract: bool = False,
        is_synthetic: bool = False,
        has_abstract_list: bool = False,
        is_enum_node: bool = False,
        is_bool_node: bool = False,
        is_token_node: bool = False,
        is_error_node: bool = False,
        dsl_name: Opt[str] = None
    ):
        """
        :param name: Name for this node.

        :param location: Location for the declaration of this node, if any.

        :param doc: User documentation for this node.

        :param base: ASTNodeType subclass corresponding to the base class for
            this node. None when creating the root node.

        :param fields: List of (name, field) for this node's fields. Inherited
            fields must not appear in this list.

        :param env_spec: Environment specification for this node, if any.

        :param element_type: For root list types, this must be the ASTNodeType
            subclass that this list contains. Must be left to None in all other
            cases.

        :param is_generic_list_type: Whether this subclass will materialize the
            generic list type.

        :param is_abstract: Whether this node is abstract. Note that this can
            be changed later. This is forced to True for the generic list type
            and for root list types whose element type has the
            `has_abstract_list` attribute set to True.

        :param is_synthetic: Whether this node is synthetic. Note that this can
            be changed later.

        :param has_abstract_list: Whether the root list type for this node must
            be abstract. Node that this can be changed later, until the list
            type is actually created.

        :param is_enum_node: Whether this node comes from the expansion of an
            enum node.

        :param is_bool_node: Whether this node is a qualifier coming from the
            expansion of an enum node.

        :param is_token_node: Whether this node only materializes a parsed
            token. If so, grammars that produce such nodes must parse only one
            token (.token_start must be equal to .token_end).

        :param is_error_node: Whether this node only materializes a parsing
            error. If so, only Skip parsers can create this node.

        :param dsl_name: Name used to represent this type at the DSL level.
            Useful to format diagnostics.
        """
        from langkit.expressions import Property

        actual_fields = list(fields)

        self.raw_name = name
        self.kwless_raw_name = (self.raw_name + names.Name('Node')
                                if is_keyword(self.raw_name) else
                                self.raw_name)

        name = names.Name('Bare') + self.kwless_raw_name

        is_root = base is None
        is_root_list = base is not None and base.is_generic_list_type
        is_list = base is not None and (is_root_list or base.is_list_type)

        if base is None:
            self.null_constant = names.Name('No') + name
        else:
            assert CompiledTypeRepo.root_grammar_class
            self.null_constant = (
                CompiledTypeRepo.root_grammar_class.null_constant
            )

        if is_root_list:
            assert element_type is not None and element_type.is_ast_node

            # TODO: at this point, we need to make sure thas
            # element_type.has_abstract_list in the future.
            is_abstract = is_abstract or element_type.has_abstract_list

        else:
            assert element_type is None
            if is_list:
                assert base is not None
                assert isinstance(base._element_type, ASTNodeType)
                element_type = base._element_type

        if is_root_list:
            assert element_type
            doc = doc or 'List of {}.'.format(element_type.dsl_name)

        super().__init__(
            name, location, doc,
            base=base, is_ptr=True, null_allowed=True, is_ada_record=False,
            is_list_type=is_list,

            # Even though bare node types are not exposed, we allow them in
            # public APIs and will (un)wrap them as entities automatically.
            exposed=True,

            is_refcounted=False,
            nullexpr=self.null_constant.camel_with_underscores,
            py_nullexpr='None',
            java_nullexpr='null',
            element_type=element_type,
            hashable=True,
            type_repo_name=self.raw_name.camel,

            dsl_name=dsl_name or self.raw_name.camel,
        )
        self._is_root_node = is_root
        self.is_generic_list_type: bool = is_generic_list_type
        self.is_root_list_type = is_root_list
        self.is_list = is_list

        # Register this new subclass where appropriate in CompiledTypeRepo
        if is_root:
            CompiledTypeRepo.root_grammar_class = self

            self.value_type_name = 'Root_Node_Record'
            """
            Name of the Ada type for the record that contains data for all
            nodes.
            """

        CompiledTypeRepo.astnode_types.append(self)

        # Now we have an official root node type, we can create its builtin
        # fields.
        if is_root:
            # If the language spec does not define one, create a default
            # "can_reach" property.
            if not any(
                (n if isinstance(n, str) else n.lower) == "can_reach"
                for n, _ in actual_fields
            ):
                from langkit.expressions import No, Self
                can_reach = Property(
                    public=False,
                    type=T.Bool,
                    expr=(
                        lambda from_node=T.root_node:
                        # If there is no from_node node, assume we can access
                        # everything. Also assume than from_node can reach Self
                        # if both do not belong to the same unit.
                        (from_node == No(T.root_node))
                        | (Self.unit != from_node.unit)  # type: ignore
                        | (Self < from_node)
                    ),
                )
                # Provide a dummy location for GDB helpers
                can_reach.location = Location(__file__)
                can_reach.artificial = True
                actual_fields.append(("can_reach", can_reach))

            # Always add builtin properties first
            actual_fields = self.builtin_properties() + actual_fields

        self._init_fields(actual_fields)

        # Encode all field names for nodes so that there is no name collision
        # when considering all fields from all nodes.
        for f in actual_fields:
            if isinstance(f, BaseField):
                f._internal_name = self.name + f.name

        # Make sure that all user fields for nodes are private
        for _, f_v in actual_fields:
            with f_v.diagnostic_context:
                check_source_language(
                    not f_v.is_user_field or
                    isinstance(f_v, BuiltinField) or
                    f_v.is_private,
                    'UserField on nodes must be private'
                )

        # Associate concrete syntax fields to the corresponding abstract ones,
        # if any. Don't bother doing validity checking here: the valide_field
        # pass will take care of it.
        inherited_fields = (self.base.get_abstract_node_data_dict()
                            if self.base else {})
        for f_n, f_v in self._fields.items():
            base_field = inherited_fields.get(f_n)
            if isinstance(f_v, Field):
                if (
                    base_field and
                    isinstance(base_field, Field) and
                    base_field.abstract
                ):
                    f_v.overriding = base_field
                    # Null fields are not initialized with a type, so they must
                    # inherit their type from the abstract field they override.
                    if f_v.null:
                        f_v._type = base_field._type
                else:
                    f_v.overriding = None

        from langkit.dsl import Annotations
        annotations = annotations or Annotations()
        self.annotations: Annotations = annotations
        self.annotations.process_annotations(self, is_root)

        if env_spec:
            env_spec.ast_node = self

        self.env_spec: Opt[EnvSpec] = env_spec
        """
        EnvSpec instance corresponding to this node.
        """

        # List types are resolved by construction: we create list types to
        # contain specific ASTNodeType subclasses. All other types are not
        # resolved, only the grammar will resolve them..
        self.is_type_resolved = is_list

        # By default, ASTNodeType subtypes aren't abstract. The "abstract"
        # decorator may change this attribute later. Likewise for synthetic
        # nodes and nodes whose root list type is abstract.
        self.abstract: bool = is_abstract or is_root or is_generic_list_type
        self.synthetic = is_synthetic

        self.has_abstract_list: bool = has_abstract_list
        """
        Whether the automatically generated list type for this ASTNodeType (the
        "root list type") is abstract.
        """

        # Prepare the list of subclasses for this node type and, if applicable,
        # register it as a subclass of its base.
        self.subclasses: List[ASTNodeType] = []
        """
        List of subclasses. Overriden in the root grammar class and its
        children. This list is completed as ASTNodeType instances are created,
        and sorted in the "compute_types" compilation pass.
        """

        if base is not None:
            base.subclasses.append(self)

        # If this is the root grammar type, create the generic list type name
        self.generic_list_type: Opt[ASTNodeType] = None
        """
        Root grammar class subclass. It is abstract, generated automatically
        when the root grammar class is known. All root list types subclass it.
        """

        if base is None:
            generic_list_type_name = (
                names.Name.from_camel(annotations.generic_list_type)
                if annotations.generic_list_type else
                (self.kwless_raw_name + names.Name('Base_List'))
            )

            self.generic_list_type = ASTNodeType(
                name=generic_list_type_name, location=None, doc='',
                base=self, fields=[], is_generic_list_type=True,
                is_abstract=True
            )

        self.transform_parsers: List[_Transform] = []
        """
        List of Transform parsers that produce this node.
        """

        self.list_element_parsers: Opt[List[Parser]] = [] if is_list else None
        """
        For list nodes, list of parsers that produce list elements. None for
        all other nodes.

        :type: list[langkit.parsers.Parser]
        """

        self.precise_list_element_types: Opt[TypeSet] = None
        """
        For list nodes, precise set of types that this list can contain.
        """

        self.unparser: Opt[NodeUnparser] = None
        """
        Unparser for this node. Computed during the NodesToParsers pass.
        """

        self.is_enum_node = is_enum_node
        self.is_bool_node = is_bool_node
        self.is_token_node = is_token_node
        self.is_error_node = is_error_node

        self.token_kind: Opt[TokenAction] = None
        """
        If this is a token node and if unparser generation is enabled, this
        must reference the only token kind that this node can be associated to.
        Must be None otherwise.

        :type: langkit.lexer.TokenAction|None
        """

        # Make sure we have one entity type for each AST node type
        entity_type = self.entity
        del entity_type

    @property
    def effective_env_spec(self):
        """
        Return the env spec, for this node, whether it's defined on this node
        or inherited from a parent node.
        """
        return self.env_spec if self.env_spec else (
            self.base.effective_env_spec if self.base else None
        )

    @property
    def doc(self):
        result = super().doc

        # If this is a list node and that parsers build it, add a precise list
        # of types it can contain: the element type might be too generic.
        if self.is_list and not self.synthetic:
            precise_types = list(self.precise_list_element_types
                                 .minimal_matched_types)
            if len(precise_types) > 1 or precise_types[0] != self.element_type:
                addition = indent(
                    'This list node can contain one of the following'
                    f' nodes: {precise_types_doc(precise_types)}',
                    first_line_indentation(result)
                )
                return append_paragraph(result, addition)

        return result

    def repr_name(self):
        """
        Return a name that will be used when serializing this AST node.
        :rtype: str
        """
        # This name is used by pretty printers-like code: we need the
        # "original" node name here, not keyword-escaped ones.
        result = self.annotations.repr_name or self.kwless_raw_name.camel
        return result

    @property
    def public_type(self):
        return self.entity

    def add_transform(self, parser):
        """
        Register ``parser`` as a Transform parser that creates this node.

        This also registers sub-parsers in node fields and keep track of field
        types, checking for consistencies.

        :param langkit.parsers._Transform parser: Transform parser to register.
        """
        self.transform_parsers.append(parser)

    def add_list_element_parser(self, parser):
        """
        Register ``parser`` as a parser that creates list elements.

        :param langkit.parsers.Parser parser: Parser to register.
        """
        self.list_element_parsers.append(parser)

    def check_inferred_field_types(self):
        """
        Check that inferred field types from the grammar are consistent with
        annotations. If there was no type annotation, use the inferred type to
        assign a type to the parse field.
        """
        for parser in self.transform_parsers:
            # Get parse fields except null ones, as parsers don't contribute to
            # typing these.
            #
            # Parsers cannot build abstract types, and only abstract types can
            # have abstract nodes, so in theory we should not have abstract
            # nodes here.  But at this point this DSL check has not happened
            # yet...
            fields = self.get_parse_fields(
                predicate=lambda f: not f.abstract and not f.null
            )

            parsers = parser.fields_parsers
            types = [p.type for p in parsers]

            # Propagate sub-parsers to fields to let them compute precise types
            for f, p in zip(fields, parsers):
                f.parsers_from_transform.append(p)

            # Typing in the Transform parser is already supposed to check
            # consistency in the DSL.
            assert len(fields) == len(types)

            # TODO: instead of expecting types to be subtypes, we might want to
            # perform type unification (take the nearest common ancestor for
            # all field types). But then again, maybe not, it might be too
            # confusing.
            for field, f_type in zip(fields, types):
                if field.has_type:
                    check_source_language(
                        f_type.matches(field.type),
                        "Field {} already had type {}, got {}".format(
                            field.qualname, field.type.dsl_name,
                            f_type.dsl_name
                        )
                    )

            # Only assign types if self was not yet typed. In the case where it
            # was already typed, we checked above that the new types were
            # consistent with the already present ones.
            if not self.is_type_resolved:
                self.is_type_resolved = True

                for inferred_type, field in zip(types, fields):

                    # At this stage, if the field has a type, it means that the
                    # user assigned it one originally. In this case we will use
                    # the inferred type for checking only (raising an assertion
                    # if it does not correspond).
                    if field.has_type:
                        with field.diagnostic_context:
                            check_source_language(
                                # Using matches here allows the user to
                                # annotate a field with a more general type
                                # than the one inferred.
                                inferred_type.matches(field.type),
                                'Expected type {} but type inferenced yielded'
                                ' type {}'.format(
                                    field.type.dsl_name,
                                    inferred_type.dsl_name
                                )
                            )
                    else:
                        field.type = inferred_type

    def compute_precise_fields_types(self):
        if self.is_list:
            # Do not compute precise types twice
            if self.precise_list_element_types:
                return

            # A list node can contain nodes coming from its own parsers, but
            # also from subclasses' parsers.
            self.precise_list_element_types = types = TypeSet()
            for subcls in self.subclasses:
                subcls.compute_precise_fields_types()
                types.update(subcls.precise_list_element_types)
            for p in self.list_element_parsers:
                types.update(p.precise_types)

        else:
            for f in self.get_parse_fields(include_inherited=False):
                f._compute_precise_types()

    def warn_imprecise_field_type_annotations(self):
        for field in self.get_parse_fields():

            # We want to compare the type annotation to the type that was
            # inferred from the grammar. There is more to do than just
            # comparing the two types since there can be several types that
            # specify the same number of concrete types: think of an abstract
            # type that is subclassed only once. So use type sets to do the
            # comparison, instead.
            common_inferred = field.precise_types.minimal_common_type
            inferred_types = TypeSet([common_inferred])
            field_types = TypeSet([field.type])

            with field.diagnostic_context:
                WarningSet.imprecise_field_type_annotations.warn_if(
                    inferred_types != field_types,
                    'Specified type is {}, but it could be more specific:'
                    ' {}'.format(field.type.dsl_name,
                                 common_inferred.dsl_name)
                )

    @staticmethod
    def common_ancestor(*nodes):
        """
        Return the bottom-most common parent AST node for all `nodes` AST
        nodes.

        :param list[ASTNodeType] nodes: List of AST node types for which we
            are looking for the common ancestor.
        :rtype: ASTNodeType
        """
        return list(takewhile(
            lambda a: len(set(a)) == 1,
            zip(*map(ASTNodeType.get_inheritance_chain, nodes))
        ))[-1][0]

    @property
    def base(self) -> Opt[ASTNodeType]:
        result = super().base
        assert result is None or isinstance(result, ASTNodeType)
        return result

    @property  # type: ignore
    @memoized
    def concrete_subclasses(self) -> list[ASTNodeType]:
        """
        Return the list of all (direct or indirect) subclass types for `self`
        that are not abstract, sorted by hierarchical name. If `self` is not
        abstract, it is included.
        """
        return [t for t in self.type_set if not t.abstract]

    @property
    def type_set(self) -> List[ASTNodeType]:
        """
        Return the list of all (direct or indirect) subclasses for ``self``,
        including ``self`` as well, sorted by hierarchical name.
        """
        result = [self]

        sorted_direct_subclasses = sorted(
            self.subclasses, key=lambda subcls: subcls.hierarchical_name
        )
        for subcls in sorted_direct_subclasses:
            result.extend(subcls.type_set)

        return result

    @property
    def ada_kind_range_bounds(self):
        """
        Return the name of the Ada enumerators for the range bounds (first and
        last) of kinds that nodes of this type can have.

        :rtype: (str, str)
        """
        subclasses = self.concrete_subclasses
        return ((subclasses[0].ada_kind_name, subclasses[-1].ada_kind_name)
                if subclasses else
                (self.ada_kind_name, self.ada_kind_name))

    def get_parse_fields(self, predicate=None, include_inherited=True):
        """
        Return the list of all the parse fields `self` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[Field]
        """
        result = self.get_abstract_node_data(predicate, include_inherited,
                                             field_class=Field)
        return result

    def fields_with_accessors(self):
        """
        Return a list of fields for which we must generate accessors in APIs.

        This list excludes inherited/overriden fields so that they are not
        generated multiple times. This list also excludes private fields unless
        the context requires them to be public in the generated library.
        """
        return [f
                for f in self.get_abstract_node_data(
                    include_inherited=False,
                    predicate=lambda f: f.is_public
                )
                if not f.is_overriding]

    def fields_to_initialize(self, include_inherited):
        """
        Return the list of fields to initialize for this node.

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.
        :rtype: list[BaseField]
        """
        return self.get_fields(
            include_inherited=include_inherited,
            predicate=lambda f: not f.abstract and not f.null
        )

    @property  # type: ignore
    @memoized
    def has_fields_initializer(self):
        """
        Return whether this node has a kind-specific fields initializer
        procedure.
        """
        if self.is_root_node:
            return False

        return (self.fields_to_initialize(include_inherited=False) or
                self.base.has_fields_initializer)

    def c_type(self, c_api_settings):
        return CAPIType(c_api_settings, 'base_node')

    @property  # type: ignore
    @memoized
    def hierarchical_name(self):
        """
        Return a name that contains all the base classes for this node type.

        For instance, if C derives from B which itself derives from A, this
        returns "A.B.C".

        :rtype: str
        """
        return '.'.join(node.name.base_name
                        for node in self.get_inheritance_chain())

    @property
    def ada_kind_name(self):
        """
        Return the name of the Ada enumerator to represent this kind of node.
        :rtype: str
        """
        return (get_context().lang_name +
                self.kwless_raw_name).camel_with_underscores

    @property
    def ada_kind_range_name(self):
        """
        Return the name of the Ada kind subtype to represent the set of node
        kinds that include `self` and all its subclasses.

        :rtype: str
        """
        if self.abstract:
            return self.ada_kind_name
        else:
            return self.ada_kind_name + '_Range'

    @property
    def equivalent_to_root(self):
        """
        Return whether this node type is equivalent to the root type.

        For instance, if the root node has only one subclass, then this
        subclass is equivalent to the root type, as all concrete nodes belong
        to both types.

        This property is useful to avoid emitting tautological kind checks
        during code generation.

        :rtype: bool
        """
        return self.is_root_node or TypeSet({self}) == TypeSet({T.root_node})

    # We want structural equality on lists whose elements have the same types.
    # Memoization is one way to make sure that, for each CompiledType instance
    # X: X.list is X.list.
    @property  # type: ignore
    @memoized
    def list(self):
        """
        Return an ASTNodeType subclass that represent a list of `self`.

        :rtype: CompiledType
        """
        result = ASTNodeType(
            name=self.kwless_raw_name + names.Name('List'),
            location=None, doc='',
            base=CompiledTypeRepo.root_grammar_class.generic_list_type,
            fields=[], element_type=self,
            dsl_name='{}.list'.format(self.dsl_name)
        )

        ctx = get_context_or_none()
        if ctx:
            ctx.list_types.add(result._element_type)
        else:
            CompiledTypeRepo.pending_list_types.append(result)

        return result

    def entity_info(self):
        """
        Return the entity info type, which is a record that contains semantic
        information which, when added to an AST node, makes an entity.
        """
        # This is manual memoization. It is necessary because memoization does
        # not play well with class method when we want the memoization to be
        # common to the whole class hierarchy.
        if not CompiledTypeRepo.entity_info:
            CompiledTypeRepo.entity_info = StructType(
                names.Name('Entity_Info'), None, None,
                [
                    (names.Name('Md'), BuiltinField(
                        # Use a deferred type so that the language spec. can
                        # reference entity types even before it declared the
                        # metadata class.
                        T.defer_env_md,
                        doc='The metadata associated to the AST node'
                    )),
                    ('rebindings', BuiltinField(T.EnvRebindings,
                                                access_needs_incref=True,
                                                doc="")),
                    ('from_rebound', BuiltinField(T.Bool, doc=""))
                ],
            )
        return CompiledTypeRepo.entity_info

    @property  # type: ignore
    @memoized
    def entity(self):
        """
        Return the entity type, which is a node type with assorted semantic
        information.
        """
        return EntityType(self)

    def validate_fields(self):
        """
        Perform various checks on this ASTNodeType's fields.

        In particular, make sure:
        * the AST node is type resolved (i.e. all fields have a type);
        * all fields are AST nodes themselves;
        * it does not hold homonym parse fields.

        Emit errors when appropriate.
        """
        parse_fields = self.get_parse_fields()

        # Consider that AST nodes with type annotations for all their fields
        # are type resolved: they don't need to be referenced by the grammar.
        self.is_type_resolved = (
            self.is_type_resolved
            or all(f.has_type for f in parse_fields)
        )
        with self.diagnostic_context:
            check_source_language(
                self.is_type_resolved,
                'Unresolved ASTNode subclass. Use it in the grammar or provide'
                ' a type annotation for all its fields'
            )

        for f in parse_fields:
            with f.diagnostic_context:
                # Null fields must override an abstract one
                check_source_language(
                    not f.null or f.overriding,
                    'Null fields can only be used to override abstract fields',
                )

                # All syntax fields must be nodes
                check_source_language(
                    f.type.is_ast_node,
                    'AST node parse fields must all be AST node themselves.'
                    ' Here, field type is {}'.format(f.type.dsl_name)
                )

        # Unless the special case of inheritted abstract fields/properties,
        # reject fields which are homonym with inherited fields.
        inherited_fields = (self.base.get_abstract_node_data_dict()
                            if self.base else {})

        # Also check that concrete nodes with not-overriden abstract fields
        abstract_fields = {f_n: f_v for f_n, f_v in inherited_fields.items()
                           if isinstance(f_v, Field) and f_v.abstract}

        for f_n, f_v in self._fields.items():
            with f_v.diagnostic_context:
                f_v_abstract_field = isinstance(f_v, Field) and f_v.abstract
                if f_v_abstract_field:
                    abstract_fields[f_v.name.lower] = f_v

                homonym_fld = inherited_fields.get(f_n)
                if not homonym_fld:
                    continue

                check_source_language(
                    not homonym_fld.final,
                    f"{homonym_fld.qualname} is final, overriding it is"
                    " illegal"
                )

                if f_v.is_property:
                    check_source_language(
                        homonym_fld.is_property,
                        'The {} property cannot override {} as the latter is'
                        ' not a property'.format(f_v.qualname,
                                                 homonym_fld.qualname),
                        severity=Severity.non_blocking_error
                    )
                elif (
                    isinstance(f_v, Field) and
                    not f_v.abstract and
                    isinstance(homonym_fld, Field) and
                    homonym_fld.abstract
                ):
                    check_source_language(
                        f_v.type.matches(homonym_fld.type),
                        'Type of overriding field ({}) does not match type of'
                        ' abstract field ({})'
                        .format(f_v.type.dsl_name, homonym_fld.type.dsl_name),
                        severity=Severity.non_blocking_error
                    )
                else:
                    check_source_language(
                        False,
                        '{} cannot override {} unless the former is a concrete'
                        ' field and the latter is an abstract one'
                        .format(f_v.qualname, homonym_fld.qualname),
                        severity=Severity.non_blocking_error
                    )

                if f_n in abstract_fields:
                    abstract_fields.pop(f_n)

        with self.diagnostic_context:
            check_source_language(
                self.abstract or not abstract_fields,
                'This node is concrete, yet it has abstract fields that are'
                ' not overriden: {}'.format(', '.join(sorted(
                    f.qualname for f in abstract_fields.values()
                ))),
                severity=Severity.non_blocking_error
            )

    def builtin_properties(self):
        """
        Return properties available for all AST nodes.

        Note that CompiledTypeRepo.root_grammar_class must be defined
        first.

        :rtype: list[(str, AbstractNodeData)]
        """
        from langkit.expressions import PropertyDef
        from langkit.expressions.astnodes import parents_access_constructor

        # Note that we must not provide implementation for them here (no
        # expression) since the implementation comes from the hard-coded root
        # AST node type definition.
        return [
            # The following fields return lexical env values, which are
            # ref-counted. However these specific envs are owned by the
            # analysis unit, so they are not ref-counted.

            ('node_env', PropertyDef(
                expr=None, prefix=None, type=T.LexicalEnv, public=False,
                external=True, uses_entity_info=True, uses_envs=True,
                optional_entity_info=True, warn_on_unused=False,
                doc='For nodes that introduce a new environment, return the'
                    ' parent lexical environment. Return the "inherited"'
                    ' environment otherwise.'
            )),
            ('children_env', PropertyDef(
                expr=None, prefix=None, type=T.LexicalEnv, public=False,
                external=True, uses_entity_info=True, uses_envs=True,
                optional_entity_info=True, warn_on_unused=False,
                doc='For nodes that introduce a new environment, return it.'
                    ' Return the "inherited" environment otherwise.'
            )),

            ('parent', PropertyDef(
                expr=None, prefix=None, type=T.entity, public=True,
                external=True, uses_entity_info=True, uses_envs=False,
                warn_on_unused=False,
                doc='Return the syntactic parent for this node. Return null'
                    ' for the root node.'
            )),

            # The following builtin fields are implemented as properties, so
            # they follow the ref-counting protocol (function calls return a
            # new ownership share). So unlike access to regular fields, they
            # don't need an additional inc-ref (AbstractNodeData's
            # access_needs_incref constructor argument).
            ('parents', PropertyDef(
                expr=lambda with_self=(T.Bool, True): None, prefix=None,
                type=T.entity.array, public=True, external=True,
                uses_entity_info=True, uses_envs=False, warn_on_unused=False,
                access_constructor=parents_access_constructor,
                doc='Return an array that contains the lexical parents, this'
                    ' node included iff ``with_self`` is True. Nearer parents'
                    ' are first in the list.'
            )),
            ('children', PropertyDef(
                expr=None, prefix=None, type=T.entity.array, public=True,
                external=True, uses_entity_info=True, uses_envs=False,
                warn_on_unused=False,
                doc="""
                Return an array that contains the direct lexical
                children.

                .. warning:: This constructs a whole array every-time you call
                    it, and as such is less efficient than calling the
                    ``Child`` built-in.
                """
            )),
            ('token_start', PropertyDef(
                expr=None, prefix=None, type=T.Token,
                public=True, external=True, uses_entity_info=False,
                uses_envs=False,
                doc='Return the first token used to parse this node.'
            )),
            ('token_end', PropertyDef(
                expr=None, prefix=None, type=T.Token,
                public=True, external=True, uses_entity_info=False,
                uses_envs=False,
                doc='Return the last token used to parse this node.'
            )),
            ('child_index', PropertyDef(
                expr=None, prefix=None, type=T.Int,
                public=True, external=True, uses_entity_info=False,
                uses_envs=False,
                doc="Return the 0-based index for Node in its parent's"
                    " children."
            )),
            ('previous_sibling', PropertyDef(
                expr=None, prefix=None, type=T.entity, public=True,
                external=True, uses_entity_info=True, uses_envs=False,
                warn_on_unused=False,
                doc="""
                Return the node's previous sibling, or null if there is no such
                sibling.
                """
            )),
            ('next_sibling', PropertyDef(
                expr=None, prefix=None, type=T.entity, public=True,
                external=True, uses_entity_info=True, uses_envs=False,
                warn_on_unused=False,
                doc="""
                Return the node's next sibling, or null if there is no such
                sibling.
                """
            )),
            ('unit', PropertyDef(
                expr=None, prefix=None, type=T.AnalysisUnit, public=True,
                external=True, uses_entity_info=False, uses_envs=False,
                warn_on_unused=False,
                doc='Return the analysis unit owning this node.'
            )),
            ('ple_root', PropertyDef(
                expr=None, prefix=None, type=T.root_node, public=False,
                external=True, uses_entity_info=False, uses_envs=False,
                warn_on_unused=False, ignore_warn_on_node=True,
                doc="""
                Return the PLE root that owns this node, or the unit root node
                if this unit has no PLE root.
                """
            )),
            ('is_ghost', PropertyDef(
                expr=None, prefix=None, type=T.Bool, public=True,
                external=True, uses_entity_info=False, uses_envs=False,
                warn_on_unused=False,
                doc="""
                Return whether the node is a ghost.

                Unlike regular nodes, ghost nodes cover no token in the input
                source: they are logically located instead between two tokens.
                Both the ``token_start`` and the ``token_end`` of all ghost
                nodes is the token right after this logical position.
                """
            )),

            ('text', PropertyDef(
                lambda: None,
                prefix=None, type=T.String, public=False,
                external=True, uses_entity_info=False,
                uses_envs=True, warn_on_unused=False, doc="""
                Return the text corresponding to this node. Private property
                (for internal DSL use).
                """
            )),

            ('full_sloc_image', PropertyDef(
                lambda: None,
                prefix=None, type=T.String, public=True,
                external=True, uses_entity_info=False,
                uses_envs=True, warn_on_unused=False, doc="""
                Return a string containing the filename + the sloc in GNU
                conformant format. Useful to create diagnostics from a node.
                """
            )),
        ]

    def snaps(self, anchor_end):
        """
        Whether this node type snaps. To see what this means, see
        langkit.dsl.Annotations documentation.

        Note that no node snaps if unparsers are not requested.

        :param bool anchor_end: If true, return whether this node snaps at the
            end, otherwise return whether it snaps at the beginning.
        :rtype: bool
        """
        from langkit.unparsers import RegularNodeUnparser

        unparser = self.unparser
        i = -1 if anchor_end else 0

        # Unless this node is specifically tagged as snapping, consider it does
        # not snap as soon as it has no unparser (which happens when unparsers
        # are disabled or for abstract nodes) or when it's a list or a token
        # node.
        if self.annotations.snaps:
            return True
        if unparser is None or not isinstance(unparser, RegularNodeUnparser):
            return False

        # This is a regular node with no specific "snaps" annotation, so it
        # snaps if the (un)parser has no leading/training token (optional or
        # not) and if the node for the first/last field snaps itself.
        #
        # And by default, nodes without fields don't snap.
        if not unparser.field_unparsers:
            return False
        field_unparser = unparser.field_unparsers[i]

        anchor_node_tokens = (unparser.post_tokens
                              if anchor_end else unparser.pre_tokens)
        field_node_tokens = (field_unparser.post_tokens
                             if anchor_end else field_unparser.pre_tokens)
        return (not anchor_node_tokens and
                not field_node_tokens and
                field_unparser.field.type.snaps(anchor_end))

    @property
    def snaps_at_start(self):
        return self.snaps(False)

    @property
    def snaps_at_end(self):
        return self.snaps(True)

    def to_public_expr(self, internal_expr):
        result = 'Wrap_Node ({}, {})'.format(internal_expr,
                                             T.entity_info.nullexpr)
        if not self.is_root_node:
            result += '.As_{}'.format(self.entity.api_name)
        return result

    def to_internal_expr(self, public_expr, context=None):
        return '{}.Internal.Node'.format(public_expr)

    def internal_converter(self, from_type):
        """
        Return the name of the converter from bare nodes of type `from_type` to
        bare nodes of type `self`.

        Note that only conversions to or from the root node are supported.

        :rtype: str
        """
        if self.is_root_node:
            assert not from_type.is_root_node
            return 'Convert_From_{}'.format(from_type.kwless_raw_name)
        elif from_type.is_root_node:
            return 'Convert_To_{}'.format(self.kwless_raw_name)
        else:
            assert False

    @property
    def parser_allocator(self):
        """
        Return the name of the function to call in parsers to allocate this
        node.

        :rtype: str
        """
        return 'Allocate_{}'.format(self.kwless_raw_name)


# We tag the ASTNodeType class as abstract here, because of the circular
# dependency between the @abstract decorator and the ASTNodeType class, which
# is caused by the assert statement that is inside the decorator.
ASTNodeType.abstract = True


@dataclass()
class EnumNodeAlternative:
    """
    Synthetic description of an enum node alternative.
    """
    base_name: names.Name
    """
    Name of the alternative, as found in the language spec.
    """

    enum_node: ASTNodeType
    """
    Enum node that owns this alternative.
    """

    alt_node: Opt[ASTNodeType]
    """
    Node that implements this alternative.

    This is None when the EnumNodeAlternative object was just created, as the
    corresponding ASTNodeType is instantiated only after that.
    """

    location: Opt[Location]
    """
    Location in the language spec where this alternative was created. This is
    the location of the enum node declaration itself for alternatives
    synthetized by the @qualifier annotation.
    """

    @property
    def full_name(self):
        """
        Name of the node that implements this alternative. This is the node of
        the enum node, suffixed with the base name.

        :rtype: names.Name
        """
        return self.enum_node.raw_name + self.base_name


class StringType(CompiledType):

    def __init__(self) -> None:
        super().__init__(
            name="StringType",
            exposed=True,
            null_allowed=True,
            is_refcounted=True,
            nullexpr="Empty_String",
            py_nullexpr='""',
            java_nullexpr='new StringWrapper(PointerWrapper.nullPointer())',
            hashable=True,
            has_equivalent_function=True,
            type_repo_name="String",
            api_name="TextType",
            dsl_name="String",
        )

    @property
    def public_requires_boxing(self):
        return True

    @property
    def api_access_name(self) -> names.Name:
        """
        Name of the access type for public strings. Used as internals for
        array struct fields.
        """
        return names.Name("Text_Access")

    def to_public_expr(self, internal_expr):
        return '{}.Content'.format(internal_expr)

    def to_internal_expr(self, public_expr, context=None):
        return 'Create_String ({})'.format(public_expr)


class ArrayType(CompiledType):
    """
    Base class for array types.
    """

    def __init__(self, element_type: CompiledType):
        name = element_type.name + names.Name("Array_Type")
        self.null_constant = names.Name('No') + name

        # By default, array types are not exposed. A compilation pass will tag
        # only the ones that are exposed through the public API.
        super().__init__(
            name=name,
            is_ptr=True,
            is_refcounted=True,
            nullexpr=self.null_constant.camel_with_underscores,
            element_type=element_type,
            null_allowed=True,
            has_equivalent_function=True,
            hashable=element_type.hashable)
        CompiledTypeRepo.array_types.add(self)

        self._requires_unique_function = False
        """
        Whether we need to generate a "Unique" function to remove duplicates
        from such arrays.
        """

        self._requires_vector = False
        """
        Whether we need to generate a Vector type for this array's element
        type. Vectors are in $.Implementation's body, so generating them while
        they are not needed triggers a "not referenced" warning on the package
        instantiation (in addition to make GNAT compile the instantiation for
        nothing)'.
        """

        self._to_iterator_property: PropertyDef
        self._init_fields(self.builtin_properties())

    @property
    def name(self):
        return self.element_type.name + names.Name('Array_Access')

    @property
    def api_name(self):
        """
        Name of the public array type. This is the same as `array_type_name`
        for public types (such as booleans, integers, analysis units, etc.) but
        we have a different one for "wrapped" types, such as entities.
        """
        return self.element_type.api_name + names.Name('Array')

    @property
    def api_access_name(self):
        """
        Name of the access type for public arrays. Used as internals for
        array struct fields.

        :rtype: names.Name
        """
        return self.api_name + names.Name('Access')

    @property
    def constructor_name(self):
        """
        Name of the internal Ada functions to instantiate this array.

        :rtype: names.Name
        """
        return (names.Name('Create') +
                self.element_type.name +
                names.Name('Array'))

    @property
    def dsl_name(self):
        return '{}.array'.format(self.element_type.dsl_name)

    @property
    def array_type_name(self):
        """
        Name of the Ada array type.

        :rtype: names.Name
        """
        return (names.Name('Internal')
                + self.element_type.name
                + names.Name('Array'))

    @property
    def pointed(self):
        """
        Name of the type for values that are pointed to by general values.

        :rtype: names.Name
        """
        return self.element_type.name + names.Name('Array_Record')

    @property
    def pkg_vector(self):
        """
        Name of the Langkit_Support.Vectors package instantiation corresponding
        to this element_type.

        :rtype: names.Name
        """
        return self.element_type.name + names.Name('Vectors')

    def c_type(self, c_api_settings):
        if (
            self.element_type.is_entity_type
            and not self.element_type.emit_c_type
        ):
            return T.entity.array.c_type(c_api_settings)
        else:
            return CAPIType(c_api_settings, self.api_name)

    def index_type(self):
        """
        Name of the index type for this array type.

        :rtype: str
        """
        pkg_vector_name = self.element_type.name + names.Name('Vectors')
        return '{}.Index_Type'.format(pkg_vector_name.camel_with_underscores)

    def vector(self) -> str:
        """
        Name of the type for vector values.
        """
        return self.pkg_vector.camel_with_underscores + '.Vector'

    def c_create(self, capi):
        """
        Name of the C API function to create an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.api_name + names.Name('Create'))

    def c_inc_ref(self, capi):
        """
        Name of the C API function to inc-ref an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.api_name + names.Name('Inc_Ref'))

    def c_dec_ref(self, capi):
        """
        Name of the C API function to dec-ref an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.api_name + names.Name('Dec_Ref'))

    @property
    def py_converter(self):
        """
        Name of the Python class used to convert back and forth between
        user-facing values (lists) and C API values (pointers to array
        records).

        :rtype: str
        """
        return '_{}Converter'.format(self.api_name.camel)

    @property
    def conversion_requires_context(self):
        return self.element_type.conversion_requires_context

    @property
    def to_public_converter(self):
        return names.Name('To_Public') + self.api_name

    @property
    def to_internal_converter(self):
        return names.Name('To_Internal') + self.api_name

    @property
    def emit_c_type(self):
        """
        Return whether to emit a C type for this type.

        See StructType.emit_c_type.

        :rtype: bool
        """
        return (not self.element_type.is_struct_type or
                self.element_type.emit_c_type)

    @property
    def public_requires_boxing(self):
        return True

    @property
    def exposed_types(self):
        return [self.element_type]

    @property
    def requires_unique_function(self):
        return self._requires_unique_function

    def require_unique_function(self):
        self.element_type.require_hash_function()
        self._requires_unique_function = True

    def require_hash_function(self):
        super().require_hash_function()

        # Array hash functions uses the element type's hash function, so
        # it has to be required.
        self.element_type.require_hash_function()

    @property
    def requires_vector(self):
        return self._requires_vector

    def require_vector(self):
        self._requires_vector = True

    @property
    def requires_to_iterator_property(self) -> bool:
        """
        Return whether the `to_iterator` property of this array type should be
        emitted during codegen. This will be true only if the property is ever
        called from within the DSL, since the property is private.
        """
        return self._to_iterator_property.is_reachable

    @property
    def has_early_decl(self) -> bool:
        # The instantiation of the Langkit_Support.Lexical_Env generic packgaes
        # depends on arrays of T.inner_env_assoc, so we need to declare it
        # early.
        return self.element_type == T.inner_env_assoc

    def builtin_properties(self) -> List[Tuple[str, PropertyDef]]:
        """
        Return properties available for all array types.
        """
        from langkit.expressions import PropertyDef, make_to_iterator

        self._to_iterator_property = PropertyDef(
            expr=None, prefix=None,

            # Unless this property is actually used, or the DSL actually
            # references this iterator type, do not generate code for the
            # iterator type.
            type=self.element_type.create_iterator(used=False),

            public=False, external=True, uses_entity_info=False,
            uses_envs=False, optional_entity_info=False, dynamic_vars=[],
            doc='Return an iterator on values of this array',
            access_constructor=make_to_iterator, lazy_field=False,
            artificial=True,
        )

        builtins = [('to_iterator', self._to_iterator_property)]

        # TODO: this is a hack to work around the fact that it is possible for
        # some types to be created after the "compute_base_properties" pass,
        # meaning their _base_property field is never initialized.
        # A real fix would be to have this field be computed lazily on-demand,
        # but it seems to be incompatible with how dispatching properties
        # are expanded (see e54387eb3 for more details).
        for _, property_def in builtins:
            property_def._base_property = None

        return builtins


class IteratorType(CompiledType):
    """
    Base class for iterator types.
    """
    def __init__(self, element_type: CompiledType):
        name = element_type.name + names.Name('Iterator_Type')
        self.null_constant = names.Name('No') + name
        self._is_used = False

        # By default, iterator types are not exposed. A compilation pass will
        # tag only the ones that are exposed through the public API.
        super(IteratorType, self).__init__(
            name=name,
            is_ptr=True,
            is_refcounted=True,
            nullexpr=self.null_constant.camel_with_underscores,
            element_type=element_type,
            has_equivalent_function=False,
            hashable=False,
            exposed=False
        )

        CompiledTypeRepo.iterator_types.append(self)

    @property
    def name(self) -> names.Name:
        return self.element_type.name + names.Name('Iterator_Access')

    @property
    def api_name(self) -> names.Name:
        """
        Name of the public iterator type.
        """
        return self.element_type.api_name + names.Name('Iterator')

    @property
    def is_used(self) -> bool:
        """
        Return whether this iterator type is actually used, and thus whether we
        will emit code for it.
        """
        return self._is_used

    def c_type(self, c_api_settings: CAPISettings) -> CAPIType:
        if (
            self.element_type.is_entity_type
            and not self.element_type.emit_c_type
        ):
            return T.entity.iterator.c_type(c_api_settings)
        else:
            return CAPIType(c_api_settings, self.api_name)

    def c_next(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to get the next value out of the iterator.

        :param capi: Settings for the C API.
        """
        return capi.get_name(self.api_name + names.Name('Next'))

    def c_inc_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to inc-ref an iterator value.

        :param capi: Settings for the C API.
        """
        return capi.get_name(self.api_name + names.Name('Inc_Ref'))

    def c_dec_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to dec-ref an iterator value.

        :param capi: Settings for the C API.
        """
        return capi.get_name(self.api_name + names.Name('Dec_Ref'))

    @property
    def to_public_converter(self) -> names.Name:
        return names.Name('To_Public') + self.api_name

    @property
    def to_internal_converter(self) -> names.Name:
        return names.Name('To_Internal') + self.api_name

    @property
    def emit_c_type(self) -> bool:
        """
        Return whether to emit a C type for this type.

        See StructType.emit_c_type.
        """
        return (not self.element_type.is_struct_type
                or self.element_type.emit_c_type)

    @property
    def iterator_type_name(self) -> names.Name:
        """
        Name of the Ada iterator type.
        """
        return (names.Name('Internal')
                + self.element_type.name
                + names.Name('Iterator'))

    @property
    def exposed_types(self) -> List[CompiledType]:
        return [self.element_type]

    @property
    def has_early_decl(self) -> bool:
        # Code for the "to_iterator" property is generated inside the code for
        # the corresponding array type. This means that we need to emit
        # incomplete decl for iterator types before decls of the corresponding
        # array types. Thus, for array types with early declarations, we need
        # early declarations for iterators, too.
        return self.element_type.array.has_early_decl


class EnumType(CompiledType):
    """
    Ada-like enumeration type.
    """

    def __init__(self,
                 name: Union[str, names.Name],
                 location: Opt[Location],
                 doc: str,
                 value_names: List[names.Name],
                 default_val_name: Opt[names.Name] = None,
                 is_builtin_type: bool = False):
        self.values: List[EnumValue] = [
            EnumValue(self, vn, i) for i, vn in enumerate(value_names)
        ]

        self.values_dict: Dict[names.Name, EnumValue] = {
            v.name: v for v in self.values
        }
        """
        Lookup dictionnary for enumeration values, by name.
        """

        self.default_val_name = default_val_name
        """
        Name of the default value for this enum, if any.
        """

        CompiledTypeRepo.enum_types.append(self)

        self.is_builtin_type = is_builtin_type
        """
        Whether Langkit automatically created this enum type. This is thus
        False for all enum types defined in the user language specification.
        """

        super().__init__(
            name, location, doc, is_ptr=False, exposed=True,
            null_allowed=default_val_name is not None,
            nullexpr=(
                self.values_dict[default_val_name].ada_name
                if default_val_name is not None else None
            ),
            hashable=True
        )

    @property
    def py_helper(self) -> str:
        """
        Name of the class helper in the Python binding layer to convert values
        for this enumeration back and forth between C and Python-level values.
        """
        return self.api_name.camel

    def resolve_value(self, value_name: str) -> AbstractExpression:
        """
        Return an abstract expression corresponding to the given value name.

        :param value_name: Lower-case name of the value to process.
        """
        return (self.values_dict[names.Name.from_lower(value_name)]
                .to_abstract_expr)


class EnumValue:
    """
    Possible value for an enumeration type.
    """

    def __init__(self, enum_type: EnumType, name: names.Name, index: int):
        self.type = enum_type
        """
        Enumeration type that owns this enumeration value.
        """

        self.name = name
        """
        Name for this enumeration value.
        """

        self.index = index
        """
        Index for this enumeration value. Each enumeration values in a given
        enumeration type are assigned an index, incrementing from 0.
        """

    @property
    def dsl_name(self) -> str:
        """
        Return the DSL name for this enumeration value.
        """
        return '{}.{}'.format(self.type.dsl_name, self.name.lower)

    @property
    def ada_name(self) -> str:
        """
        Return the identifier used in Ada to designate this value.
        """
        return self.name.camel_with_underscores

    def c_name(self, c_api_settings: CAPISettings) -> str:
        """
        Return the identifier used in C to designate this value.

        :param c_api_settings: The settings for the C API.
        """
        return '{}_{}'.format(c_api_settings.symbol_prefix.upper(),
                              (self.type.name + self.name).upper)

    @property
    def to_abstract_expr(self) -> AbstractExpression:
        """
        Create an abstract expression wrapping this enumeration value.
        """
        from langkit.expressions import EnumLiteral
        return EnumLiteral(self)


class BigIntegerType(CompiledType):
    def __init__(self):
        super().__init__(
            'BigIntegerType',
            dsl_name='BigInt',
            exposed=True,
            nullexpr='No_Big_Integer',
            is_refcounted=True,
            has_equivalent_function=True,
            is_ada_record=True,
            c_type_name='big_integer',
            api_name='BigInteger')

    @property
    def to_public_converter(self):
        return 'Create_Public_Big_Integer'

    @property
    def to_internal_converter(self):
        return 'Create_Big_Integer'


class AnalysisUnitType(CompiledType):
    def __init__(self):
        from langkit.expressions import PropertyDef

        super().__init__(
            'InternalUnit',
            exposed=True,
            nullexpr='null',
            null_allowed=True,
            hashable=True,
            c_type_name='analysis_unit',
            api_name='AnalysisUnit',
            dsl_name='AnalysisUnit')

        root_field = BuiltinField(
            T.defer_root_node,
            doc='Return the root node of this unit.',
            internal_name=names.Name('Ast_Root'))

        self._init_fields([
            ('root', root_field),
            ('is_referenced_from', PropertyDef(
                lambda unit=T.AnalysisUnit: None,
                prefix=None, type=T.Bool, public=False,
                external=True, uses_entity_info=False,
                uses_envs=True, warn_on_unused=False,
                doc='Return whether this unit is referenced from ``unit``.'
            )),
        ])

    @property
    def to_public_converter(self):
        return 'Wrap_Unit'

    @property
    def to_internal_converter(self):
        return 'Unwrap_Unit'


class SymbolType(CompiledType):
    def __init__(self):
        from langkit.expressions import PropertyDef

        super().__init__(
            'SymbolType',
            dsl_name='Symbol',
            exposed=True,
            nullexpr='null',
            null_allowed=True,

            # See below: symbols are represented in the C API as text records
            is_ada_record=True,
            c_type_name='symbol_type',
            api_name='UnboundedTextType',
            hashable=True,
            conversion_requires_context=True)

        self._init_fields([
            ('image', PropertyDef(
                lambda: None,
                prefix=None, type=T.String, public=False,
                external=True, uses_entity_info=False,
                uses_envs=True, warn_on_unused=False,
                doc='Return this symbol as a string'
            )),
        ])

    def to_public_expr(self, internal_expr):
        return 'To_Unbounded_Text (Image ({}))'.format(internal_expr)

    def to_internal_expr(self, public_expr, context):
        return 'Lookup_Symbol ({}, To_Text ({}))'.format(context, public_expr)


def create_builtin_types():
    """
    Create CompiledType instances for all built-in types. This will
    automatically register them in the current CompiledTypeRepo.
    """
    AnalysisUnitType()

    EnumType(name='AnalysisUnitKind',
             location=None,
             doc="""
             Specify a kind of analysis unit. Specification units provide an
             interface to the outer world while body units provide an
             implementation for the corresponding interface.
             """,
             value_names=[names.Name('Unit_Specification'),
                          names.Name('Unit_Body')],
             is_builtin_type=True)

    CompiledType('RefCategories', null_allowed=False)

    EnumType(name='LookupKind',
             location=None,
             doc="""
             """,
             value_names=[names.Name('Recursive'),
                          names.Name('Flat'),
                          names.Name('Minimal')],
             is_builtin_type=True)
    lex_env_type = CompiledType(
        'LexicalEnv',
        nullexpr='Empty_Env',
        null_allowed=True,
        is_ptr=False,
        is_refcounted=True,
        py_nullexpr='LexicalEnv.Empty',
        hashable=True,
        has_equivalent_function=True,
    )

    LogicVarType()

    CompiledType(
        'LogicEquation',
        dsl_name='Equation',
        nullexpr='Null_Logic_Equation',
        null_allowed=False,
        c_type_name='equation_type',
        is_refcounted=True,
    )

    rebindings = EnvRebindingsType()
    rebindings_parent_field = BuiltinField(
        rebindings, doc='Return the parent rebindings for ``rebindings``.',
        internal_name=names.Name('Parent'))

    rebindings._init_fields([
        ('old_env', BuiltinField(
            lex_env_type,
            doc="""
            Return the lexical environment that is remapped by ``rebindings``.
            """
        )),
        ('new_env', BuiltinField(
            lex_env_type, doc="""
            Return the lexical environment that ``rebindings`` remaps to.
            """
        )),
        ('get_parent', rebindings_parent_field),
    ])

    CompiledType(
        name='Boolean',
        dsl_name='Bool',
        exposed=True,
        is_ptr=False,
        nullexpr='False',
        null_allowed=True,
        py_nullexpr='False',
        java_nullexpr='false',
        hashable=True,

        # "bool" is not a built-in type in C: we define our own type based on
        # uint8_t.
        c_type_name='bool'
    )

    CompiledType(
        name='Integer',
        dsl_name='Int',
        exposed=True,
        is_ptr=False,
        nullexpr='0',
        external=True,
        c_type_name='int',
        hashable=True,
    )

    CompiledType(
        name='Address',
        dsl_name='Address',
        exposed=False,
        is_ptr=False,
        nullexpr='System.Null_Address',
        null_allowed=True,
        external=True,
        hashable=True,
    )

    CompiledType(
        'SourceLocationRange',
        exposed=True,
        is_ptr=False,
        nullexpr='SourceLocationRange()',
    )

    TokenType()
    SymbolType()
    BigIntegerType()
    StringType()

    CompiledType('CharacterType',
                 dsl_name='Character',
                 exposed=True,
                 nullexpr="Chars.NUL",
                 c_type_name='uint32_t',
                 external=True,
                 api_name='CharacterType',
                 hashable=True)

    EnumType(name="DesignatedEnvKind",
             location=None,
             doc="""Discriminant for DesignatedEnv structures.""",
             value_names=[names.Name("None"),
                          names.Name("Current_Env"),
                          names.Name("Named_Env"),
                          names.Name("Direct_Env")],
             default_val_name=names.Name("None"),
             is_builtin_type=True)
    StructType(
        name=names.Name("Designated_Env"),
        location=None,
        doc="""
            Designate an environment for an env spec action.

            The designated environment can be either, depending on the ``Kind``
            field:

            * If ``Kind`` is ``None``, no environment is designated.

            * If ``Kind`` is ``Current_Env``, designate the current environment
              at this point during PLE.

            * If ``Kind`` is ``Named_Env``, designate the environment which
              has precedence for the ``Env_Name`` environment name. If
              ``Env_Name`` is null, this designates to environment.

            * If ``Kind`` is ``Direct_Env``, the direct value for the
              designated environment. That environment must be a primary one
              and cannot be foreign to the node currently processed by PLE. If
              it is the empty environment, do nothing.
        """,
        fields=[
            ("kind", UserField(type=T.DesignatedEnvKind)),
            ("env_name", UserField(type=T.Symbol)),
            ("direct_env", UserField(type=T.LexicalEnv)),
        ],
        is_builtin_type=True,
    )


class TypeRepo:
    """
    Repository of types. Used to be able to do early references to not yet
    declared types, in this fashion::

        T = TypeRepo()

        class A(ASTNode):
            p = AbstractProperty(type=T.B)

        class B(ASTNode):
            pass

    Only Struct and AST node types are reachable through the type repository.
    """

    class Defer:
        """
        Internal class representing a not-yet resolved type.
        """
        def __init__(self, getter, label):
            """
            :param () -> CompiledType getter: A function that will return
                the resolved type when called.
            :param str label: Short description of what this Defer object
                resolves to, for debugging purposes.
            """
            self.getter = getter
            self.label = label

        def get(self):
            """
            Resolve the internally referenced type.

            :rtype: CompiledType
            """
            return self.getter()

        def __getattr__(self, name):
            def get():
                prefix = self.get()

                # The DSL name for automatic ASTNodeType instances for enum
                # node alternatives is: ``Foo.Bar`` where Foo is the name of
                # the (abstract) enum node type and Bar is the name of the
                # alternative. This syntax does not apply directly using
                # TypeRepo shortcut, so handle it explicitly here.
                if (
                    # The following is True iff prefix is an abstract enum node
                    isinstance(prefix, ASTNodeType) and
                    prefix.is_enum_node and
                    not prefix.base.is_enum_node
                ):
                    try:
                        return prefix._alternatives_map[name]
                    except KeyError:
                        pass

                if (
                    name in ('array', 'list', 'iterator', 'entity', 'new')
                    or not isinstance(prefix, BaseStructType)
                ):
                    return getattr(prefix, name)

                try:
                    return prefix._fields[name]
                except KeyError:
                    check_source_language(
                        False,
                        '{prefix} has no {attr} attribute'.format(
                            prefix=(prefix.dsl_name
                                    if isinstance(prefix, CompiledType) else
                                    prefix),
                            attr=repr(name)
                        ),
                        ok_for_codegen=True
                    )
            return TypeRepo.Defer(get, '{}.{}'.format(self.label, name))

        def __call__(self, *args, **kwargs):
            label_args = []
            for arg in args:
                label_args.append(str(arg))
            for kw, arg in kwargs.items():
                label_args.append('{}={}'.format(kw, arg))

            return TypeRepo.Defer(
                lambda: self.get()(*args, **kwargs),
                '{}({})'.format(self.label, ', '.join(label_args))
            )

        def __repr__(self):
            return '<Defer {}>'.format(self.label)

    def __getattr__(self, type_name):
        """
        Build and return a Defer type that references the above type.

        :param str type_name: The name of the rule.
        """
        type_dict = CompiledTypeRepo.type_dict

        def resolve():
            try:
                return type_dict[type_name]
            except KeyError:
                close_matches = difflib.get_close_matches(type_name, type_dict)
                check_source_language(
                    False,
                    'Invalid type name: {}{}'.format(
                        type_name,
                        ', did you one of the following? {}'.format(
                            ', '.join(close_matches)
                        ) if close_matches else ''
                    )
                )

        # Resolve immediately the type reference if possible, except for AST
        # nodes: use a Defer object anyway so that we can support properties
        # reference on top of it.
        result = type_dict.get(type_name)
        return (TypeRepo.Defer(resolve, type_name)
                if result is None or isinstance(result, ASTNodeType) else
                result)

    @property
    def root_node(self):
        """
        Shortcut to get the root AST node.
        :rtype: ASTNodeType
        """
        result = CompiledTypeRepo.root_grammar_class
        assert result
        return result

    @property  # type: ignore
    @memoized
    def node_kind(self):
        """
        Name of type node kind type.
        """
        return self.root_node.entity.api_name + names.Name('Kind_Type')

    @property
    def defer_root_node(self):
        return self.Defer(lambda: self.root_node, 'root_node')

    @property
    def env_md(self):
        """
        Shortcut to get the lexical environment metadata type.
        :rtype: StructType
        """
        assert CompiledTypeRepo.env_metadata is not None
        return CompiledTypeRepo.env_metadata

    @property
    def defer_env_md(self):
        return self.Defer(lambda: self.env_md, '.env_md')

    @property
    def entity_info(self):
        """
        Shortcut to get the entity information type.
        :rtype: StructType
        """
        return CompiledTypeRepo.root_grammar_class.entity_info()

    @property
    def entity(self):
        """
        This property returns the root type used to describe an AST node with
        semantic information attached.
        """
        return self.root_node.entity

    @property  # type: ignore
    @memoized
    def env_assoc(self):
        """
        EnvAssoc type, used to add associations of key and value to the lexical
        environments, via the add_to_env primitive.
        """
        return StructType(
            names.Name('Env_Assoc'), None, None,
            [('key', UserField(type=T.Symbol)),
             ('value', UserField(type=self.defer_root_node)),
             ('dest_env', UserField(type=T.DesignatedEnv)),
             ('metadata', UserField(type=self.defer_env_md))]
        )

    @property  # type: ignore
    @memoized
    def inner_env_assoc(self) -> StructType:
        """
        Return the type to hold "inner environment associations".

        This built-in type is involved in the dynamic primary envs mechanism.
        """
        return StructType(
            names.Name('Inner_Env_Assoc'), None, None,
            [('key', UserField(type=T.Symbol)),
             ('value', UserField(type=self.defer_root_node)),
             ('metadata', UserField(type=self.defer_env_md))]
        )

    @property
    def all_types(self) -> ValuesView[CompiledType]:
        """
        Return all compiled types created so far.
        """
        return CompiledTypeRepo.type_dict.values()


def resolve_type(typeref):
    """
    Resolve a type reference to the actual CompiledType instance.

    :param typeref: Type reference to resolve. It can be either:

        * None: it is directly returned;
        * a CompiledType instance: it is directly returned;
        * a TypeRepo.Defer instance: it is deferred;
        * a DSLType subclass: the corresponding CompiledType instance is
          retrieved;
        * an _EnumNodeAlternative instance: the type corresponding to this
          alternative is retrieved.

    :rtype: CompiledType
    """
    from langkit.dsl import DSLType, _EnumNodeAlternative

    if typeref is None or isinstance(typeref, CompiledType):
        result = typeref

    elif isinstance(typeref, TypeRepo.Defer):
        result = typeref.get()

    elif issubtype(typeref, DSLType):
        result = typeref._resolve()

    elif isinstance(typeref, _EnumNodeAlternative):
        result = typeref.type

    else:
        check_source_language(False,
                              'Invalid type reference: {}'.format(typeref))

    assert result is None or isinstance(result, CompiledType)
    return result


T = TypeRepo()
"""
Default type repository instance, to be used to refer to a type before its
declaration.
"""

create_builtin_types()

CompiledTypeOrDefer = Union[CompiledType, TypeRepo.Defer]
