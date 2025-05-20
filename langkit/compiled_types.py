from __future__ import annotations

import abc
from collections import OrderedDict
from dataclasses import dataclass
from itertools import count, takewhile
import shlex
from typing import (
    Callable,
    ClassVar,
    Iterator,
    Sequence,
    TYPE_CHECKING,
    Type,
    TypeVar,
    ValuesView,
    overload,
)

from langkit import names
from langkit.c_api import CAPISettings, CAPIType
from langkit.common import is_keyword
from langkit.compile_context import CompileCtx, get_context
from langkit.diagnostics import (
    DiagnosticContext,
    Location,
    WarningSet,
    check_source_language,
    error,
)
import langkit.names
from langkit.utils import memoized, self_memoized
from langkit.utils.text import append_paragraph, first_line_indentation, indent
from langkit.utils.types import TypeSet


if TYPE_CHECKING:
    from typing_extensions import Self as _Self

    from langkit.envs import EnvSpec
    import langkit.expressions as E
    from langkit.generic_interface import (
        GenericInterface,
        InterfaceMethodProfile,
    )
    from langkit.lexer import TokenAction
    from langkit.parsers import Parser, _Transform
    from langkit.unparsers import NodeUnparser

    import liblktlang as L

    # Type alias so that we can refer to the list type in ASTNodeType method,
    # as ASTNodeType has a "list" property.
    builtin_list = list

    AbstractNodeDataType = TypeVar(
        "AbstractNodeDataType", bound="AbstractNodeData"
    )


def gdb_helper(*args: str) -> str:
    """
    Format given arguments into a special Ada comment for GDB helpers.

    :param list[str] args: Elements of the special comment.
    """
    return "--# {}".format(shlex.join(args))


def type_ref_list_doc(types: Sequence[CompiledType]) -> str:
    """
    Helper to format a list of type references for the Sphinx documentation.
    """
    return ", ".join(sorted(f":typeref:`{t.type_repo_name}`" for t in types))


@CompileCtx.register_template_extensions
def template_extensions(ctx: CompileCtx) -> dict:
    capi = ctx.c_api_settings
    root_entity = ctx.root_node_type.entity

    return {
        "names": names,
        "grammar_rule_type": T.GrammarRule.c_type(capi).name,
        "default_grammar_rule": capi.get_name("default_grammar_rule"),
        "root_entity": root_entity,
        "entity_array": root_entity.array.api_name,
        "ctx": ctx,
        "cfg": ctx.config,
        "ada_lib_name": ctx.ada_api_settings.lib_name,
        "T": T,
        "ada_api": ctx.ada_api_settings,
        "capi": capi,
        "bool_type": T.Bool.c_type(capi).name,
        "analysis_context_type": CAPIType(capi, "analysis_context").name,
        "analysis_unit_type": T.AnalysisUnit.c_type(capi).name,
        "node_kind_type": CAPIType(capi, "node_kind_enum").name,
        "node_type": ctx.root_node_type.c_type(capi).name,
        "entity_type": T.entity.c_type(capi).name,
        "stack_trace_type": CAPIType(capi, "stack_trace").name,
        "symbol_type": T.Symbol.c_type(capi).name,
        "string_type": T.String.c_type(capi).name,
        "env_rebindings_type": T.EnvRebindings.c_type(capi).name,
        "unit_kind_type": T.AnalysisUnitKind.c_type(capi).name,
        "event_handler_type": CAPIType(capi, "event_handler").name,
        "event_handler_destroy_type": CAPIType(
            capi, "event_handler_destroy_callback"
        ).name,
        "event_handler_unit_requested_type": CAPIType(
            capi, "event_handler_unit_requested_callback"
        ).name,
        "event_handler_unit_parsed_type": CAPIType(
            capi, "event_handler_unit_parsed_callback"
        ).name,
        "file_reader_type": CAPIType(capi, "file_reader").name,
        "file_reader_destroy_type": CAPIType(
            capi, "file_reader_destroy_callback"
        ).name,
        "file_reader_read_type": CAPIType(
            capi, "file_reader_read_callback"
        ).name,
        "unit_provider_type": CAPIType(capi, "unit_provider").name,
        "unit_provider_destroy_type": CAPIType(
            capi, "unit_provider_destroy_callback"
        ).name,
        "unit_provider_get_unit_filename_type": CAPIType(
            capi, "unit_provider_get_unit_filename_callback"
        ).name,
        "unit_provider_get_unit_from_name_type": CAPIType(
            capi, "unit_provider_get_unit_from_name_callback"
        ).name,
        "token_kind": CAPIType(capi, "token_kind").name,
        "tdh_ptr_type": CAPIType(capi, "token_data_handler").name,
        "token_type": CAPIType(capi, "token").name,
        "sloc_type": CAPIType(capi, "source_location").name,
        "sloc_range_type": T.SourceLocationRange.c_type(capi).name,
        "text_type": CAPIType(capi, "text").name,
        "big_integer_type": CAPIType(capi, "big_integer").name,
        "diagnostic_type": CAPIType(capi, "diagnostic").name,
        "exception_type": CAPIType(capi, "exception").name,
        "exception_kind_type": CAPIType(capi, "exception_kind").name,
        "introspection_member_ref_type": (
            CAPIType(capi, "introspection_member_ref").name
        ),
        "rewriting_handle_type": (CAPIType(capi, "rewriting_handle").name),
        "unit_rewriting_handle_type": (
            CAPIType(capi, "unit_rewriting_handle").name
        ),
        "node_rewriting_handle_type": (
            CAPIType(capi, "node_rewriting_handle").name
        ),
        "rewriting_apply_result_type": (
            CAPIType(capi, "rewriting_apply_result").name
        ),
        "Field": Field,
        "TypeSet": TypeSet,
    }


class CompiledTypeRepo:
    """
    Namespace class: Repository for every compiled type. This is used to have a
    comprehensive list of every compiled type instance, so that you can use a
    TypeRepo instance to refer to any compiled type.
    """

    type_dict: dict[str, CompiledType] = {}
    """
    Mapping: type name -> CompiledType instance. Used in TypeRepo for type
    lookup by name.
    """

    @classmethod
    def reset(cls) -> None:
        """
        Make this holder empty again. Useful to use Langkit multiple times in a
        process.
        """
        cls.type_dict = {}


@dataclass
class MemberNames:
    """
    Set of names for a given compiled type member (field, property, ...).
    """

    index: str
    """
    Name used as an index in compiled type-level member lookup tables.

    This must be the lower-cased version of ``spec`` if ``spec`` is defined, so
    that ``.foo`` references in Lkt code correctly resolve to the ``foo``
    member. For internal members, this should contain invalid identifier names
    so that Lkt code cannot reference them.

    An overriding member must have the same index name as its base member, so
    that consolidated lookup tables have only one entry for the full overriding
    hierarchy.
    """

    codegen: str
    """
    Internal name to be used in code generation (Ada implementation packages):
    function name for properties/lazy fields, component name for fields, ...
    """

    spec: names.Name | None = None
    """
    Name as defined in the language spec. This can be ``None`` for internal
    members, that are not meant to be accessed from the language spec itself.
    """

    api: names.Name | None = None
    """
    Public members need to have a name, to be used in generated APIs.
    """

    @staticmethod
    def for_internal(context: CompileCtx, radix: str) -> MemberNames:
        """
        Return names for an internal member.

        Internal members are not accessible from language spec code: the index
        is not a valid identifier and there is no spec/API name.

        :param context: Compilation conetxt that will own this member.
        :param radix: Radix used to create unique index/code generation names.
            In principle a simple counter would do, but including a meaningful
            radix in the names makes it easier to understand generated code,
            and simplifies debugging in general: better "env_mappings_42" than
            "internal_prop_42".
        """
        name = names.Name.from_lower(
            f"internal_{radix}_{next(context.internal_members_counter)}"
        )
        return MemberNames(
            index=f"[internal]{name.lower}",
            codegen=name.camel_with_underscores,
            spec=None,
            api=None,
        )

    @staticmethod
    def for_struct_field(
        spec: names.Name | str, codegen: str | None = None
    ) -> MemberNames:
        """
        Return names for a struct field.

        :param spec: Lower case (if ``str``) name for this field, as declared
            in the language specification.
        :param codegen: Camel-with-underscores name to use for code generation.
            If left to None, an automatic one is generated.
        """
        spec_name = (
            spec
            if isinstance(spec, names.Name)
            else names.Name.from_lower(spec)
        )
        return MemberNames(
            index=spec_name.lower,
            codegen=codegen if codegen else spec_name.camel_with_underscores,
            spec=spec_name,
            api=spec_name,
        )

    @staticmethod
    def for_node_field(
        owner: CompiledType,
        spec: names.Name | str,
    ) -> MemberNames:
        """
        Return names for a parse node field.

        :param owner: Compiled type that owns the member.
        :param spec: Lower case (if ``str``) name for this field, as declared
            in the language specification.
        """
        return MemberNames._for_spec(
            owner,
            MemberNames.prefix_field,
            spec,
            builtin=False,
        )

    @staticmethod
    def for_property(
        owner: CompiledType,
        spec: names.Name | str,
        builtin: bool = False,
    ) -> MemberNames:
        """
        Return names for a property.

        :param owner: Compiled type that owns the member.
        :param spec: Lower case (if ``str``) name for this field, as declared
            in the language specification.
        :param builtin: Whether this is a builtin property.
        """
        return MemberNames._for_spec(
            owner,
            MemberNames.prefix_property,
            spec,
            builtin,
        )

    @staticmethod
    def for_lazy_field(
        owner: CompiledType,
        spec: names.Name | str,
    ) -> MemberNames:
        """
        Return names for a lazy field.

        :param owner: Compiled type that owns the member.
        :param spec: Lower case (if ``str``) name for this field, as declared
            in the language specification.
        """
        return MemberNames._for_spec(
            owner,
            MemberNames.prefix_field,
            spec,
            builtin=False,
        )

    # Possible values for ``_for_spec``'s ``prefix`` argument
    prefix_field: ClassVar[names.Name] = names.Name("F")
    prefix_property: ClassVar[names.Name] = names.Name("P")
    prefix_internal: ClassVar[names.Name] = names.Name("Internal")

    @staticmethod
    def _for_spec(
        owner: CompiledType,
        prefix: names.Name,
        spec: names.Name | str,
        builtin: bool,
    ) -> MemberNames:
        """
        Common logic to create member names.

        :param owner: Compiled type that owns the member.
        :param prefix: Short prefix to include in API and code generation
            names.
        :param spec: Lower case (if ``str``) name for this field, as declared
            in the language specification.
        :param builtin: Whether this is a builtin member. We do not decorate
            the API nor code generation names for them.
        """
        spec_name = (
            spec
            if isinstance(spec, names.Name)
            else names.Name.from_lower(spec)
        )

        # In nodes, use a qualified name for code generation to avoid name
        # conflicts between homonym fields/properties in the whole node type
        # hierarchy.
        #
        # There is one exception: built-in properties must not be decorated for
        # convenience in template code.
        codegen = (
            owner.kwless_raw_name + prefix + spec_name
            if isinstance(owner, ASTNodeType) and not builtin
            else spec_name
        )

        return MemberNames(
            index=spec_name.lower,
            codegen=codegen.camel_with_underscores,
            spec=spec_name,
            api=spec_name if builtin else prefix + spec_name,
        )


_OwnerType = TypeVar("_OwnerType", bound="CompiledType")


@overload
def fields_callback_wrapper(
    ct: Type[_OwnerType],
    callback: Callable[[_OwnerType], Sequence[AbstractNodeData]],
) -> Callable[[CompiledType], Sequence[AbstractNodeData]]: ...


@overload
def fields_callback_wrapper(ct: Type[_OwnerType], callback: None) -> None: ...


def fields_callback_wrapper(
    ct: Type[_OwnerType],
    callback: Callable[[_OwnerType], Sequence[AbstractNodeData]] | None,
) -> Callable[[CompiledType], Sequence[AbstractNodeData]] | None:
    """
    Wrap a ``fields`` callback from a member class constructor.

    ``CompiledType.__init__`` takes (1) a callback that accepts
    ``CompiledType`` instances, however we want subclasses like
    ``StructType.__init__`` to take (2) callbacks that accept
    ``BaseStructType`` values.

    Since function types are contravariant in types of arguments, (2) is not a
    subtype of (1), so for type checking, this function returns a wrapper for
    (2) callbacks that matches (1).
    """
    if callback is None:
        return None
    non_none_callback = callback

    def wrapper(t: CompiledType) -> Sequence[AbstractNodeData]:
        assert isinstance(t, ct)
        return non_none_callback(t)

    return wrapper


class AbstractNodeData(abc.ABC):
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

    kind_name: ClassVar[str]
    """
    Human-readable name for this kind of node data. Used to format diagnostics.
    """

    # Name to use for the implicit entity information argument in field
    # accessors.
    entity_info_name: ClassVar[names.Name] = names.Name("E_Info")

    _abstract: bool
    location: Location
    names: MemberNames
    optional_entity_info: bool
    owner: CompiledType
    type: CompiledType

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        location: Location,
        type: CompiledType,
        public: bool = True,
        default_value: E.BindableLiteralExpr | None = None,
        abstract: bool = False,
        access_needs_incref: bool = False,
        internal_name: langkit.names.Name | None = None,
        access_constructor: (
            Callable[
                [
                    E.ExprDebugInfo | None,
                    E.Expr,
                    AbstractNodeData,
                    list[E.Expr | None],
                ],
                E.Expr,
            ]
            | None
        ) = None,
        final: bool = False,
        implements: Callable[[], InterfaceMethodProfile] | None = None,
    ):
        """
        :param owner: Compiled type that owns this member.

        :param names: Names for this member.

        :param location: Source location of the declaration for this member.

        :param type: Type for this member (fields) or return type (properties).

        :param public: Whether this AbstractNodeData instance is supposed to be
            public or not.

            In the context of properties only, None is also allowed: in this
            case, inherit vibility from parents. If there is no property to
            override and None is passed, make the property private. This is
            computed in the "compute" pass.

        :param default_value: Default value for this field, when omitted from
            New expressions.

        :param abstract: Whether this member is abstract.

        :param access_needs_incref: If True, field access evaluation does not
            create an ownership share: callers must call Inc_Ref themselves.
            See the eponym property.

        :param internal_name: If provided, override the default name to use in
            code generation for this node data.

        :param access_constructor: Optional callback to create a resolved
            expression that implements a DSL access to this field. If
            provided, this overrides the default code generation of
            ``EvalMemberExpr``.

        :param final: If True, this field/property cannot be overriden. This is
            possible only for concrete fields/properties.

        :param implements: If provided, callback that returns the generic
            interface method that this member implements.
        """

        self._serial = next(self._counter)
        self._is_public = public

        self.context = owner.context
        self.owner = owner
        self.names = names
        self.location = location
        self.type = type

        self.arguments: list[Argument] = []
        """
        Code generation-wise, all node data can be considered as functions
        which take at least a mandatory Self argument and return the
        corresponding data.

        This is a list that describes all other arguments. Note that only
        Property instances accept other arguments.
        """

        self._uses_entity_info: bool | None = False
        self._has_self_entity = False
        self.optional_entity_info = False
        self._access_needs_incref = access_needs_incref
        self.default_value = default_value
        self.access_constructor = access_constructor

        self._abstract = abstract
        self._inheritance_computed = False
        self._base: _Self | None = None
        self._overridings: list[_Self] = []
        self.final = final

        self.implements: InterfaceMethodProfile | None = None
        """
        Generic interface method that this type member implements, if any.
        """

        if implements:
            self.context.deferred.implemented_methods.add(self, implements)

        owner._add_field(self)

    def __lt__(self, other: AbstractNodeData) -> bool:
        return self._serial < other._serial

    @property
    def abstract(self) -> bool:
        """
        Return whether this field is abstract. This can be true only for
        properties and syntax fields.
        """
        return self._abstract

    @property
    def base(self) -> _Self | None:
        """
        If this field overrides an inherited one in a base class, return the
        inherited one, otherwise return None.
        """
        assert self._inheritance_computed
        return self._base

    @property
    def root(self) -> _Self:
        """
        Return the ultimate field that ``self`` inherits.

        This returns ``self`` itself if it is not overriding.
        """
        result = self
        while result.base:
            result = result.base
        return result

    @property
    def is_overriding(self) -> bool:
        """
        Return whether this field overrides an inheritted one in a base class.
        """
        return self.base is not None

    @property
    def overridings(self) -> Sequence[_Self]:
        """
        Return the list of fields that override ``self``.
        """
        return self._overridings

    @property
    def concrete_overridings(self) -> Sequence[_Self]:
        """
        Return the list of concrete overridings for ``self``.
        """
        return [f for f in self.overridings if not f.abstract]

    @property  # type: ignore
    @self_memoized
    def all_overridings(self) -> list[_Self]:
        """
        Return self's overriding fields and all their own overriding ones,
        recursively.
        """

        def helper(field: _Self, except_self: bool = False) -> list[_Self]:
            return sum(
                (helper(f) for f in field.overridings),
                [] if except_self else [field],
            )

        return helper(self, except_self=True)

    def field_set(self) -> Sequence[_Self]:
        """
        Return all fields associated with this fields in terms of overriding
        hierarchy.
        """
        return (
            self.base.field_set()
            if self.base
            else [self] + self.all_overridings
        )

    def reset_inheritance_info(self) -> None:
        """
        Reset memoization caches for inheritance-related information.

        Must be called on the root field when modifying a tree of inherited
        fields.
        """
        assert self.base is None
        for f in [self] + self.all_overridings:
            f._base = None
            f._overridings = []
        all_overridings_memoizer = getattr(
            AbstractNodeData.all_overridings, "fget"
        )
        all_overridings_memoizer.reset(self)

    @property
    def uses_entity_info(self) -> bool:
        """
        Return whether evaluating this field requires entity info.
        """
        assert self._uses_entity_info is not None
        return self._uses_entity_info

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
        return self.names.spec is None

    @property
    def public_type(self) -> CompiledType:
        return self.type.public_type

    def c_type_or_error(self, capi: CAPISettings) -> CAPIType:
        """
        Within a diagnostic context for this field, return its C API type.
        """
        return self.public_type.c_type(capi)

    @property
    def internal_name(self) -> str:
        """
        Name of the field in the generated code.
        """
        return self.names.codegen
        if self._internal_name:
            return self._internal_name
        else:
            return self.name

    @property
    def original_name(self) -> str:
        """
        Name for this property as specified in Lkt sources.

        For internal properties, we generally use impossible syntax (brackets,
        leading underscore, ...) to avoid name clashes with user-defined
        properties.
        """
        assert self.names.spec
        return self.names.spec.lower

    @property
    def indexing_name(self) -> str:
        """
        Name to use for this node data in structure field dicts.

        For user fields created by users, this should be the lower case form
        that appears in Lkt sources. For fields created by the compilation
        process, this is arbitrary.
        """
        return self.names.index

    @property
    def qual_impl_name(self) -> str:
        """
        Fully qualified name for the implementation of this property.

        This is useful during code generation to avoid name clashes.
        """
        import langkit.expressions as E

        if isinstance(self, E.PropertyDef):
            return "{}.Implementation{}.{}".format(
                self.context.ada_api_settings.lib_name,
                ".Extensions" if self.user_external else "",
                self.internal_name,
            )
        else:
            return self.internal_name

    @property  # type: ignore
    @memoized
    def api_name(self) -> langkit.names.Name:
        """
        Return the name to use for this node data in public APIs.
        """
        assert self.is_public
        assert self.names.api is not None
        return self.names.api

    @property
    def qualname(self) -> str:
        """
        Return the qualified name for this field, i.e. the name of the owning
        type plus the name of the field itself. This is useful for diagnostic
        messages.

        Note that if expansion renamed this property, this will return the
        original (Lkt-level) name.
        """
        return f"{self.owner.lkt_name}.{self.names.index}"

    def __repr__(self) -> str:
        return "<{} {}>".format(type(self).__name__, self.qualname)

    @abc.abstractproperty
    def doc(self) -> str:
        """
        Documentation for the abstract node field.
        """
        ...

    @property
    def accessor_basename(self) -> langkit.names.Name:
        """
        Return the base name for the C API accessor we generate for this field.

        Note that this is available only for fields attached to parse nodes.
        """
        assert isinstance(self.owner, ASTNodeType)
        return self.owner.kwless_raw_name + self.api_name

    @property
    def natural_arguments(self) -> list[Argument]:
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

    @property
    def ada_default_value(self) -> str:
        """
        Return the default value to use for the record component declaration
        that implements this field in generated code.
        """
        return (
            self.type.storage_nullexpr
            if self.default_value is None
            else self.default_value.render_expr()
        )


class NoNullexprError(Exception):
    """
    Exception raised by "nullexpr" properties when no null expression is
    available.
    """

    pass


class CompiledType:
    """
    Descriptor for a type in the generated code.
    """

    def __init__(
        self,
        context: CompileCtx,
        name: str | names.Name,
        location: Location,
        doc: str = "",
        base: _Self | None = None,
        fields: (
            Callable[[CompiledType], Sequence[AbstractNodeData]] | None
        ) = None,
        is_ptr: bool = True,
        has_special_storage: bool = False,
        is_list_type: bool = False,
        is_entity_type: bool = False,
        exposed: bool = False,
        c_type_name: str | None = None,
        external: bool = False,
        null_allowed: bool = False,
        is_ada_record: bool = False,
        is_refcounted: bool = False,
        nullexpr: str | None = None,
        py_nullexpr: str | None = None,
        java_nullexpr: str | None = None,
        element_type: CompiledType | None = None,
        hashable: bool = False,
        has_equivalent_function: bool = False,
        type_repo_name: str | None = None,
        api_name: str | names.Name | None = None,
        lkt_name: str | None = None,
        conversion_requires_context: bool = False,
    ) -> None:
        """
        :param context: Compilation context that owns this type.

        :param name: Type name. If a string, it must be camel-case.

        :param location: Location of the declaration of this compiled type.

        :param str doc: User documentation for this type.

        :param base: If this type derives from another type T, this is T.

        :param fields: Callback that takes the compiled type instance and
            returns a list of fields to include for this type. The callback is
            evaluated as soon as all named types are known.

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

        :param lkt_name: If provided, name used to represent this type at the
            Lkt level. Useful to format diagnostics.

        :param conversion_requires_context: Whether converting this type from
            public to internal values requires an analysis context.
        """
        assert isinstance(context, CompileCtx)
        if isinstance(name, str):
            name = names.Name.from_camel(name)
        if isinstance(api_name, str):
            api_name = names.Name.from_camel(api_name)

        self.context = context
        self._name = name
        self.location = location
        self._doc = doc
        self._doc_location: Location | None = None
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
        self._lkt_name = lkt_name

        self.type_repo_name = type_repo_name or lkt_name or name.camel
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

        self.derivations: set[CompiledType] = set()
        """
        Set of types that derive from ``self``.
        """

        self._abstract_node_data_dict_cache: dict[
            tuple[bool, Type[AbstractNodeData]],
            dict[str, AbstractNodeData],
        ] = {}
        """
        Cache for the get_abstract_node_data_dict class method.
        """

        self._fields: dict[str, AbstractNodeData] = OrderedDict()
        """
        List of AbstractNodeData fields for this type.
        """

        if fields:
            fields_callback = fields
            context.deferred.type_members.add(
                self, lambda: fields_callback(self)
            )

        # If "self" derives from another type, register it
        if self._base is not None:
            self._base.derivations.add(self)

        self._iterator: IteratorType | None = None
        """
        Iterator for "self". Created on-demand (see the "create_iterator"
        method and "iterator" property).
        """

    @property
    def base(self) -> _Self | None:
        """
        Return the base of this type, or None if there is no derivation
        involved.
        """
        return self._base

    def get_inheritance_chain(self) -> list[_Self]:
        """
        Return the chain of types following the `base` link as a list.
        Root-most types come first.
        """
        t: _Self | None = self
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
        return self.context.python_api_settings.type_public_name(self)

    def __lt__(self, other: object) -> bool:
        assert isinstance(other, CompiledType)
        return self.name < other.name

    @property
    def public_type(self) -> CompiledType:
        """
        Return the type to use when exposing values in public APIs.

        This returns `self` for most types, but some (such as bare nodes) are
        automatically wrapped as entity.
        """
        return self

    @property
    def conversion_requires_context(self) -> bool:
        return self._conversion_requires_context

    @property
    def has_equivalent_function(self) -> bool:
        return self._has_equivalent_function

    @property
    def requires_hash_function(self) -> bool:
        """
        Return whether code generation must produce a Hash function for this
        type.
        """
        return self != T.env_md and self._requires_hash_function

    def require_hash_function(self) -> None:
        """
        Tag this type as requiring a hash function.
        """
        self._requires_hash_function = True

    def add_as_memoization_key(self, context: CompileCtx) -> None:
        """
        Add `self` to the set of types that are used as keys in the hashed maps
        used to implement properties memoization. It has to be hashable.
        """
        assert self.hashable, "Trying to use {} as hashable type".format(
            self.lkt_name
        )
        context.memoization_keys.add(self)
        self.require_hash_function()

    def add_as_memoization_value(self, context: CompileCtx) -> None:
        """
        Add `self` to the set of types that are used as values in the hashed
        maps used to implement properties memoization.
        """
        context.memoization_values.add(self)

    @property
    def memoization_kind(self) -> str:
        """
        Return the enumerator name that corresponds to this type for the
        discriminated record to materialize memoization keys/values.
        """
        return (names.Name("Mmz") + self.name).camel_with_underscores

    @property
    def name(self) -> names.Name:
        """
        Name of the type for general values in the Ada generated code.
        """
        return self._name

    @property
    def api_name(self) -> names.Name:
        """
        Name of public type. By default, it is the same as `name`.
        """
        return self.name if self._api_name is None else self._api_name

    @property
    def qualified_api_name(self) -> str:
        """
        Return the Ada qualified name to use for this type in public APIs.
        """
        result = self.api_name.camel_with_underscores
        prefix = None
        if self.is_enum_type:
            prefix = "Common"
        elif not (
            self.is_big_int_type
            or self.is_bool_type
            or self.is_character_type
            or self.is_env_rebindings_type
            or self.is_env_rebindings_type
            or self.is_int_type
            or self.is_source_location_type
            or self.is_string_type
            or self.is_symbol_type
            or self.is_token_type
        ):
            prefix = "Analysis"

        return f"{prefix}.{result}" if prefix else result

    @property
    def lkt_name(self) -> str:
        """
        Type name as it appears in Lkt source code. To be used in diagnostics.
        """
        return self._lkt_name or self.name.camel

    @property
    def to_public_converter(self) -> names.Name | None:
        """
        If this type requires a conversion in public properties, return the
        name of the function that takes an internal value and returns a public
        one. Return None otherwise.
        """
        return None

    @property
    def to_internal_converter(self) -> names.Name | None:
        """
        If this type requires a conversion in public properties, return the
        name of the function that takes a public value and returns an internal
        one. Return None otherwise.
        """
        return None

    def to_public_expr(self, internal_expr: str) -> str:
        """
        Given ``internal_expr``, an expression that computes an internal value,
        for this type return another expression that converts it to a public
        value.
        """
        if self.to_public_converter:
            return "{} ({})".format(self.to_public_converter, internal_expr)
        else:
            # By default, assume public and internal types are identical, i.e.
            # that we can return the internal value as-is.
            assert self.name == self.api_name, (
                "Trying to avoid conversion while internal type is {} and"
                " public type is {}".format(self.name, self.api_name)
            )
            return internal_expr

    def to_internal_expr(
        self,
        public_expr: str,
        context: str | None = None,
    ) -> str:
        """
        Given ``public_expr``, an expression that computes a public value, for
        this type return another expression that converts it to an internal
        value.

        :param context: If this type requires the context for this conversion,
            this must be an expression that yields the context.
        """
        requires_context = self.conversion_requires_context
        assert not (requires_context and context is None)

        if self.to_internal_converter:
            if requires_context:
                assert context is not None
                context_suffix = f", {context}"
            else:
                context_suffix = ""
            return "{} ({}{})".format(
                self.to_internal_converter, public_expr, context_suffix
            )
        else:
            # By default, assume public and internal types are identical, i.e.
            # that we can return the internal value as-is.
            assert self.name == self.api_name
            return public_expr

    def __repr__(self) -> str:
        return f"<{type(self).__name__} {self.lkt_name}>"

    @property
    def doc(self) -> str:
        """
        Return the user documentation for this type, or None if there is no
        documentation.
        """
        return self._doc

    @property
    def is_analysis_unit_type(self) -> bool:
        """
        Return whether this is the analysis unit type.
        """
        return self == T.AnalysisUnit

    @property
    def is_analysis_unit_kind(self) -> bool:
        """
        Return whether this is the analysis unit kind type.
        """
        return self == T.AnalysisUnitKind

    @property
    def is_array_type(self) -> bool:
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
    def is_set_type(self) -> bool:
        """
        Return whether this is an instance of SetType.
        """
        return isinstance(self, SetType)

    @property
    def is_bool_type(self) -> bool:
        """
        Return whether this is the boolean type.
        """
        return self == T.Bool

    @property
    def is_int_type(self) -> bool:
        """
        Return whether this is the integer type.
        """
        return self == T.Int

    @property
    def is_big_int_type(self) -> bool:
        """
        Return whether this is the big integer type.
        """
        return self == T.BigInt

    @property
    def is_enum_type(self) -> bool:
        """
        Return whether this is an enumeration type.
        """
        return isinstance(self, EnumType)

    @property
    def is_collection(self) -> bool:
        """
        Return whether this is a collection type.
        """
        return self._element_type is not None

    @property
    def is_env_rebindings_type(self) -> bool:
        """
        Return whether this is the env rebindings type.
        """
        return self == T.EnvRebindings

    @property
    def is_equation_type(self) -> bool:
        """
        Return whether this is the equation type.
        """
        return self == T.Equation

    @property
    def is_lexical_env_type(self) -> bool:
        """
        Return whether this is the lexical environment type.
        """
        return self == T.LexicalEnv

    @property
    def is_logic_var_type(self) -> bool:
        """
        Return whether this is an instance of LogicVarType.
        """
        return isinstance(self, LogicVarType)

    @property
    def is_long_type(self) -> bool:
        """
        Return whether this is a integer type.
        """
        return self == T.Int

    @property
    def is_character_type(self) -> bool:
        """
        Return whether this is a character type.
        """
        return self == T.Char

    @property
    def is_string_type(self) -> bool:
        """
        Return whether this is the string type.
        """
        return self == T.String

    @property
    def is_symbol_type(self) -> bool:
        """
        Return whether this is a symbol type.
        """
        return self == T.Symbol

    @property
    def is_token_type(self) -> bool:
        """
        Return whether this is a token type.
        """
        return self == T.Token

    @property
    def is_source_location_type(self) -> bool:
        """
        Return whether this is a source location type.
        """
        return self == T.SourceLocation

    @property
    def is_big_integer_type(self) -> bool:
        """
        Return whether this is a big integer type.
        """
        return self == T.BigInt

    @property
    def is_root_node(self) -> bool:
        """
        Return whether this is the root node.
        """
        return isinstance(self, ASTNodeType) and self._is_root_node

    @property
    def element_type(self) -> CompiledType:
        """
        Assuming this is a collection type (array, iterator, list) or an
        entity, return the corresponding element type.
        """
        assert self._element_type
        return self._element_type

    @property
    def is_refcounted(self) -> bool:
        """
        Return whether this type matters for the ref-counting mechanism.

        This returns whether this type is dynamically allocated and thus
        ref-counted, or whether it owns ref-counted values. "dynamically" in
        this context means: with a lifetime that is different from the owning
        analysis unit.

        If it is ref-counted, there must exist Inc_Ref and Dec_Ref primitives.
        The Dec_Ref primitive must handle null values correctly (i.e. ignore
        them).
        """
        return self._is_refcounted

    @property
    def storage_type_name(self) -> names.Name:
        """
        Return the name of the type that is used to store instances of this
        type in structs and AST nodes. See documentation for
        has_special_storage.
        """
        return self.name

    def extract_from_storage_expr(self, node_expr: str, base_expr: str) -> str:
        """
        Turn a storage value into a public value. See documentation for
        has_special_storage.

        :param node_expr: Expression that yields the node that owns the storage
            value.
        :param base_expr: Expression that yields the storage value.
        :return: An expression that yields the public value.
        """
        return base_expr

    def convert_to_storage_expr(self, node_expr: str, base_expr: str) -> str:
        """
        Turn a public value into a storage value. See documentation for
        has_special_storage.

        :param str node_expr: Expression that yields the node that will own the
            resulting storage value.
        :param base_expr: Expression that yields the public value:
        :return: An expression that yields the storage value.
        """
        return base_expr

    @property
    def has_nullexpr(self) -> bool:
        """
        Return whether this type has a null expression.
        """
        try:
            _ = self.nullexpr
        except NoNullexprError:
            return False
        else:
            return True

    @property
    def nullexpr(self) -> str:
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.
        """
        if self._nullexpr is None:
            raise NoNullexprError(
                f"{self.lkt_name} has no Ada null expression"
            )
        else:
            return self._nullexpr

    @property
    def py_nullexpr(self) -> str:
        """
        Return a string to be used in Python code gen for "null" expressions.

        Must be overridden in subclasses... for which we need the Python null
        expression.
        """
        if self._py_nullexpr is None:
            raise NoNullexprError(
                f"{self.lkt_name} has no Python null expression"
            )
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
            raise NoNullexprError(
                f"{self.lkt_name} has no Java null expression"
            )
        else:
            return self._java_nullexpr

    @property
    def storage_nullexpr(self) -> str:
        """
        Return the nullexpr that is used for fields of this type in structs and
        ASTNodes. See documentation for has_special_storage.
        """
        return self.nullexpr

    def c_type(self, capi: CAPISettings) -> CAPIType:
        """
        Return a CAPIType instance for this type.
        """
        return CAPIType(
            capi, self.c_type_name or self.name, external=self.external
        )

    def c_name(self, capi: CAPISettings, suffix: str) -> str:
        """
        Return a name derived from the name of this type in the C API.

        :param suffix: Lower-case suffix to add to the C API type name.
        """
        return capi.get_name(
            self.c_type(capi).unprefixed_name + names.Name.from_lower(suffix)
        )

    def unify(
        self,
        other: _Self,
        error_location: Location | L.LktNode,
        error_msg: str | None = None,
    ) -> _Self:
        """
        If ``self`` and ``other`` are types that match, return the most general
        type to cover both. Create an error diagnostic if they don't match.

        :param other: Type to unify with ``self``.
        :param error_location: Location for the error diagnostic.
        :param error_msg: Diagnostic message for mismatching types. If None, a
            generic one is used, otherwise, we call ``.format`` on it with the
            ``self`` and ``other`` keys being the names of mismatching types.
        """

        # ASTNodeType instances (and thus entities) always can be unified:
        # just take the most recent common ancestor.
        if isinstance(self, EntityType) and isinstance(other, EntityType):
            bare_self = self.element_type
            assert isinstance(bare_self, ASTNodeType)

            bare_other = other.element_type
            assert isinstance(bare_other, ASTNodeType)

            entity_result: EntityType = ASTNodeType.common_ancestor(
                bare_self, bare_other
            ).entity
            # Even though this block is guarded by the check that self is an
            # EntityType, mypy pretends that entity_result does not matches
            # _Self.
            return entity_result  # type: ignore

        elif isinstance(self, ASTNodeType) and isinstance(other, ASTNodeType):
            # Even though this block is guarded by the check that self is an
            # ASTNodeType mypy pretends that node_result does not matches
            # _Self.
            node_result = ASTNodeType.common_ancestor(self, other)
            return node_result  # type: ignore

        # Otherwise, we require a strict subtyping relation
        check_source_language(
            self.matches(other),
            (error_msg or "Mismatching types: {self} and {other}").format(
                self=self.lkt_name, other=other.lkt_name
            ),
            location=error_location,
        )
        return self

    def matches(self, formal: CompiledType) -> bool:
        """
        Return whether `self` matches `formal`.

        This is mere equality for all types but AST nodes, in which `self` is
        allowed to be a subtype for `formal`.

        :param formal: Type to match. `self` is checked to be a subtype of it.
        """

        if self.is_entity_type and formal.is_entity_type:
            return self.element_type.matches(formal.element_type)
        elif isinstance(self, NodeBuilderType) and isinstance(
            formal, NodeBuilderType
        ):
            return self.node_type.matches(formal.node_type)

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
        return ArrayType(self.context, self)

    @property
    @memoized
    def set(self) -> SetType:
        """
        Create a set type whose element type is `self`.
        """
        return SetType(self.context, self)

    def create_iterator(self, used: bool) -> IteratorType:
        """
        Create, if needed, the iterator type for "self".

        This creates the iterator type if it does not exist yet, and return the
        existing one otherwise.

        :param used: Whether the returned iterator type must be considered as
            used (i.e. required for code generation).
        """
        if self._iterator is None:
            self._iterator = IteratorType(self.context, self)

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
    def is_base_struct_type(self) -> bool:
        return isinstance(self, BaseStructType)

    @property
    def is_struct_type(self) -> bool:
        return isinstance(self, StructType)

    @property
    def is_ast_node(self) -> bool:
        return isinstance(self, ASTNodeType)

    @property
    def is_array(self) -> bool:
        return isinstance(self, ArrayType)

    @property
    def public_requires_boxing(self) -> bool:
        """
        Whether the public type in the Ada API for this requires some boxing to
        be embedded in a record. This is true for all unbounded types (i.e.
        arrays).
        """
        return False

    @property
    def exposed_types(self) -> list[CompiledType]:
        """
        Return the list of types that ``self`` exposes when it is itself
        exposed.
        """
        return []

    def _add_field(self, field: AbstractNodeData) -> None:
        """
        Append a field to this type.

        This also initializes the inheritance information (base/overridings)
        for this new field according to the base type for "self".

        Note: this is automatically called from ``AbstractNodeData.__init__``.

        :param field: Field to append.
        """
        import langkit.expressions as E

        self._fields[field.names.index] = field

        # Invalidate the field lookup cache for this node and all derivations,
        # as this new field can be looked up by derivations.

        def reset_cache(t: CompiledType) -> None:
            t._abstract_node_data_dict_cache = {}
            for dt in t.derivations:
                reset_cache(dt)

        reset_cache(self)

        # Look for the base field, if any: a potential field which has the same
        # name as "field" in the base struct.
        base_dict: dict[str, AbstractNodeData] = (
            self.base.get_abstract_node_data_dict() if self.base else {}
        )
        base_field = base_dict.get(field.indexing_name)

        # Initialize inheritance information, checking basic base/overriding
        # field consistency.
        field._inheritance_computed = True
        if base_field:
            base_field._overridings.append(field)
            field._base = base_field

            check_source_language(
                not base_field.final,
                f"{base_field.qualname} is final, overriding it is illegal",
                location=field.location,
            )

            if isinstance(field, BuiltinField):
                # Language specs are not supposed to define builtin fields,
                # hence the assert instead of a user diagnostic.
                assert isinstance(base_field, BuiltinField)

            elif isinstance(base_field, UserField):
                error(
                    "non-parse fields cannot be overriden",
                    location=field.location,
                )

            elif isinstance(base_field, Field):
                if not isinstance(field, Field):
                    error(
                        "only parse fields can override parse fields",
                        location=field.location,
                    )

                check_source_language(
                    base_field.abstract and not field.abstract,
                    f"{field.qualname} cannot override {base_field.qualname}"
                    " unless the former is a concrete field and the latter is"
                    " an abstract one",
                    location=field.location,
                )

            elif isinstance(base_field, E.PropertyDef):
                if not isinstance(field, E.PropertyDef):
                    error(
                        "only properties can override properties",
                        location=field.location,
                    )
                check_source_language(
                    not field.abstract,
                    "Abstract properties cannot override another property."
                    f" Here, {field.qualname} is abstract and overrides"
                    f" {base_field.qualname}.",
                    location=field.location,
                )

    def get_user_fields(
        self,
        predicate: Callable[[UserField], bool] | None = None,
        include_inherited: bool = True,
    ) -> list[UserField]:
        """
        Return the list of all the user fields `self` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.

        :param include_inherited: If true, include inheritted fields in the
            returned list. Return only fields that were part of the declaration
            of this node otherwise.
        """
        result = self.get_abstract_node_data(
            predicate,  # type: ignore
            include_inherited,
            field_class=UserField,
        )
        return result  # type: ignore

    def get_fields(
        self,
        predicate: Callable[[BaseField], bool] | None = None,
        include_inherited: bool = True,
    ) -> list[BaseField]:
        """
        Return the list of all the fields `self` has, including its parents'.

        :param predicate: Predicate to filter fields if needed.

        :param include_inherited: If true, include inheritted fields in the
            returned list. Return only fields that were part of the declaration
            of this node otherwise.
        """
        result = self.get_abstract_node_data(
            predicate,  # type: ignore
            include_inherited,
            field_class=BaseField,
        )
        return result  # type: ignore

    def get_abstract_node_data(
        self,
        predicate: Callable[[AbstractNodeData], bool] | None = None,
        include_inherited: bool = True,
        field_class: Type[AbstractNodeData] = AbstractNodeData,
    ) -> list[AbstractNodeData]:
        """
        Get all BaseField instances for the class.

        :param predicate: Predicate to filter fields if needed.

        :param include_inherited: If true, include inheritted fields in the
            returned list. Return only fields that were part of the declaration
            of this node otherwise.

        :param field_class: The field class to use to filter fields.
        """
        return sorted(
            filter(
                predicate or (lambda f: True),
                self.get_abstract_node_data_dict(
                    include_inherited, field_class
                ).values(),
            ),
            key=lambda f: f._serial,
        )

    def get_properties(
        self,
        predicate: Callable[[E.PropertyDef], bool] | None = None,
        include_inherited: bool = True,
    ) -> list[E.PropertyDef]:
        """
        Return the list of all the fields `self` has.

        :param predicate: Predicate to filter fields if needed.

        :param include_inherited: If true, include inheritted fields in the
            returned list. Return only fields that were part of the declaration
            of this node otherwise.
        """
        import langkit.expressions as E

        result = self.get_abstract_node_data(
            lambda f: (
                isinstance(f, E.PropertyDef)
                and (predicate is None or predicate(f))
            ),
            include_inherited,
        )
        return result  # type: ignore

    def get_abstract_node_data_dict(
        self,
        include_inherited: bool = True,
        field_class: Type[AbstractNodeData] = AbstractNodeData,
    ) -> dict[str, AbstractNodeData]:
        """
        Get all BaseField instances for the class.

        :param include_inherited: If true, include inheritted fields in the
            returned list. Return only fields that were part of the declaration
            of this node otherwise.

        :param field_class: The field class to use to filter fields.
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
                filter(
                    lambda kv: isinstance(kv[1], field_class),
                    all_fields.items(),
                )
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

    @property
    def is_refcounted(self) -> bool:
        raise RuntimeError(
            "NoCompiledType is not supposed to be used in context where"
            " whether it is refcounted must be known"
        )


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

    def __init__(self, context: CompileCtx):
        super().__init__(
            context,
            name="LogicVar",
            location=Location.builtin,
            nullexpr="null",
            is_ptr=False,
            has_special_storage=True,
            c_type_name="logic_var_type",
        )

    @property
    def storage_type_name(self) -> names.Name:
        return names.Name("Logic_Var_Record")

    @property
    def storage_nullexpr(self) -> str:
        return "Null_Var_Record"

    def extract_from_storage_expr(self, node_expr: str, base_expr: str) -> str:
        del node_expr
        return "{}'Unrestricted_Access".format(base_expr)

    def convert_to_storage_expr(self, node_expr: str, base_expr: str) -> str:
        # This should never be needed, but we need to provide a dummy
        # implementation for the inherited abstract method.
        raise NotImplementedError


class EnvRebindingsType(CompiledType):
    """
    Singleton for the environment rebinding type.
    """

    def __init__(self, context: CompileCtx):
        # Even though env rebindings are "exposed" through the C API (entities
        # are structs, and they have an env rebindings field), they are not
        # meant to be part of any public API: the C API acts only as a common
        # bindings gateway, not a true public API. So keep env rebindings
        # "exposed=False".
        super().__init__(
            context,
            name="EnvRebindings",
            location=Location.builtin,
            fields=lambda t: [
                BuiltinField(
                    owner=t,
                    names=MemberNames.for_struct_field("get_parent", "Parent"),
                    type=self,
                    doc="Return the parent rebindings for ``rebindings``.",
                ),
                BuiltinField(
                    owner=t,
                    names=MemberNames.for_struct_field("old_env"),
                    type=T.LexicalEnv,
                    doc="""
                    Return the lexical environment that is remapped by
                    ``rebindings``.
                    """,
                ),
                BuiltinField(
                    owner=t,
                    names=MemberNames.for_struct_field("new_env"),
                    type=T.LexicalEnv,
                    doc="""
                    Return the lexical environment that ``rebindings`` remaps
                    to.
                    """,
                ),
            ],
            null_allowed=True,
            nullexpr="null",
            c_type_name="env_rebindings_type",
            is_refcounted=False,
            py_nullexpr="None",
            java_nullexpr="PointerWrapper.nullPointer()",
            hashable=True,
        )


class TokenType(CompiledType):
    """
    Singleton for the token data type.
    """

    def __init__(self, context: CompileCtx):
        super().__init__(
            context,
            name="TokenReference",
            location=Location.builtin,
            lkt_name="Token",
            exposed=True,
            is_ptr=False,
            null_allowed=True,
            nullexpr="No_Token",
            is_ada_record=True,
            has_special_storage=True,
            c_type_name="token",
        )

    @property
    def storage_type_name(self) -> names.Name:
        return names.Name("Token_Index")

    @property
    def storage_nullexpr(self) -> str:
        return "No_Token_Index"

    def extract_from_storage_expr(self, node_expr: str, base_expr: str) -> str:
        return "Token ({}, {})".format(node_expr, base_expr)

    def convert_to_storage_expr(self, node_expr: str, base_expr: str) -> str:
        return "Stored_Token ({}, {})".format(node_expr, base_expr)

    @property
    def mypy_type_hint(self) -> str:
        return "Opt[Token]"


class Argument:
    """
    Holder for properties arguments.
    """

    local_var: E.LocalVars.LocalVar
    """
    Local variable instance to refer to this argument from inside its property.

    If not set at ``Argument`` instantiation time, this attribute it set as
    soon as the argument is added to its owning property.
    """

    def __init__(
        self,
        location: Location,
        name: names.Name,
        type: CompiledType,
        is_artificial: bool = False,
        default_value: E.BindableLiteralExpr | None = None,
        local_var: E.LocalVars.LocalVar | None = None,
    ):
        """
        :param name: Argument name.
        :param type: Argument type.
        :param is_artificial: Whether the argument was automatically created by
            Langkit, i.e. the language specification did not mention it.
        :param default_value: If None, there is no default value associated to
            this argument. Otherwise, it is the default value to use when
            generating code for the corresponding property argument.
        :param local_var: For properties only. If provided, local variable that
            expressions in the property body must use to access the dynamic
            variable binding made during the property call. If not provided,
            a local variable is automatically created when adding this argument
            to its owning property.
        """
        self.location = location
        self.name = name
        self.type = type
        self.is_artificial = is_artificial
        self.default_value: E.BindableLiteralExpr | None = default_value
        self.has_local_var = False

        if local_var:
            self.set_local_var(local_var)

    def set_local_var(self, var: E.LocalVars.LocalVar) -> None:
        """
        Initialize the ``local_var`` attribute for this ``Argument`` instance.

        This may not be done at instantiation time as, for convenience when
        creating built-in properties, ``Argument`` objects can be created
        before their owning property.
        """
        assert not self.has_local_var
        self.local_var = var
        self.has_local_var = True

    @property
    def var(self) -> E.VariableExpr:
        """
        Expression to refer to this argument from inside its property.
        """
        return self.local_var.ref_expr

    @property
    def public_type(self) -> CompiledType:
        return self.type.public_type

    @property
    def public_default_value(self) -> E.Expr:
        """
        Assuming this argument has a default value, return the default value to
        use in public APIs, according to the type exposed in public.
        """
        from langkit.expressions import NullExpr

        assert self.default_value is not None

        if not self.type.is_ast_node:
            return self.default_value

        if isinstance(self.default_value, NullExpr):
            return NullExpr(None, self.public_type)
        else:
            assert False, "Unsupported default value"

    @property
    def lkt_name(self) -> str:
        return self.name.lower

    def __repr__(self) -> str:
        return "<Argument {} : {}>".format(self.lkt_name, self.type.lkt_name)


class BaseField(AbstractNodeData):
    """
    Base class for node fields and structure fields.
    """

    concrete: ClassVar[bool] = False
    """
    Field used to prevent instantiation of the class. Concrete descendants
    of BaseField must put that field to True in their definition.
    """

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        location: Location,
        type: CompiledType,
        repr: bool = True,
        doc: str = "",
        default_value: E.BindableLiteralExpr | None = None,
        abstract: bool = False,
        access_needs_incref: bool = False,
        null: bool = False,
        nullable: bool | None = None,
        implements: Callable[[], InterfaceMethodProfile] | None = None,
    ):
        """
        Create an AST node field.

        :param owner: Compiled type that owns this member.
        :param names: Names for this member.
        :param location: Source location of the declaration for this member.
        :param type: Type for this field.
        :param repr: If true, the field will be displayed when
            pretty-printing the embedding AST node.
        :param doc: User documentation for this field.
        :param default_value: Default value for this field, when omitted from
            New expressions.
        :param abstract: Whether this member is abstract.
        :param access_needs_incref: See AbstractNodeData's constructor.
        :param null: Whether this field is always supposed to be null.
        :param nullable: None if the language spec does not defines whether
            this field can be null in the absence of parsing error (i.e. when
            it is inferred). True/False otherwise (whether it can be null).
        :param implements: If provided, callback that returns the generic
            interface method that this member implements.
        """

        assert self.concrete, "BaseField itself cannot be instantiated"

        super().__init__(
            owner=owner,
            names=names,
            location=location,
            type=type,
            public=True,
            default_value=default_value,
            abstract=abstract,
            access_needs_incref=access_needs_incref,
            implements=implements,
        )

        self.repr = repr
        self._doc = doc

        self.should_emit = True
        """
        Subclasses can change that variable to trigger wether the field
        should be emitted or not.
        """

        self._null = null

        self.nullable_from_spec = nullable
        """
        Whether this field has the ``@nullable`` annotation.
        """

        self._nullable = self.nullable_from_spec
        """
        Whether we consider this field as nullable. This is determined during
        the "compute field nullability" pass.
        """

        self._synthetized = False
        """
        Whether this field is initialized by node synthetization in properties.
        """

    def __repr__(self) -> str:
        return "<ASTNode {} Field({})>".format(self._serial, self.qualname)

    @property
    def doc(self) -> str:
        return self._doc

    @property
    def null(self) -> bool:
        """
        Return whether this field is always supposed to be null.
        """
        return self._null

    @property
    def nullable(self) -> bool:
        """
        Whether this field can be null in the absence of parsing error.
        """
        assert self._nullable is not None
        return self._nullable

    @property
    def synthetized(self) -> bool:
        """
        Whether this field is initialized by node synthetization in properties.
        """
        return self._synthetized

    def set_synthetized(self) -> None:
        """
        Tag this field and its ancestors as being initialized by node
        synthetization in properties.
        """
        field: BaseField | None = self
        while field is not None:
            field._synthetized = True
            field = field.base


class Field(BaseField):
    """
    Fields that are meant to store parsing results. Can be used only on
    subclasses of ASTNodeType.
    """

    concrete = True
    kind_name = "field"

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        location: Location,
        type: CompiledType,
        repr: bool = True,
        doc: str = "",
        abstract: bool = False,
        null: bool = False,
        nullable: bool | None = None,
        implements: Callable[[], InterfaceMethodProfile] | None = None,
    ):
        assert not abstract or not null
        self._null = null
        super().__init__(
            owner=owner,
            names=names,
            location=location,
            type=type,
            abstract=abstract,
            repr=repr,
            doc=doc,
            null=null,
            nullable=nullable,
            implements=implements,
        )

        self.parsers_from_transform: list[Parser] = []
        """
        List of parsers that provide a value for this field. Such parsers are
        children of Transform parsers.
        """

        self.types_from_synthesis: TypeSet[ASTNodeType] = TypeSet()
        """
        Set of types coming from node synthetization in properties.
        """

        self._precise_types: TypeSet[ASTNodeType] | None = None
        """
        Cache for the precise_types property.
        """

        self._precise_element_types: TypeSet[ASTNodeType] | None = None
        """
        Cache for the precise_element_types property.
        """

        self._index: int | None = None
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

    def _compute_precise_types(self) -> None:
        from langkit.parsers import Null

        etypes: TypeSet
        is_list = self.type.is_list_type

        assert isinstance(self.owner, ASTNodeType)
        assert isinstance(self.type, ASTNodeType)

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
            for f in self.concrete_overridings:
                f._compute_precise_types()
                types.update(f.precise_types)
                if is_list:
                    etypes.update(f.precise_element_types)

        elif all(n.synthetic for n in self.owner.concrete_subclasses):
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
                # Keep track of whether at least one parser can create a
                # non-null node for this field. Note that Null parser create a
                # non-null node in two cases: for list nodes and "qualifier"
                # nodes.
                if not isinstance(p, Null) or p.for_bool_node:
                    all_null = False
                types.update(p.precise_types)
                if is_list:
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
                    check_source_language(
                        not all_null,
                        "According to the grammar, this field always contain"
                        " null nodes: please tag it a null field",
                        location=self.location,
                    )

            # Due to inheritance, even fields of regular nodes can appear in a
            # synthetic node, so take types from node synthesis into account.
            types.update(self.types_from_synthesis)
            if is_list:
                for t in self.types_from_synthesis.matched_types:
                    if t.is_list_type:
                        etypes.include(t.element_type)

        self._precise_types = types
        self._precise_element_types = etypes if is_list else None

    @property
    def doc(self) -> str:
        # Only nodes can have parse fields
        assert isinstance(self.owner, ASTNodeType)

        result = super().doc

        # If parsers build this field, add a precise list of types it can
        # contain: the field type might be too generic.  Note that synthetic
        # nodes are not built by parsers, so for now we don't have precise type
        # information for them.
        precise_types_added = False
        precise_types = list(self.precise_types.minimal_matched_types)

        # If the field always contains a list, and that we know it holds one or
        # several types more precise that its element type, give precise types
        # for the list items.
        if len(precise_types) == 1 and self.type.is_list_type:
            precise_element_types = list(
                self.precise_element_types.minimal_matched_types
            )
            if (
                len(precise_element_types) > 1
                or precise_element_types[0] != self.type.element_type
            ):
                result = append_paragraph(
                    result,
                    "This field contains a list that itself contains one of "
                    "the following nodes: "
                    f"{type_ref_list_doc(precise_element_types)}",
                )
                precise_types_added = True

        if not precise_types_added and (
            len(precise_types) > 1 or precise_types[0] != self.type
        ):
            result = append_paragraph(
                result,
                "This field can contain one of the following nodes: "
                f"{type_ref_list_doc(precise_types)}",
            )
            precise_types_added = True

        result = append_paragraph(
            result,
            (
                "This field may be null even when there are no parsing errors."
                if self.nullable
                else (
                    "When there are no parsing errors, this field is never"
                    " null."
                )
            ),
        )

        return result

    @property
    def index(self) -> int:
        """
        Return the 0-based index of this parsing field in the owning AST node's
        children list. Only non-null concrete fields have an index.
        """
        assert self._index is not None, "Index for {} is not computed".format(
            self.qualname
        )
        assert (
            self._index != -1
        ), "Trying to get index of abstract/null field {}".format(
            self.qualname
        )
        return self._index


class UserField(BaseField):
    """
    Fields that are not meant to store parsing results. Can be used on any
    Node type, will be ignored by the parsing code.
    """

    is_user_field = True
    concrete = True

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        location: Location,
        type: CompiledType,
        repr: bool = False,
        doc: str = "",
        public: bool = True,
        default_value: E.BindableLiteralExpr | None = None,
        access_needs_incref: bool = True,
        implements: Callable[[], InterfaceMethodProfile] | None = None,
    ):
        """
        See inherited doc. In this version we just ensure that a type is
        passed because it is mandatory for data fields. We also set repr to
        False because most of the time you don't want User fields to show up in
        the pretty printer.

        :param names: Names for this member.
        :param is_public: Whether this field is public in the generated APIs.
        :param access_needs_incref: See AbstractNodeData's constructor.
        :param default_value: Default value for this field, when omitted from
            New expressions.
        :param implements: If provided, callback that returns the generic
            interface method that this member implements.
        """
        super().__init__(
            owner,
            names,
            location,
            type,
            repr,
            doc,
            default_value=default_value,
            access_needs_incref=access_needs_incref,
            nullable=True,
            implements=implements,
        )
        self._is_public = public

    @classmethod
    def for_struct(
        cls,
        owner: StructType,
        name: str,
        location: Location,
        type: CompiledType,
        doc: str = "",
        default_value: E.BindableLiteralExpr | None = None,
        implements: Callable[[], InterfaceMethodProfile] | None = None,
    ) -> UserField:
        """
        Create a struct field.

        :param name: Lower-case name for this field, as declared in the
            language specification.
        :param location: Source location for this field.
        :param type: Type for this field.
        :param doc: Documentation for this field, or empty string if not
            documented.
        :param default_value: If this field has a default initialization,
            expression for the default initialization value.
        :param implements: If provided, callback that returns the generic
            interface method that this member implements.
        """
        return cls(
            owner=owner,
            names=MemberNames.for_struct_field(name),
            location=location,
            type=type,
            doc=doc,
            default_value=default_value,
            implements=implements,
        )


class MetadataField(UserField):
    """
    Field for metadata struct. Can be used only there, and requires the user to
    specify whether the field shall be taken into account when processing
    identity related properties.
    """

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        location: Location,
        type: CompiledType,
        use_in_equality: bool,
        repr: bool = False,
        doc: str = "",
        public: bool = True,
        default_value: E.BindableLiteralExpr | None = None,
        access_needs_incref: bool = True,
    ):
        self.use_in_equality = use_in_equality
        super().__init__(
            owner,
            names,
            location,
            type,
            repr,
            doc,
            public,
            default_value,
            access_needs_incref,
        )


class BuiltinField(UserField):
    """
    A built-in field is just like a UserField, except that its name has no
    prefix. It is typically used for fields of built-in structs.
    """

    def __init__(
        self,
        owner: CompiledType,
        names: MemberNames,
        type: CompiledType,
        repr: bool = False,
        doc: str = "",
        public: bool = True,
        default_value: E.BindableLiteralExpr | None = None,
        access_needs_incref: bool = True,
    ):
        super().__init__(
            owner=owner,
            names=names,
            location=Location.builtin,
            type=type,
            repr=repr,
            doc=doc,
            public=public,
            default_value=default_value,
            access_needs_incref=access_needs_incref,
        )
        self.should_emit = False


class BaseStructType(CompiledType):
    """
    Base class to share common behavior between StructType and ASTNodeType.
    """

    def __init__(
        self,
        context: CompileCtx,
        name: names.Name,
        location: Location,
        doc: str = "",
        base: _Self | None = None,
        fields: (
            Callable[[BaseStructType], Sequence[AbstractNodeData]] | None
        ) = None,
        implements: Callable[[], Sequence[GenericInterface]] | None = None,
        is_ptr: bool = True,
        is_list_type: bool = False,
        is_entity_type: bool = False,
        exposed: bool = False,
        c_type_name: str | None = None,
        external: bool = False,
        null_allowed: bool = False,
        is_ada_record: bool = False,
        is_refcounted: bool = False,
        nullexpr: str | None = None,
        py_nullexpr: str | None = None,
        java_nullexpr: str | None = None,
        element_type: CompiledType | None = None,
        hashable: bool = False,
        has_equivalent_function: bool = False,
        type_repo_name: str | None = None,
        api_name: str | names.Name | None = None,
        lkt_name: str | None = None,
        conversion_requires_context: bool = False,
    ):
        """
        See CompiledType.__init__ for a description of the other arguments.

        :param implements: If provided, callback that returns the list of
            generic interfaces that this node/struct implements.
        """
        self.implements: list[GenericInterface] = []
        if implements:
            context.deferred.implemented_interfaces.add(self, implements)

        if is_keyword(name):
            name = name + names.Name("Node")

        super().__init__(
            context,
            name,
            location,
            doc,
            base,
            fields=fields_callback_wrapper(BaseStructType, fields),
            is_ptr=is_ptr,
            is_list_type=is_list_type,
            is_entity_type=is_entity_type,
            exposed=exposed,
            c_type_name=c_type_name,
            external=external,
            null_allowed=null_allowed,
            is_ada_record=is_ada_record,
            is_refcounted=is_refcounted,
            nullexpr=nullexpr,
            py_nullexpr=py_nullexpr,
            java_nullexpr=java_nullexpr,
            element_type=element_type,
            hashable=hashable,
            has_equivalent_function=has_equivalent_function,
            type_repo_name=type_repo_name or name.camel,
            api_name=api_name,
            lkt_name=lkt_name,
            conversion_requires_context=conversion_requires_context,
        )

    @property
    def py_nullexpr(self) -> str:
        return self._py_nullexpr or "{}({})".format(
            self.name.camel,
            ", ".join(f.type.py_nullexpr for f in self.get_fields()),
        )

    @property
    def required_fields_in_exprs(self) -> dict[str, BaseField]:
        """
        Return all fields that must be involved in the property DSL.

        This returns a mapping from DSL field names to fields for all fields
        that must be considered when building structs in the property DSL.
        """

        def is_required(f: AbstractNodeData) -> bool:
            if f.is_internal:
                # If this field does not come from sources (it's
                # automatic/internal), and thus users are not supposed to
                # access/set it.
                return False

            elif isinstance(f, BuiltinField):
                # BuiltinFields are actually stored fields only for structure
                # types (not for nodes).
                return self.is_struct_type

            elif isinstance(f, Field):
                return not f.null

            else:
                return isinstance(f, UserField)

        return {
            f.original_name: f
            for f in self.get_abstract_node_data()
            if isinstance(f, BaseField) and is_required(f)
        }

    def add_internal_user_field(
        self,
        name: names.Name,
        type: CompiledType,
        default_value: E.BindableLiteralExpr | None,
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
        return UserField(
            owner=self,
            names=MemberNames.for_internal(self.context, name.lower),
            location=Location.builtin,
            type=type,
            doc=doc,
            public=False,
            default_value=default_value,
        )

    def implemented_interfaces(
        self, include_parents: bool = True
    ) -> list[GenericInterface]:
        """
        Return the interfaces implemented by this node, including its parents'
        if ``include_parents`` is true.
        """
        result = []
        t: BaseStructType | None = self
        while t is not None:
            result.extend(t.implements)
            t = t.base if include_parents else None
        return result


class StructType(BaseStructType):
    """
    POD composite type.
    """

    def __init__(
        self,
        context: CompileCtx,
        name: names.Name,
        location: Location,
        doc: str = "",
        fields: (
            Callable[[StructType], Sequence[AbstractNodeData]] | None
        ) = None,
        implements: Callable[[], Sequence[GenericInterface]] | None = None,
        is_list_type: bool = False,
        is_entity_type: bool = False,
        c_type_name: str | None = None,
        external: bool = False,
        py_nullexpr: str | None = None,
        java_nullexpr: str | None = None,
        element_type: CompiledType | None = None,
        has_equivalent_function: bool = False,
        type_repo_name: str | None = None,
        conversion_requires_context: bool = False,
    ):
        """
        :param name: See CompiledType.__init__.

        :param implements: If provided, callback that returns the list of
            generic interfaces that this struct implements.
        """
        internal_name = names.Name("Internal") + name
        super().__init__(
            context,
            internal_name,
            location,
            doc,
            fields=fields_callback_wrapper(StructType, fields),
            implements=implements,
            is_ptr=False,
            is_list_type=is_list_type,
            is_entity_type=is_entity_type,
            exposed=False,
            c_type_name=c_type_name,
            external=external,
            null_allowed=True,
            is_ada_record=True,
            nullexpr=(names.Name("No") + name).camel_with_underscores,
            py_nullexpr=py_nullexpr,
            java_nullexpr=java_nullexpr,
            hashable=True,
            has_equivalent_function=has_equivalent_function,
            type_repo_name=name.camel,
            api_name=name,
            lkt_name=name.camel,
            conversion_requires_context=conversion_requires_context,
        )
        context.add_pending_composite_type(self)

    @property
    def conversion_requires_context(self) -> bool:
        return any(
            f.type.conversion_requires_context for f in self.get_fields()
        )

    def add_as_memoization_key(self, context: CompileCtx) -> None:
        super().add_as_memoization_key(context)
        for f in self.get_fields():
            f.type.add_as_memoization_key(context)

    @property
    def has_equivalent_function(self) -> bool:
        return any(f.type.has_equivalent_function for f in self.get_fields())

    def require_hash_function(self) -> None:
        super().require_hash_function()
        for f in self.get_fields():
            f.type.require_hash_function()

    @property
    def is_refcounted(self) -> bool:
        return any(f.type.is_refcounted for f in self._fields.values())

    @property
    def is_empty(self) -> bool:
        return len(self.get_fields()) == 0

    @property
    def has_early_decl(self) -> bool:
        # The env metadata and inner env assoc structs are emitted separately
        # as they need to be fully declared before the
        # Langkit_Support.Lexical_Env generic package instantiation, while
        # regular structs are declared after that instantiation.
        return self in (T.env_md, T.InnerEnvAssoc)

    @property
    def is_predeclared(self) -> bool:
        """
        Whether this struct type is to be considered as pre-declared in
        templates for the internal Ada API.

        Note that except for the type declaration itself ("type ... is") and
        the "nullexpr" constant, declarations for this type must still be
        emitted (Hash function, ...).
        """
        # It's the Langkit_Support.Lexical_Env generic package instantiation
        # that declares entity, entity info and inner env assoc structs.
        return self in (T.EntityInfo, T.entity)

    def c_inc_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to inc-ref structure value.

        :param capi: Settings for the C API.
        """
        return capi.get_name(self.name + names.Name("Inc_Ref"))

    def c_dec_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to dec-ref structure value.

        :param capi: Settings for the C API.
        """
        return capi.get_name(self.name + names.Name("Dec_Ref"))

    @property
    def emit_c_type(self) -> bool:
        """
        Return whether to emit a C type for this type.

        This is used to filter out all entity types except the root one. All
        entity types are compatible from an ABI point of view, so this reduces
        the amount of code emitted for them.
        """
        return not self.is_entity_type or self == T.entity

    @property
    def public_record_type(self) -> names.Name:
        """
        Name of the Ada record type used to expose this struct in the public
        API.
        """
        return self.name + names.Name("Record")

    @property
    def contains_boxed_fields(self) -> bool:
        """
        Return if at least one field requires boxing in the public API.
        """
        return any(f.type.public_requires_boxing for f in self.get_fields())

    @property
    def to_public_converter(self) -> names.Name:
        return names.Name("To_Public") + self.api_name

    @property
    def to_internal_converter(self) -> names.Name:
        return names.Name("To_Internal") + self.api_name

    @property
    def exposed_types(self) -> list[CompiledType]:
        # Entity types are exposed as opaque types, so don't expose their
        # internals.
        return (
            [] if self.is_entity_type else [f.type for f in self.get_fields()]
        )


class EntityType(StructType):
    """
    Subclass of StructType dedicated to entity types.
    """

    def __init__(self, context: CompileCtx, astnode: ASTNodeType):
        self.astnode = astnode

        name = names.Name("Entity")
        if not self.astnode.is_root_node:
            name += self.astnode.kwless_raw_name

        super().__init__(
            context,
            name=name,
            location=astnode.location,
            doc="",
            fields=lambda t: [
                BuiltinField(
                    owner=t,
                    names=MemberNames.for_struct_field("node"),
                    type=self.astnode,
                    doc="The stored AST node",
                ),
                BuiltinField(
                    owner=t,
                    names=MemberNames.for_struct_field("info"),
                    type=T.EntityInfo,
                    access_needs_incref=True,
                    doc="Entity info for this node",
                ),
            ],
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
    def element_type(self) -> ASTNodeType:
        result = super().element_type
        assert isinstance(result, ASTNodeType)
        return result

    @property
    def lkt_name(self) -> str:
        return f"Entity[{self.element_type.lkt_name}]"

    def c_type(self, capi: CAPISettings) -> CAPIType:
        # Emit only one C binding type for entities. They are all ABI
        # compatible, so this reduces the amount of types emitted.
        return CAPIType(capi, "node")

    @property
    def is_root_type(self) -> bool:
        """
        Return whether this entity type correspond to the AST node root type.
        """
        return self.astnode.is_root_node

    @property
    def base(self) -> EntityType | None:
        """
        Return the entity type that `self` overrides, or None for the root.
        """
        base = self.astnode.base
        return None if base is None else base.entity

    @property
    def api_name(self) -> names.Name:
        """
        Type name to use in the public API.
        """
        return self.astnode.kwless_raw_name

    @property
    def constructor_name(self) -> names.Name:
        """
        Name of the internal Ada functions to instantiate this array.
        """
        return names.Name("Create") + self.name

    def to_public_expr(self, internal_expr: str) -> str:
        # Wrap the bare node into a public entity
        result = "Wrap_Node ({}.Node, {}.Info)".format(
            internal_expr, internal_expr
        )

        # If needed, convert the result expression to the appropriate public
        # entity derivation.
        if not self.element_type.is_root_node:
            result += ".As_{}".format(self.api_name)

        return result

    def to_internal_expr(
        self,
        public_expr: str,
        context: str | None = None,
    ) -> str:
        return "({internal_node}, {public_entity}.Internal.Info)".format(
            internal_node=self.element_type.to_internal_expr(public_expr),
            public_entity=public_expr,
        )


class Annotations:

    node: ASTNodeType
    """
    Node that owns these annotations.
    """

    def __init__(
        self,
        repr_name: str | None = None,
        generic_list_type: str | None = None,
        rebindable: bool = False,
        custom_short_image: bool = False,
        snaps: bool = False,
        ple_unit_root: bool = False,
    ):
        """
        Constructor for a node's annotations.

        :param repr_name: The name to be used in repr for this node type.
        :param generic_list_type: The name of the generic list type.
        :param rebindable: Whether lexical environments that belong to this
            kind of node can be rebound.
        :param custom_short_image: Whether this node must use a custom
            ``Short_Text_Image`` implementation. If true, extensions must add
            the declaration and the definition of a function called
            ``[NODE_NAME]_Short_Text_Image`` that takes the node in argument
            and that returns a ``Text_Type`` value.
        :param snaps: Whether this node's SLOCs are supposed to snap or not.
            Snapping designates the behavior where the start SLOC will be
            anchored to the previous token's end SLOC rather than the node's
            first token start SLOC, and conversely for the end SLOC.
        :param ple_unit_root: Tag this node as the root for sub-analysis units.
            At most one node can be tagged this way, which implies that other
            nodes cannot derive from it. In the grammar, PLE unit roots can
            only appear as a child of a list node, which must be the root node.
        """
        self.repr_name = repr_name
        self.generic_list_type = generic_list_type
        self.rebindable = rebindable
        self.custom_short_image = custom_short_image
        self.snaps = snaps
        self.ple_unit_root = ple_unit_root


class ASTNodeType(BaseStructType):
    """
    Type for an AST node.
    """

    kwless_raw_name: names.Name

    # If this is an enum onde, list of descriptions for its enum alternatives
    _alternatives: list[ASTNodeType]

    # If this is an enum node, mapping from alternative camel name to
    # ASTNodeType instance for the alternative.
    _alternatives_map: dict[str, ASTNodeType]

    def __init__(
        self,
        context: CompileCtx,
        name: names.Name,
        location: Location,
        doc: str,
        base: ASTNodeType | None,
        env_spec: EnvSpec | None = None,
        element_type: ASTNodeType | None = None,
        annotations: Annotations | None = None,
        is_generic_list_type: bool = False,
        is_abstract: bool = False,
        is_synthetic: bool = False,
        with_abstract_list: bool = False,
        implements: Callable[[], Sequence[GenericInterface]] | None = None,
        is_enum_node: bool = False,
        is_bool_node: bool = False,
        is_token_node: bool = False,
        is_error_node: bool = False,
        lkt_name: str | None = None,
    ):
        """
        :param context: Compilation context that owns this type.

        :param name: Name for this node.

        :param location: Location of the declaration of this node.

        :param doc: User documentation for this node.

        :param base: ASTNodeType subclass corresponding to the base class for
            this node. None when creating the root node.

        :param env_spec: Environment specification for this node, if any.

        :param element_type: For root list types, this must be the ASTNodeType
            subclass that this list contains. Must be left to None in all other
            cases.

        :param is_generic_list_type: Whether this subclass will materialize the
            generic list type.

        :param is_abstract: Whether this node is abstract. Note that this can
            be changed later. This is forced to True for the generic list type
            and for root list types whose element type has the
            `with_abstract_list` attribute set to True.

        :param is_synthetic: Whether this node is synthetic. Note that this can
            be changed later.

        :param with_abstract_list: Whether the root list type for this node
            must be abstract. Node that this can be changed later, until the
            list type is actually created.

        :param implements: If provided, callback that returns the list of
            generic interfaces that this node implements.

        :param is_enum_node: Whether this node comes from the expansion of an
            enum node.

        :param is_bool_node: Whether this node is a qualifier coming from the
            expansion of an enum node.

        :param is_token_node: Whether this node only materializes a parsed
            token. If so, grammars that produce such nodes must parse only one
            token (.token_start must be equal to .token_end).

        :param is_error_node: Whether this node only materializes a parsing
            error. If so, only Skip parsers can create this node.

        :param lkt_name: Name used to represent this type in Lkt sources.
            Useful to format diagnostics.
        """
        self.raw_name = name
        self.kwless_raw_name = (
            self.raw_name + names.Name("Node")
            if is_keyword(self.raw_name)
            else self.raw_name
        )

        name = names.Name("Bare") + self.kwless_raw_name

        is_root = base is None
        is_root_list = base is not None and base.is_generic_list_type
        is_list = base is not None and (is_root_list or base.is_list_type)

        if base is None:
            self.null_constant = names.Name("No") + name
        else:
            self.null_constant = context.root_node_type.null_constant

        if is_root_list:
            assert element_type is not None and element_type.is_ast_node

            # TODO: at this point, we need to make sure thas
            # element_type.with_abstract_list in the future.
            is_abstract = is_abstract or element_type.with_abstract_list

        else:
            assert element_type is None
            if is_list:
                assert base is not None
                assert isinstance(base._element_type, ASTNodeType)
                element_type = base._element_type

        if is_root_list:
            assert element_type
            doc = doc or "List of {}.".format(element_type.lkt_name)

        super().__init__(
            context,
            name,
            location,
            doc=doc,
            base=base,
            fields=self.builtin_properties if is_root else None,
            implements=implements,
            is_ptr=True,
            null_allowed=True,
            is_ada_record=False,
            is_list_type=is_list,
            # Even though bare node types are not exposed, we allow them in
            # public APIs and will (un)wrap them as entities automatically.
            exposed=True,
            is_refcounted=False,
            nullexpr=self.null_constant.camel_with_underscores,
            py_nullexpr="None",
            java_nullexpr="null",
            element_type=element_type,
            hashable=True,
            type_repo_name=self.raw_name.camel,
            lkt_name=lkt_name or self.raw_name.camel,
        )
        self._is_root_node = is_root
        self.is_generic_list_type: bool = is_generic_list_type
        self.is_root_list_type = is_root_list
        self.is_list = is_list

        # Register this new node type to the compile context
        if is_root:
            self.context.root_node_type = self
            self.context.has_root_node_type = True

            self.value_type_name = "Root_Node_Record"
            """
            Name of the Ada type for the record that contains data for all
            nodes.
            """
        context.add_pending_node_type(self)

        annotations = annotations or Annotations()
        self.annotations: Annotations = annotations
        annotations.node = self

        self.env_spec: EnvSpec | None = env_spec
        """
        EnvSpec instance corresponding to this node.
        """

        # By default, ASTNodeType subtypes aren't abstract. The "abstract"
        # decorator may change this attribute later. Likewise for synthetic
        # nodes and nodes whose root list type is abstract.
        self.abstract: bool = is_abstract or is_root or is_generic_list_type
        self.synthetic: bool = is_synthetic

        self.with_abstract_list: bool = with_abstract_list
        """
        Whether the automatically generated list type for this ASTNodeType (the
        "root list type") is abstract.
        """

        # Prepare the list of subclasses for this node type and, if applicable,
        # register it as a subclass of its base.
        self.subclasses: list[ASTNodeType] = []
        """
        List of subclasses. Overriden in the root grammar class and its
        children. This list is completed as ASTNodeType instances are created,
        and sorted in the "compute_types" compilation pass.
        """

        if base is not None:
            base.subclasses.append(self)

        # If this is the root grammar type, create the generic list type name
        self.generic_list_type: ASTNodeType
        """
        Root grammar class subclass. It is abstract, generated automatically
        when the root grammar class is known. All root list types subclass it.
        """

        if base is None:
            generic_list_type_name = (
                names.Name.from_camel(annotations.generic_list_type)
                if annotations.generic_list_type
                else (self.kwless_raw_name + names.Name("Base_List"))
            )

            self.generic_list_type = ASTNodeType(
                self.context,
                name=generic_list_type_name,
                location=location,
                doc="",
                base=self,
                is_generic_list_type=True,
                is_abstract=True,
            )
        elif is_generic_list_type:
            self.generic_list_type = self
        else:
            self.generic_list_type = base.generic_list_type

        self.transform_parsers: list[_Transform] = []
        """
        List of Transform parsers that produce this node.
        """

        self.list_element_parsers: list[Parser] | None = (
            [] if is_list else None
        )
        """
        For list nodes, list of parsers that produce list elements. None for
        all other nodes.
        """

        self.precise_list_element_types: TypeSet | None = None
        """
        For list nodes, precise set of types that this list can contain.
        """

        self.unparser: NodeUnparser | None = None
        """
        Unparser for this node. Computed during the NodesToParsers pass.
        """

        self.is_enum_node = is_enum_node
        self.is_bool_node = is_bool_node
        self.is_token_node = is_token_node
        self.is_error_node = is_error_node

        self.token_kind: TokenAction | None = None
        """
        If this is a token node and if unparser generation is enabled, this
        must reference the only token kind that this node can be associated to.
        Must be None otherwise.
        """

        # Make sure we have one entity type for each AST node type
        entity_type = self.entity
        del entity_type

    @property
    def effective_env_spec(self) -> EnvSpec | None:
        """
        Return the env spec, for this node, whether it's defined on this node
        or inherited from a parent node.
        """
        return (
            self.env_spec
            if self.env_spec
            else (self.base.effective_env_spec if self.base else None)
        )

    @property
    def doc(self) -> str:
        result = super().doc

        extra_paragraphs = []

        # If this is a list node and that parsers build it, add a precise list
        # of types it can contain: the element type might be too generic.
        if self.is_list and not self.synthetic:
            assert self.precise_list_element_types is not None
            precise_types = list(
                self.precise_list_element_types.minimal_matched_types
            )
            if len(precise_types) > 1 or precise_types[0] != self.element_type:
                extra_paragraphs.append(
                    "This list node can contain one of the following"
                    f" nodes: {type_ref_list_doc(precise_types)}"
                )

        if self.subclasses:
            extra_paragraphs.append(
                f"Derived nodes: {type_ref_list_doc(self.subclasses)}"
            )
        else:
            extra_paragraphs.append("This node type has no derivation.")

        base_indent = first_line_indentation(result)
        for text in extra_paragraphs:
            addition = indent(text, base_indent)
            result = append_paragraph(result, addition)

        return result

    def repr_name(self) -> str:
        """
        Return a name that will be used when serializing this AST node.
        """
        # This name is used by pretty printers-like code: we need the
        # "original" node name here, not keyword-escaped ones.
        result = self.annotations.repr_name or self.kwless_raw_name.camel
        return result

    @property
    def public_type(self) -> CompiledType:
        return self.entity

    def add_transform(self, parser: _Transform) -> None:
        """
        Register ``parser`` as a Transform parser that creates this node.

        This also registers sub-parsers in node fields and keep track of field
        types, checking for consistencies.

        :param parser: Transform parser to register.
        """
        self.transform_parsers.append(parser)

    def add_list_element_parser(self, parser: Parser) -> None:
        """
        Register ``parser`` as a parser that creates list elements.

        :param parser: Parser to register.
        """
        assert self.list_element_parsers is not None
        self.list_element_parsers.append(parser)

    def check_inferred_field_types(self) -> None:
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
                assert f_type is not None
                check_source_language(
                    f_type.matches(field.type),
                    "Field {} already had type {}, got {}".format(
                        field.qualname, field.type.lkt_name, f_type.lkt_name
                    ),
                    location=parser.location,
                )

    def compute_precise_fields_types(self) -> None:
        if self.is_list:
            # Do not compute precise types twice
            if self.precise_list_element_types:
                return

            # A list node can contain nodes coming from its own parsers, but
            # also from subclasses' parsers.
            types: TypeSet[ASTNodeType] = TypeSet()
            self.precise_list_element_types = types
            for subcls in self.subclasses:
                subcls.compute_precise_fields_types()
                assert subcls.precise_list_element_types is not None
                types.update(subcls.precise_list_element_types)
            assert self.list_element_parsers is not None
            for p in self.list_element_parsers:
                types.update(p.precise_types)

        else:
            for f in self.get_parse_fields(include_inherited=False):
                f._compute_precise_types()

    def warn_imprecise_field_type_annotations(self) -> None:
        for field in self.get_parse_fields():

            # We want to compare the type annotation to the type that was
            # inferred from the grammar. There is more to do than just
            # comparing the two types since there can be several types that
            # specify the same number of concrete types: think of an abstract
            # type that is subclassed only once. So use type sets to do the
            # comparison, instead.
            common_inferred = field.precise_types.minimal_common_type
            assert isinstance(common_inferred, ASTNodeType)
            inferred_types = TypeSet([common_inferred])
            assert isinstance(field.type, ASTNodeType)
            field_types = TypeSet([field.type])

            WarningSet.imprecise_field_type_annotations.warn_if(
                inferred_types != field_types,
                "Specified type is {}, but it could be more specific:"
                " {}".format(field.type.lkt_name, common_inferred.lkt_name),
                location=field.location,
            )

    @staticmethod
    def common_ancestor(*nodes: ASTNodeType) -> ASTNodeType:
        """
        Return the bottom-most common parent AST node for all `nodes` AST
        nodes.

        :param nodes: List of AST node types for which we are looking for the
            common ancestor.
        """
        return list(
            takewhile(
                lambda a: len(set(a)) == 1,
                zip(*map(lambda n: n.get_inheritance_chain(), nodes)),
            )
        )[-1][0]

    @property
    def base(self) -> ASTNodeType | None:
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
    def type_set(self) -> list[ASTNodeType]:
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
    def ada_kind_range_bounds(self) -> tuple[str, str]:
        """
        Return the name of the Ada enumerators for the range bounds (first and
        last) of kinds that nodes of this type can have.
        """
        subclasses = self.concrete_subclasses
        return (
            (subclasses[0].ada_kind_name, subclasses[-1].ada_kind_name)
            if subclasses
            else (self.ada_kind_name, self.ada_kind_name)
        )

    def get_parse_fields(
        self,
        predicate: Callable[[Field], bool] | None = None,
        include_inherited: bool = True,
    ) -> list[Field]:
        """
        Return the list of all the parse fields `self` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :param include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.
        """
        result = self.get_abstract_node_data(
            predicate,  # type: ignore
            include_inherited,
            field_class=Field,
        )
        return result  # type: ignore

    def fields_with_accessors(self) -> list[AbstractNodeData]:
        """
        Return a list of fields for which we must generate accessors in APIs.

        This list excludes inherited/overriden fields so that they are not
        generated multiple times. This list also excludes private fields unless
        the context requires them to be public in the generated library.
        """
        return [
            f
            for f in self.get_abstract_node_data(
                include_inherited=False, predicate=lambda f: f.is_public
            )
            if not f.is_overriding
        ]

    def fields_to_initialize(self, include_inherited: bool) -> list[BaseField]:
        """
        Return the list of fields to initialize for this node.

        :param include_inherited: If true, include inheritted fields in the
            returned list. Return only fields that were part of the declaration
            of this node otherwise.
        """
        return self.get_fields(
            include_inherited=include_inherited,
            predicate=lambda f: not f.abstract and not f.null,
        )

    @property  # type: ignore
    @memoized
    def has_fields_initializer(self) -> bool:
        """
        Return whether this node has a kind-specific fields initializer
        procedure.
        """
        if self.is_root_node:
            return False
        else:
            assert self.base is not None
            return (
                bool(self.fields_to_initialize(include_inherited=False))
                or self.base.has_fields_initializer
            )

    def c_type(self, c_api_settings: CAPISettings) -> CAPIType:
        return CAPIType(c_api_settings, "base_node")

    @property  # type: ignore
    @memoized
    def hierarchical_name(self) -> str:
        """
        Return a name that contains all the base classes for this node type.

        For instance, if C derives from B which itself derives from A, this
        returns "A.B.C".
        """
        return ".".join(
            node.name.base_name for node in self.get_inheritance_chain()
        )

    @property
    def ada_kind_name(self) -> str:
        """
        Return the name of the Ada enumerator to represent this kind of node.
        """
        return (
            self.context.config.library.language_name + self.kwless_raw_name
        ).camel_with_underscores

    @property
    def ada_kind_range_name(self) -> str:
        """
        Return the name of the Ada kind subtype to represent the set of node
        kinds that include `self` and all its subclasses.
        """
        if self.abstract:
            return self.ada_kind_name
        else:
            return self.ada_kind_name + "_Range"

    @property
    def equivalent_to_root(self) -> bool:
        """
        Return whether this node type is equivalent to the root type.

        For instance, if the root node has only one subclass, then this
        subclass is equivalent to the root type, as all concrete nodes belong
        to both types.

        This property is useful to avoid emitting tautological kind checks
        during code generation.
        """
        return self.is_root_node or TypeSet({self}) == TypeSet({T.root_node})

    # We want structural equality on lists whose elements have the same types.
    # Memoization is one way to make sure that, for each CompiledType instance
    # X: X.list is X.list.
    @property  # type: ignore
    @memoized
    def list(self) -> ASTNodeType:
        """
        Return an ASTNodeType subclass that represent a list of `self`.
        """
        result = ASTNodeType(
            self.context,
            name=self.kwless_raw_name + names.Name("List"),
            location=Location.nowhere,
            doc="",
            base=self.context.root_node_type.generic_list_type,
            element_type=self,
            lkt_name=f"ASTList[{self.lkt_name}]",
        )
        self.context.list_types.add(self)
        return result

    @property  # type: ignore
    @memoized
    def entity(self) -> EntityType:
        """
        Return the entity type, which is a node type with assorted semantic
        information.
        """
        return EntityType(self.context, self)

    @property
    @memoized
    def builder_type(self) -> NodeBuilderType:
        """
        Return the node builder type corresponding to this node.

        Note that this may raise a language check error if no syntethic node
        matches this node type.
        """
        return NodeBuilderType(self.context, self)

    def create_default_can_reach(self) -> E.PropertyDef:
        """
        Build and return the default ``can_reach`` property.

        This is meant to be used for the default node, when the language
        specification does not define this property.
        """
        import langkit.expressions as E

        from_node_arg = Argument(
            location=Location.builtin,
            name=names.Name("From_Node"),
            type=T.root_node,
        )
        can_reach = E.PropertyDef(
            owner=self,
            names=MemberNames.for_property(self, "can_reach"),
            location=Location.builtin,
            public=False,
            type=T.Bool,
            expr=None,
            arguments=[from_node_arg],
            artificial=True,
        )
        can_reach.location = Location.builtin

        unit_property = self.get_abstract_node_data_dict()["unit"]

        # If there is no from_node node, assume we can access everything. Also
        # assume than from_node can reach Self if both do not belong to the
        # same unit.
        def create_expr() -> E.Expr:
            with can_reach.bind():
                return E.IfExpr(
                    None,
                    E.make_eq_expr(
                        None, from_node_arg.var, E.NullExpr(None, T.root_node)
                    ),
                    E.BooleanLiteralExpr(None, True),
                    E.IfExpr(
                        None,
                        E.make_not_expr(
                            None,
                            E.make_eq_expr(
                                None,
                                E.EvalMemberExpr(
                                    debug_info=None,
                                    receiver_expr=can_reach.node_var.ref_expr,
                                    node_data=unit_property,
                                    arguments=[],
                                ),
                                E.EvalMemberExpr(
                                    debug_info=None,
                                    receiver_expr=from_node_arg.var,
                                    node_data=unit_property,
                                    arguments=[],
                                ),
                            ),
                        ),
                        E.BooleanLiteralExpr(None, True),
                        E.OrderingTestExpr.make_compare_nodes(
                            None,
                            can_reach,
                            E.OrderingTestKind.less_than,
                            can_reach.node_var.ref_expr,
                            from_node_arg.var,
                        ),
                    ),
                )

        self.context.deferred.property_expressions.add(can_reach, create_expr)

        return can_reach

    def create_abstract_as_bool_cb(
        self,
        location: Location,
    ) -> Callable[[], builtin_list[E.PropertyDef]]:
        """
        Callback for deferred type members to build and return the abstract
        ``as_bool`` property for booleanized enum node base types.
        """

        def fields_cb() -> list[E.PropertyDef]:
            import langkit.expressions as E

            prop = E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "as_bool"),
                location=Location.builtin,
                expr=None,
                type=T.Bool,
                abstract=True,
                dynamic_vars=[],
                lazy_field=False,
                public=True,
                doc="Return whether this node is present",
            )
            prop.location = location
            return [prop]

        return fields_cb

    def create_concrete_as_bool_cb(
        self,
        is_present: bool,
        location: Location,
    ) -> Callable[[], builtin_list[E.PropertyDef]]:
        """
        Callback for deferred type members to build and return the abstract
        return the concrete ``as_bool`` property for booleanized enum node
        subclasses.

        :param is_present: Whether the booleanized enum is considered to be
            present in this concrete property.
        """

        def fields_cb() -> list[E.PropertyDef]:
            import langkit.expressions as E

            prop = E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "as_bool"),
                location=Location.builtin,
                type=T.Bool,
                expr=E.BooleanLiteralExpr(None, is_present),
            )
            prop.location = location
            return [prop]

        return fields_cb

    def validate_fields(self) -> None:
        """
        Perform various checks on this ASTNodeType's fields.

        In particular, make sure:
        * the AST node is type resolved (i.e. all fields have a type);
        * all fields are AST nodes themselves;
        * it does not hold homonym parse fields.

        Emit errors when appropriate.
        """
        parse_fields = self.get_parse_fields()

        for f in parse_fields:
            diag_ctx = DiagnosticContext(f.location)

            # Null fields must override an abstract one
            diag_ctx.check_source_language(
                not f.null or f.is_overriding,
                "Null fields can only be used to override abstract fields",
            )

            # All syntax fields must be nodes
            if not isinstance(f.type, ASTNodeType):
                diag_ctx.error(
                    "AST node parse fields must all be AST node"
                    f" themselves. Here, field type is {f.type.lkt_name}",
                )

            # Null fields cannot contain list or qualifier nodes
            if f.null:
                diag_ctx.check_source_language(
                    not f.type.is_list_type,
                    "field that contain list nodes cannot be null",
                )
                diag_ctx.check_source_language(
                    not f.type.is_bool_node,
                    "field that contain qualifier nodes cannot be null",
                )

        # All fields inheritted by "self", i.e. all fields from its base node
        # (if any).
        inherited_fields = (
            self.base.get_abstract_node_data_dict() if self.base else {}
        )

        # Subset of inherited fields that are abstract.
        #
        # Such abstract fields are removed from this dict as soon as we find a
        # concrete overriding field for them, and abstract fields that belong
        # to "self" are added, so that we know at the end what fields are kept
        # abstract in this node.
        abstract_fields = {
            f_n: f_v
            for f_n, f_v in inherited_fields.items()
            if isinstance(f_v, Field) and f_v.abstract
        }

        for f_n, f_v in self._fields.items():
            # If this is an abstract parse field, add it to abstract_fields,
            # and remove any corresponding entry from abstract_fields if it is
            # a concrete parse field.
            if isinstance(f_v, Field):
                if f_v.abstract:
                    abstract_fields[f_n] = f_v
                else:
                    abstract_fields.pop(f_n, None)

                if f_v.base is not None:
                    check_source_language(
                        f_v.type.matches(f_v.base.type),
                        f"Type of overriding field ({f_v.type.lkt_name}) does"
                        " not match type of abstract field"
                        f" ({f_v.base.type.lkt_name})",
                        location=f_v.location,
                    )

        # For concrete nodes, make sure that all abstract fields are overriden
        # by concrete ones.
        check_source_language(
            self.abstract or not abstract_fields,
            "This node is concrete, yet it has abstract fields that are"
            " not overriden: {}".format(
                ", ".join(sorted(f.qualname for f in abstract_fields.values()))
            ),
            location=self.location,
        )

    def builtin_properties(
        self,
        owner: CompiledType,
    ) -> builtin_list[E.PropertyDef]:
        """
        Return properties available for all AST nodes.
        """
        import langkit.expressions as E

        assert owner == self

        # Note that we must not provide implementation for them here (no
        # expression) since the implementation comes from the hard-coded root
        # AST node type definition.
        return [
            # The following fields return lexical env values, which are
            # ref-counted. However these specific envs are owned by the
            # analysis unit, so they are not ref-counted.
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "node_env", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.LexicalEnv,
                public=False,
                external=True,
                uses_entity_info=True,
                uses_envs=True,
                optional_entity_info=True,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="For nodes that introduce a new environment, return the"
                ' parent lexical environment. Return the "inherited"'
                " environment otherwise.",
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "children_env", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.LexicalEnv,
                public=False,
                external=True,
                uses_entity_info=True,
                uses_envs=True,
                optional_entity_info=True,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="For nodes that introduce a new environment, return it."
                ' Return the "inherited" environment otherwise.',
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "parent", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.entity,
                public=True,
                external=True,
                uses_entity_info=True,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="Return the syntactic parent for this node. Return null"
                " for the root node.",
            ),
            # The following builtin fields are implemented as properties, so
            # they follow the ref-counting protocol (function calls return a
            # new ownership share). So unlike access to regular fields, they
            # don't need an additional inc-ref (AbstractNodeData's
            # access_needs_incref constructor argument).
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "parents", builtin=True),
                location=Location.builtin,
                expr=None,
                arguments=[
                    Argument(
                        location=Location.builtin,
                        name=names.Name("With_Self"),
                        type=T.Bool,
                        default_value=E.BooleanLiteralExpr(None, True),
                    )
                ],
                type=T.entity.array,
                public=True,
                external=True,
                uses_entity_info=True,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                access_constructor=E.parents_access_constructor,
                doc="Return an array that contains the lexical parents, this"
                " node included iff ``with_self`` is True. Nearer parents"
                " are first in the list.",
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "children", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.entity.array,
                public=True,
                external=True,
                uses_entity_info=True,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return an array that contains the direct lexical
                children.

                .. warning:: This constructs a whole array every-time you call
                    it, and as such is less efficient than calling the
                    ``Child`` built-in.
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "token_start", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.Token,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                artificial=True,
                has_property_syntax=True,
                doc="Return the first token used to parse this node.",
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "token_end", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.Token,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                artificial=True,
                has_property_syntax=True,
                doc="Return the last token used to parse this node.",
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "child_index", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.Int,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                artificial=True,
                has_property_syntax=True,
                doc="Return the 0-based index for Node in its parent's"
                " children.",
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "previous_sibling", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.entity,
                public=True,
                external=True,
                uses_entity_info=True,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return the node's previous sibling, or null if there is no such
                sibling.
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "next_sibling", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.entity,
                public=True,
                external=True,
                uses_entity_info=True,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return the node's next sibling, or null if there is no such
                sibling.
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "unit", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.AnalysisUnit,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="Return the analysis unit owning this node.",
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "ple_root", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.root_node,
                public=False,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return the PLE root that owns this node, or the unit root node
                if this unit has no PLE root.
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "is_ghost", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.Bool,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return whether the node is a ghost.

                Unlike regular nodes, ghost nodes cover no token in the input
                source: they are logically located instead between two tokens.
                Both the ``token_start`` and the ``token_end`` of all ghost
                nodes is the token right after this logical position.
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(self, "text", builtin=True),
                location=Location.builtin,
                expr=None,
                type=T.String,
                public=False,
                external=True,
                uses_entity_info=False,
                uses_envs=True,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return the text corresponding to this node. Private property
                (for internal DSL use).
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "full_sloc_image", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                type=T.String,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=True,
                warn_on_unused=False,
                artificial=True,
                has_property_syntax=True,
                doc="""
                Return a string containing the filename + the sloc in GNU
                conformant format. Useful to create diagnostics from a node.
                """,
            ),
            E.PropertyDef(
                owner=self,
                names=MemberNames.for_property(
                    self, "completion_item_kind_to_int", builtin=True
                ),
                location=Location.builtin,
                expr=None,
                arguments=[
                    Argument(
                        location=Location.builtin,
                        name=names.Name("Kind"),
                        type=T.CompletionItemKind,
                    )
                ],
                type=T.Int,
                public=True,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                warn_on_unused=False,
                artificial=True,
                doc="""
                Convert a CompletionItemKind enum to its corresponding
                integer value.
                """,
            ),
        ]

    def snaps(self, anchor_end: bool) -> bool:
        """
        Whether this node type snaps. To see what this means, see documentation
        for ``Annotations``.

        Note that no node snaps if unparsers are not requested.

        :param anchor_end: If true, return whether this node snaps at the end,
            otherwise return whether it snaps at the beginning.
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
        field_type = field_unparser.field.type
        assert isinstance(field_type, ASTNodeType)

        anchor_node_tokens = (
            unparser.post_tokens if anchor_end else unparser.pre_tokens
        )
        field_node_tokens = (
            field_unparser.post_tokens
            if anchor_end
            else field_unparser.pre_tokens
        )
        return (
            not anchor_node_tokens
            and not field_node_tokens
            and field_type.snaps(anchor_end)
        )

    @property
    def snaps_at_start(self) -> bool:
        return self.snaps(False)

    @property
    def snaps_at_end(self) -> bool:
        return self.snaps(True)

    def to_public_expr(self, internal_expr: str) -> str:
        result = "Wrap_Node ({}, {})".format(
            internal_expr, T.EntityInfo.nullexpr
        )
        if not self.is_root_node:
            result += ".As_{}".format(self.entity.api_name)
        return result

    def to_internal_expr(
        self,
        public_expr: str,
        context: str | None = None,
    ) -> str:
        return "{}.Internal.Node".format(public_expr)

    @property
    def parser_allocator(self) -> str:
        """
        Return the name of the function to call in parsers to allocate this
        node.
        """
        return "Allocate_{}".format(self.kwless_raw_name)


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

    alt_node: ASTNodeType | None
    """
    Node that implements this alternative.

    This is None when the EnumNodeAlternative object was just created, as the
    corresponding ASTNodeType is instantiated only after that.
    """

    location: Location
    """
    Location in the language spec where this alternative was created. This is
    the location of the enum node declaration itself for alternatives
    synthetized by the @qualifier annotation.
    """

    @property
    def full_name(self) -> names.Name:
        """
        Name of the node that implements this alternative. This is the node of
        the enum node, suffixed with the base name.
        """
        return self.enum_node.raw_name + self.base_name


class StringType(CompiledType):

    def __init__(self, context: CompileCtx):
        super().__init__(
            context,
            name="StringType",
            location=Location.builtin,
            exposed=True,
            null_allowed=True,
            is_refcounted=True,
            nullexpr="Empty_String",
            py_nullexpr='""',
            java_nullexpr="new StringWrapper(PointerWrapper.nullPointer())",
            hashable=True,
            has_equivalent_function=True,
            type_repo_name="String",
            api_name="TextType",
            lkt_name="String",
        )

    @property
    def public_requires_boxing(self) -> bool:
        return True

    @property
    def api_access_name(self) -> names.Name:
        """
        Name of the access type for public strings. Used as internals for
        array struct fields.
        """
        return names.Name("Text_Access")

    def to_public_expr(self, internal_expr: str) -> str:
        return "{}.Content".format(internal_expr)

    def to_internal_expr(
        self,
        public_expr: str,
        context: str | None = None,
    ) -> str:
        return "Create_String ({})".format(public_expr)


class NodeBuilderType(CompiledType):
    """
    Base class for node builder types.
    """

    @dataclass(frozen=True)
    class ConstructorArgument:
        """
        Description of a constructor argument for a synthetizing node builder.
        """

        codegen_name: str
        """
        Ada identifier for argument, to be used in code generation.
        """

        type: CompiledType
        """
        Type for this argument.
        """

        field: BaseField | None
        """
        If this argument is used to initialize a field in the node to
        synthetize, reference to that field.

        Note that in the case of parse fields, the argument type is a node
        builder, which is why ``field.type`` and ``type`` can be different.

        There is only one argument that maps to no particular field: the
        constructor argument that contains node builders corresponding to the
        children of list node. When present, this argument always comes first.
        """

        @property
        def is_parse_field(self) -> bool:
            return self.field is not None and not self.field.is_user_field

        @property
        def is_user_field(self) -> bool:
            return self.field is not None and self.field.is_user_field

    def __init__(self, context: CompileCtx, node_type: ASTNodeType):
        """
        :param node_type: Node type that this builder creates.
        """
        self.node_type = node_type
        super().__init__(
            context,
            name=node_type.name + names.Name("Node_Builder"),
            location=Location.builtin,
            fields=self.builtin_properties,
            exposed=False,
            null_allowed=False,
            is_refcounted=True,
            nullexpr="null",
            lkt_name=f"NodeBuilder[{node_type.lkt_name}]",
        )
        context.add_pending_composite_type(self)

        self.synth_node_builder_needed = False
        """
        Whether we need to generate code to create synthetizing node builders
        for this type in Ada.
        """

    @property
    def record_type(self) -> str:
        """
        Return the name of the Ada record type that implements this node
        builder type.
        """
        # This record type will not be present in generated code unless it is
        # explicitly required during the "construct" pass.
        assert self.synth_node_builder_needed
        return f"{self.name.camel_with_underscores}_Record"

    @property
    def access_type(self) -> str:
        """
        Return the name of the Ada record access type that implements this node
        builder type.
        """
        # This record type will not be present in generated code unless it is
        # explicitly required during the "construct" pass.
        assert self.synth_node_builder_needed
        return f"{self.name.camel_with_underscores}_Access"

    @property
    def synth_constructor(self) -> str:
        """
        Return the name of the Ada function that creates a synthetizing node
        builder.
        """
        # This constructor will not be present in generated code unless it is
        # explicitly required during the "construct" pass.
        assert self.synth_node_builder_needed
        return f"Create_{self.name.camel_with_underscores}"

    @property
    def synth_constructor_args(
        self,
    ) -> list[NodeBuilderType.ConstructorArgument]:
        """
        Return the list of arguments for the Ada function that creates a
        synthetizing node builder.
        """

        def arg_type(field: BaseField) -> CompiledType:
            """
            Return the type of the argument used to initialize the given field.
            """
            if isinstance(field, Field):
                assert isinstance(field.type, ASTNodeType)
                return field.type.builder_type
            else:
                return field.type

        result = [
            NodeBuilderType.ConstructorArgument(
                field.names.codegen, arg_type(field), field
            )
            for field in self.node_type.required_fields_in_exprs.values()
        ]

        # If this node builder synthetizes a list node, insert the argument
        # holding node builders for the children first in the list of
        # arguments (see the docstring for
        # ``NodeBuilderType.ConstructorArgument.field``).
        if self.node_type.is_list_type:
            elt_type = self.node_type.element_type
            assert isinstance(elt_type, ASTNodeType)
            result.insert(
                0,
                NodeBuilderType.ConstructorArgument(
                    "List_Elements", elt_type.builder_type.array, None
                ),
            )
        return result

    def builtin_properties(self, owner: CompiledType) -> list[E.PropertyDef]:
        """
        Return properties available for all node builder types.
        """
        import langkit.expressions as E

        assert owner == self

        def construct_build(
            debug_info: E.ExprDebugInfo | None,
            prefix: E.Expr,
            node_data: AbstractNodeData,
            args: list[E.Expr | None],
        ) -> E.Expr:
            """
            Create the resolved expression for a call to the ".build" property.

            See AbstractNodeData.__init__'s access_constructor.
            """
            prop = E.PropertyDef.get_or_none()
            if prop is None or not prop.lazy_field:
                error(
                    "NodeBuilder.build can be called in lazy field"
                    " initializers only",
                    location=(
                        debug_info.location if debug_info else Location.builtin
                    ),
                )

            assert len(args) == 1
            parent_expr = args[0] or E.NullExpr(None, T.root_node)

            return E.LiteralExpr(
                debug_info,
                template=(
                    "Node_Builder_Type'({}).all.Build"
                    " (Parent => {}, Self_Node => Self)"
                ),
                expr_type=node_data.type,
                operands=[prefix, parent_expr],
            )

        return [
            E.PropertyDef(
                owner=owner,
                names=MemberNames.for_property(self, "build", builtin=True),
                location=Location.builtin,
                expr=None,
                arguments=[
                    Argument(
                        Location.builtin,
                        names.Name("Parent"),
                        type=T.root_node,
                        default_value=E.NullExpr(None, T.root_node),
                    )
                ],
                type=self.node_type,
                public=False,
                external=True,
                uses_entity_info=False,
                uses_envs=False,
                optional_entity_info=False,
                dynamic_vars=[],
                doc="Create a new from this builder.",
                lazy_field=False,
                artificial=True,
                access_constructor=construct_build,
            )
        ]


class ArrayType(CompiledType):
    """
    Base class for array types.
    """

    def __init__(self, context: CompileCtx, element_type: CompiledType):
        name = element_type.name + names.Name("Array_Type")
        self.null_constant = names.Name("No") + name

        # By default, array types are not exposed. A compilation pass will tag
        # only the ones that are exposed through the public API.
        super().__init__(
            context,
            name=name,
            location=Location.builtin,
            fields=self.builtin_properties,
            is_ptr=True,
            is_refcounted=True,
            nullexpr=self.null_constant.camel_with_underscores,
            element_type=element_type,
            null_allowed=True,
            has_equivalent_function=True,
            hashable=element_type.hashable,
        )

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

        self._to_iterator_property: E.PropertyDef

        context.add_pending_composite_type(self)

    @property
    def name(self) -> names.Name:
        return self.element_type.name + names.Name("Array_Access")

    @property
    def api_name(self) -> names.Name:
        """
        Name of the public array type. This is the same as `array_type_name`
        for public types (such as booleans, integers, analysis units, etc.) but
        we have a different one for "wrapped" types, such as entities.
        """
        return self.element_type.api_name + names.Name("Array")

    @property
    def api_access_name(self) -> names.Name:
        """
        Name of the access type for public arrays. Used as internals for
        array struct fields.
        """
        return self.api_name + names.Name("Access")

    @property
    def constructor_name(self) -> names.Name:
        """
        Name of the internal Ada functions to instantiate this array.
        """
        return (
            names.Name("Create") + self.element_type.name + names.Name("Array")
        )

    @property
    def lkt_name(self) -> str:
        return f"Array[{self.element_type.lkt_name}]"

    @property
    def array_type_name(self) -> names.Name:
        """
        Name of the Ada array type.
        """
        return (
            names.Name("Internal")
            + self.element_type.name
            + names.Name("Array")
        )

    @property
    def pointed(self) -> names.Name:
        """
        Name of the type for values that are pointed to by general values.
        """
        return self.element_type.name + names.Name("Array_Record")

    @property
    def pkg_vector(self) -> names.Name:
        """
        Name of the Langkit_Support.Vectors package instantiation corresponding
        to this element_type.
        """
        return self.element_type.name + names.Name("Vectors")

    def c_type(self, c_api_settings: CAPISettings) -> CAPIType:
        if (
            isinstance(self.element_type, EntityType)
            and not self.element_type.emit_c_type
        ):
            return T.entity.array.c_type(c_api_settings)
        else:
            return CAPIType(
                c_api_settings,
                self.element_type.c_type(c_api_settings).unprefixed_name
                + names.Name("Array"),
            )

    def index_type(self) -> str:
        """
        Name of the index type for this array type.
        """
        pkg_vector_name = self.element_type.name + names.Name("Vectors")
        return "{}.Index_Type".format(pkg_vector_name.camel_with_underscores)

    def vector(self) -> str:
        """
        Name of the type for vector values.
        """
        return self.pkg_vector.camel_with_underscores + ".Vector"

    def c_create(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to create an array value.
        """
        return self.c_name(capi, "create")

    def c_inc_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to inc-ref an array value.
        """
        return self.c_name(capi, "inc_ref")

    def c_dec_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to dec-ref an array value.
        """
        return self.c_name(capi, "dec_ref")

    @property
    def py_converter(self) -> str:
        """
        Name of the Python class used to convert back and forth between
        user-facing values (lists) and C API values (pointers to array
        records).
        """
        return "_{}Converter".format(self.api_name.camel)

    @property
    def conversion_requires_context(self) -> bool:
        return self.element_type.conversion_requires_context

    @property
    def to_public_converter(self) -> names.Name:
        return names.Name("To_Public") + self.api_name

    @property
    def to_internal_converter(self) -> names.Name:
        return names.Name("To_Internal") + self.api_name

    @property
    def emit_c_type(self) -> bool:
        """
        Return whether to emit a C type for this type.

        See StructType.emit_c_type.
        """
        return (
            not isinstance(self.element_type, StructType)
            or self.element_type.emit_c_type
        )

    @property
    def public_requires_boxing(self) -> bool:
        return True

    @property
    def exposed_types(self) -> list[CompiledType]:
        return [self.element_type]

    @property
    def requires_unique_function(self) -> bool:
        return self._requires_unique_function

    def require_unique_function(self) -> None:
        self.element_type.require_hash_function()
        self._requires_unique_function = True

    def require_hash_function(self) -> None:
        super().require_hash_function()

        # Array hash functions uses the element type's hash function, so
        # it has to be required.
        self.element_type.require_hash_function()

    @property
    def requires_vector(self) -> bool:
        return self._requires_vector

    def require_vector(self) -> None:
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
        # depends on arrays of T.InnerEnvAssoc, so we need to declare it
        # early.
        return self.element_type == T.InnerEnvAssoc

    def builtin_properties(self, owner: CompiledType) -> list[E.PropertyDef]:
        """
        Return properties available for all array types.
        """
        import langkit.expressions as E

        assert owner == self

        self._to_iterator_property = E.PropertyDef(
            owner=self,
            names=MemberNames.for_property(self, "to_iterator", builtin=True),
            location=Location.builtin,
            expr=None,
            # Unless this property is actually used, or the DSL actually
            # references this iterator type, do not generate code for the
            # iterator type.
            type=self.element_type.create_iterator(used=False),
            public=False,
            external=True,
            uses_entity_info=False,
            uses_envs=False,
            optional_entity_info=False,
            dynamic_vars=[],
            doc="Return an iterator on values of this array",
            access_constructor=E.make_to_iterator,
            lazy_field=False,
            artificial=True,
        )

        return [self._to_iterator_property]


class SetType(CompiledType):
    """
    Base class for set types.
    """

    def __init__(self, context: CompileCtx, element_type: CompiledType):
        name = element_type.name + names.Name("Set_Type")
        self.null_constant = names.Name("No") + name

        # Set types cannot be exposed
        super().__init__(
            context,
            name=name,
            location=Location.builtin,
            is_ptr=True,
            is_refcounted=True,
            nullexpr=self.null_constant.camel_with_underscores,
            element_type=element_type,
            null_allowed=True,
            has_equivalent_function=True,
            hashable=False,
            exposed=False,
        )
        element_type.require_hash_function()
        context.add_pending_composite_type(self)

    @property
    def name(self) -> names.Name:
        return self.element_type.name + names.Name("Set_Access")

    @property
    def lkt_name(self) -> str:
        return f"Set[{self.element_type.lkt_name}]"

    @property
    def set_type_name(self) -> names.Name:
        """
        Name of the Ada set type.
        """
        return (
            names.Name("Internal") + self.element_type.name + names.Name("Set")
        )

    @property
    def pointed(self) -> names.Name:
        """
        Name of the type for values that are pointed to by general values.
        """
        return self.element_type.name + names.Name("Set_Record")

    @property
    def hashed_sets_pkg_name(self) -> names.Name:
        """
        Name of the instance of ``Ada.Containers.Hashed_Sets`` used by this
        set type to store its elements.
        """
        return self.element_type.name + names.Name("Hashed_Sets")


class IteratorType(CompiledType):
    """
    Base class for iterator types.
    """

    def __init__(self, context: CompileCtx, element_type: CompiledType):
        name = element_type.name + names.Name("Iterator_Type")
        self.null_constant = names.Name("No") + name
        self._is_used = False

        # By default, iterator types are not exposed. A compilation pass will
        # tag only the ones that are exposed through the public API.
        super(IteratorType, self).__init__(
            context,
            name=name,
            location=Location.builtin,
            is_ptr=True,
            is_refcounted=True,
            nullexpr=self.null_constant.camel_with_underscores,
            element_type=element_type,
            has_equivalent_function=False,
            hashable=False,
            exposed=False,
        )

        context.add_pending_composite_type(self)

    @property
    def name(self) -> names.Name:
        return self.element_type.name + names.Name("Iterator_Access")

    @property
    def lkt_name(self) -> str:
        return f"Iterator[{self.element_type.lkt_name}]"

    @property
    def api_name(self) -> names.Name:
        """
        Name of the public iterator type.
        """
        return self.element_type.api_name + names.Name("Iterator")

    @property
    def is_used(self) -> bool:
        """
        Return whether this iterator type is actually used, and thus whether we
        will emit code for it.
        """
        return self._is_used

    def c_type(self, c_api_settings: CAPISettings) -> CAPIType:
        if (
            isinstance(self.element_type, EntityType)
            and not self.element_type.emit_c_type
        ):
            return T.entity.iterator.c_type(c_api_settings)
        else:
            return CAPIType(
                c_api_settings,
                self.element_type.c_type(c_api_settings).unprefixed_name
                + names.Name("Iterator"),
            )

    def c_next(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to get the next value out of the iterator.
        """
        return self.c_name(capi, "next")

    def c_inc_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to inc-ref an iterator value.
        """
        return self.c_name(capi, "inc_ref")

    def c_dec_ref(self, capi: CAPISettings) -> str:
        """
        Name of the C API function to dec-ref an iterator value.
        """
        return self.c_name(capi, "dec_ref")

    @property
    def to_public_converter(self) -> names.Name:
        return names.Name("To_Public") + self.api_name

    @property
    def to_internal_converter(self) -> names.Name:
        return names.Name("To_Internal") + self.api_name

    @property
    def emit_c_type(self) -> bool:
        """
        Return whether to emit a C type for this type.

        See StructType.emit_c_type.
        """
        return (
            not isinstance(self.element_type, StructType)
            or self.element_type.emit_c_type
        )

    @property
    def iterator_type_name(self) -> names.Name:
        """
        Name of the Ada iterator type.
        """
        return (
            names.Name("Internal")
            + self.element_type.name
            + names.Name("Iterator")
        )

    @property
    def exposed_types(self) -> list[CompiledType]:
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

    def __init__(
        self,
        context: CompileCtx,
        name: str | names.Name,
        location: Location,
        doc: str,
        value_names: list[names.Name],
        default_val_name: names.Name | None = None,
    ):
        self.values: list[EnumValue] = [
            EnumValue(self, vn, i) for i, vn in enumerate(value_names)
        ]

        self.values_dict: dict[names.Name, EnumValue] = {
            v.name: v for v in self.values
        }
        """
        Lookup dictionnary for enumeration values, by name.
        """

        self.default_val_name = default_val_name
        """
        Name of the default value for this enum, if any.
        """

        super().__init__(
            context,
            name,
            location,
            doc,
            is_ptr=False,
            exposed=True,
            null_allowed=default_val_name is not None,
            nullexpr=(
                self.values_dict[default_val_name].ada_name
                if default_val_name is not None
                else None
            ),
            hashable=True,
        )
        context.add_pending_enum_type(self)

    @property
    def py_helper(self) -> str:
        """
        Name of the class helper in the Python binding layer to convert values
        for this enumeration back and forth between C and Python-level values.
        """
        return self.api_name.camel

    def resolve_value(
        self,
        debug_info: E.ExprDebugInfo | None,
        value_name: str,
    ) -> E.Expr:
        """
        Return an expression corresponding to the given value name.

        :param debug_info: Debug info for the returned expression.
        :param value_name: Lower-case name of the value to process.
        """
        value = self.values_dict[names.Name.from_lower(value_name)]
        return value.create_ref_expr(debug_info)


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
    def lkt_name(self) -> str:
        """
        Return the Lkt name for this enumeration value.
        """
        return "{}.{}".format(self.type.lkt_name, self.name.lower)

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
        return "{}_{}".format(
            c_api_settings.symbol_prefix.upper(),
            (self.type.name + self.name).upper,
        )

    def create_ref_expr(self, debug_info: E.ExprDebugInfo | None) -> E.Expr:
        """
        Create an expression wrapping this enumeration value.

        :param debug_info: Debug info for the returned expression.
        """
        from langkit.expressions import EnumLiteralExpr

        return EnumLiteralExpr(debug_info, self)


class BigIntegerType(CompiledType):
    def __init__(self, context: CompileCtx):
        super().__init__(
            context,
            "BigIntegerType",
            Location.builtin,
            lkt_name="BigInt",
            exposed=True,
            nullexpr="No_Big_Integer",
            is_refcounted=True,
            has_equivalent_function=True,
            is_ada_record=True,
            c_type_name="big_integer",
            api_name="BigInteger",
        )

    @property
    def to_public_converter(self) -> names.Name:
        return names.Name("Create_Public_Big_Integer")

    @property
    def to_internal_converter(self) -> names.Name:
        return names.Name("Create_Big_Integer")


class AnalysisUnitType(CompiledType):
    def __init__(self, context: CompileCtx):
        super().__init__(
            context,
            "InternalUnit",
            Location.builtin,
            fields=lambda t: [
                BuiltinField(
                    owner=t,
                    names=MemberNames.for_struct_field("root", "Ast_Root"),
                    type=T.root_node,
                    doc="Return the root node of this unit.",
                ),
            ],
            exposed=True,
            nullexpr="null",
            null_allowed=True,
            hashable=True,
            c_type_name="analysis_unit",
            api_name="AnalysisUnit",
            lkt_name="AnalysisUnit",
        )

    @property
    def to_public_converter(self) -> names.Name:
        return names.Name("Wrap_Unit")

    @property
    def to_internal_converter(self) -> names.Name:
        return names.Name("Unwrap_Unit")


class SymbolType(CompiledType):
    def __init__(self, context: CompileCtx):
        import langkit.expressions as E

        super().__init__(
            context,
            "SymbolType",
            Location.builtin,
            fields=lambda t: [
                E.PropertyDef(
                    owner=t,
                    names=MemberNames.for_property(
                        self, "image", builtin=True
                    ),
                    location=Location.builtin,
                    expr=None,
                    type=T.String,
                    arguments=[],
                    public=False,
                    external=True,
                    uses_entity_info=False,
                    uses_envs=True,
                    warn_on_unused=False,
                    artificial=True,
                    doc="Return this symbol as a string",
                ),
            ],
            lkt_name="Symbol",
            exposed=True,
            nullexpr="No_Symbol",
            null_allowed=True,
            is_ptr=False,
            # See below: symbols are represented in the C API as text records
            is_ada_record=True,
            c_type_name="symbol_type",
            api_name="UnboundedTextType",
            hashable=True,
            conversion_requires_context=True,
        )

    def to_public_expr(self, internal_expr: str) -> str:
        return "To_Unbounded_Text (Image ({}))".format(internal_expr)

    def to_internal_expr(
        self,
        public_expr: str,
        context: str | None = None,
    ) -> str:
        assert context is not None
        return "Lookup_Symbol ({}, To_Text ({}))".format(context, public_expr)


def create_builtin_types(context: CompileCtx) -> None:
    """
    Create CompiledType instances for all built-in types. This will
    automatically register them in the current CompiledTypeRepo.
    """
    from langkit.expressions.base import NullExpr

    def gen_iface_refs(*names: str) -> Callable[[], list[GenericInterface]]:
        return lambda: [context.resolve_interface(n) for n in names]

    def gen_method_ref(name: str) -> Callable[[], InterfaceMethodProfile]:
        return lambda: context.resolve_interface_method_qualname(name)

    NoCompiledType(context, "NoCompiledType", Location.builtin)

    AnalysisUnitType(context)

    EnumType(
        context,
        name="AnalysisUnitKind",
        location=Location.builtin,
        doc="""
        Specify a kind of analysis unit. Specification units provide an
        interface to the outer world while body units provide an
        implementation for the corresponding interface.
        """,
        value_names=[
            names.Name("Unit_Specification"),
            names.Name("Unit_Body"),
        ],
    )

    CompiledType(
        context, "RefCategories", Location.builtin, null_allowed=False
    )

    EnumType(
        context,
        name="LookupKind",
        location=Location.builtin,
        doc="",
        value_names=[
            names.Name("Recursive"),
            names.Name("Flat"),
            names.Name("Minimal"),
        ],
    )

    CompiledType(
        context,
        "LexicalEnv",
        Location.builtin,
        nullexpr="Empty_Env",
        null_allowed=True,
        is_ptr=False,
        is_refcounted=True,
        py_nullexpr="LexicalEnv.Empty",
        hashable=True,
        has_equivalent_function=True,
    )

    LogicVarType(context)

    CompiledType(
        context,
        "LogicEquation",
        Location.builtin,
        lkt_name="Equation",
        nullexpr="Null_Logic_Equation",
        null_allowed=False,
        c_type_name="equation_type",
        is_refcounted=True,
    )

    EnvRebindingsType(context)

    CompiledType(
        context,
        name="Boolean",
        location=Location.builtin,
        lkt_name="Bool",
        exposed=True,
        is_ptr=False,
        nullexpr="False",
        null_allowed=True,
        py_nullexpr="False",
        java_nullexpr="false",
        hashable=True,
        # "bool" is not a built-in type in C: we define our own type based on
        # uint8_t.
        c_type_name="bool",
    )

    CompiledType(
        context,
        name="Integer",
        location=Location.builtin,
        lkt_name="Int",
        exposed=True,
        is_ptr=False,
        nullexpr="0",
        external=True,
        c_type_name="int",
        hashable=True,
    )

    CompiledType(
        context,
        name="Address",
        location=Location.builtin,
        lkt_name="Address",
        exposed=False,
        is_ptr=False,
        nullexpr="System.Null_Address",
        null_allowed=True,
        external=True,
        hashable=True,
    )

    CompiledType(
        context,
        "SourceLocation",
        location=Location.builtin,
        exposed=True,
        is_ptr=False,
        nullexpr="No_Source_Location",
        null_allowed=True,
        is_ada_record=True,
    )

    CompiledType(
        context,
        "SourceLocationRange",
        location=Location.builtin,
        exposed=True,
        is_ptr=False,
        nullexpr="No_Source_Location_Range",
        null_allowed=True,
    )

    TokenType(context)
    SymbolType(context)
    BigIntegerType(context)
    StringType(context)

    CompiledType(
        context,
        "CharacterType",
        location=Location.builtin,
        lkt_name="Char",
        exposed=True,
        nullexpr="Chars.NUL",
        c_type_name="uint32_t",
        external=True,
        api_name="CharacterType",
        hashable=True,
    )

    EnumType(
        context,
        name="DesignatedEnvKind",
        location=Location.builtin,
        doc="""Discriminant for DesignatedEnv structures.""",
        value_names=[
            names.Name("None"),
            names.Name("Current_Env"),
            names.Name("Named_Env"),
            names.Name("Direct_Env"),
        ],
        default_val_name=names.Name("None"),
    )

    StructType(
        context,
        name=names.Name("Designated_Env"),
        location=Location.builtin,
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
        fields=lambda t: [
            UserField.for_struct(
                t, "kind", Location.builtin, T.DesignatedEnvKind
            ),
            UserField.for_struct(t, "env_name", Location.builtin, T.Symbol),
            UserField.for_struct(
                t, "direct_env", Location.builtin, T.LexicalEnv
            ),
        ],
    )

    logic_context = StructType(
        context,
        name=names.Name("Logic_Context"),
        location=Location.builtin,
        doc="""
            Describes an interpretation of a reference. Can be attached
            to logic atoms (e.g. Binds) to indicate under which interpretation
            this particular atom was produced, which can in turn be used to
            produce informative diagnostics for resolution failures.
        """,
        implements=gen_iface_refs("LogicContextInterface"),
        fields=lambda t: [
            UserField.for_struct(
                owner=t,
                name="ref_node",
                location=Location.builtin,
                type=T.entity,
                implements=gen_method_ref("LogicContextInterface.ref_node"),
            ),
            UserField.for_struct(
                owner=t,
                name="decl_node",
                location=Location.builtin,
                type=T.entity,
                implements=gen_method_ref("LogicContextInterface.decl_node"),
            ),
        ],
    )

    CompiledType(
        context,
        "InternalLogicContextAccess",
        location=Location.builtin,
        exposed=False,
        nullexpr="null",
        null_allowed=True,
        is_refcounted=False,
    )

    solver_diagnostic = StructType(
        context,
        name=names.Name("Solver_Diagnostic"),
        location=Location.builtin,
        doc="""
            A raw diagnostic produced by a solver resolution failure.
            This contains as much information as possible to allow formatters
            down the chain to filter/choose which diagnostics to show among
            a set of diagnostics produced for a single equation.

            * ``Message_Template`` is a string explaining the error, which may
              contain holes represented by the ``{}`` characters. Literal
              opening braces are encoded as ``{{``.

            * ``Args`` is an array of nodes, which are to be plugged in the
              holes of the template in the same order (i.e. the first argument
              goes into the first hole of the template, etc.).

            * ``Location`` is a node which indicates the location of the error.

            * ``Contexts`` is the array of contexts that were deemed relevant
              for this error.

            * ``Round`` is the solver round during which this diagnostic was
              emitted.
        """,
        implements=gen_iface_refs("SolverDiagnosticInterface"),
        fields=lambda t: [
            UserField.for_struct(
                owner=t,
                name="message_template",
                location=Location.builtin,
                type=T.String,
                implements=gen_method_ref(
                    "SolverDiagnosticInterface.message_template"
                ),
            ),
            UserField.for_struct(
                owner=t,
                name="args",
                location=Location.builtin,
                type=T.entity.array,
                implements=gen_method_ref("SolverDiagnosticInterface.args"),
            ),
            UserField.for_struct(
                owner=t,
                name="location",
                location=Location.builtin,
                type=T.root_node,
                implements=gen_method_ref(
                    "SolverDiagnosticInterface.location"
                ),
            ),
            UserField.for_struct(
                owner=t,
                name="contexts",
                location=Location.builtin,
                type=logic_context.array,
                implements=gen_method_ref(
                    "SolverDiagnosticInterface.contexts"
                ),
            ),
            UserField.for_struct(
                owner=t,
                name="round",
                location=Location.builtin,
                type=T.Int,
            ),
        ],
    )

    StructType(
        context,
        name=names.Name("Solver_Result"),
        location=Location.builtin,
        doc="""
            A pair returned by the ``Solve_With_Diagnostic`` primitive,
            consisting of:

            * A ``Success`` field indicating whether resolution was successful
              or not.

            * A ``Diagnostics`` field containing an array of diagnostics which
              may be non-empty if ``Success`` is ``False``.
        """,
        fields=lambda t: [
            UserField.for_struct(
                owner=t,
                name="success",
                location=Location.builtin,
                type=T.Bool,
            ),
            UserField.for_struct(
                owner=t,
                name="diagnostics",
                location=Location.builtin,
                type=solver_diagnostic.array,
                default_value=NullExpr(None, solver_diagnostic.array),
            ),
        ],
    )

    # Even though InitializationState really is an enum type in Ada, we do not
    # want to expose it in public API, so better let Langkit treat this as an
    # opaque type.
    CompiledType(
        context,
        name=names.Name("Initialization_State"),
        location=Location.builtin,
        is_ptr=False,
        nullexpr="Uninitialized",
    )

    EnumType(
        context,
        name="CompletionItemKind",
        location=Location.builtin,
        doc="""
        Type of completion item. Refer to the official LSP specification.
        """,
        value_names=[
            names.Name("Text_Kind"),
            names.Name("Method_Kind"),
            names.Name("Function_Kind"),
            names.Name("Constructor_Kind"),
            names.Name("Field_Kind"),
            names.Name("Variable_Kind"),
            names.Name("Class_Kind"),
            names.Name("Interface_Kind"),
            names.Name("Module_Kind"),
            names.Name("Property_Kind"),
            names.Name("Unit_Kind"),
            names.Name("Value_Kind"),
            names.Name("Enum_Kind"),
            names.Name("Keyword_Kind"),
            names.Name("Snippet_Kind"),
            names.Name("Color_Kind"),
            names.Name("File_Kind"),
            names.Name("Reference_Kind"),
            names.Name("Folder_Kind"),
            names.Name("Enum_Member_Kind"),
            names.Name("Constant_Kind"),
            names.Name("Struct_Kind"),
            names.Name("Event_Kind"),
            names.Name("Operator_Kind"),
            names.Name("Type_Parameter_Kind"),
        ],
    )

    # EnvAssoc type, used to add associations of key and value to the lexical
    # environments, via the add_to_env primitive.
    StructType(
        context,
        name=names.Name("Env_Assoc"),
        location=Location.builtin,
        fields=lambda t: [
            UserField.for_struct(t, "key", Location.builtin, T.Symbol),
            UserField.for_struct(t, "value", Location.builtin, T.root_node),
            UserField.for_struct(
                t, "dest_env", Location.builtin, T.DesignatedEnv
            ),
            UserField.for_struct(t, "metadata", Location.builtin, T.Metadata),
        ],
    )

    # Type to hold "inner environment associations". This built-in type is
    # involved in the dynamic primary envs mechanism.
    StructType(
        context,
        names.Name("Inner_Env_Assoc"),
        location=Location.builtin,
        fields=lambda t: [
            UserField.for_struct(t, "key", Location.builtin, T.Symbol),
            UserField.for_struct(t, "value", Location.builtin, T.root_node),
            UserField.for_struct(
                t,
                "rebindings",
                Location.builtin,
                T.EnvRebindings,
                default_value=NullExpr(None, T.EnvRebindings),
            ),
            UserField.for_struct(
                t,
                "metadata",
                Location.builtin,
                T.Metadata,
                default_value=NullExpr(None, T.Metadata),
            ),
        ],
    )

    # Entity info type: a record that contains semantic information which, when
    # added to an AST node, makes an entity.
    StructType(
        context,
        name=names.Name("Entity_Info"),
        location=Location.builtin,
        fields=lambda t: [
            BuiltinField(
                owner=t,
                names=MemberNames.for_struct_field("md"),
                type=T.Metadata,
                doc="The metadata associated to the AST node",
            ),
            BuiltinField(
                owner=t,
                names=MemberNames.for_struct_field("rebindings"),
                type=T.EnvRebindings,
                access_needs_incref=True,
                doc="",
            ),
            BuiltinField(
                owner=t,
                names=MemberNames.for_struct_field("from_rebound"),
                type=T.Bool,
                doc="",
            ),
        ],
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

    def __getattr__(self, type_name: str) -> CompiledType:
        """
        Look for a type by name.

        :param str type_name: The name of the rule.
        """
        return CompiledTypeRepo.type_dict[type_name]

    @property
    def root_node(self) -> ASTNodeType:
        """
        Shortcut to get the root AST node.
        """
        return get_context().root_node_type

    @property  # type: ignore
    @memoized
    def node_kind(self) -> names.Name:
        """
        Name of type node kind type.
        """
        return self.root_node.entity.api_name + names.Name("Kind_Type")

    @property
    def env_md(self) -> StructType:
        """
        Shortcut to get the lexical environment metadata type.
        """
        return get_context().env_metadata

    @property
    def entity(self) -> EntityType:
        """
        This property returns the root type used to describe an AST node with
        semantic information attached.
        """
        return self.root_node.entity

    @property
    def all_types(self) -> ValuesView[CompiledType]:
        """
        Return all compiled types created so far.
        """
        return CompiledTypeRepo.type_dict.values()


T = TypeRepo()
"""
Default type repository instance, to be used to refer to a type before its
declaration.
"""
