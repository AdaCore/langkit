from __future__ import annotations

import dataclasses
import enum
from functools import reduce
import itertools
from typing import Any, Callable, Type, overload

import liblktlang as L

from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    Annotations,
    Argument,
    BaseField,
    CompiledType,
    CompiledTypeRepo,
    EnumNodeAlternative,
    EnumType,
    Field,
    MemberNames,
    MetadataField,
    StructType,
    T,
    UserField,
)
from langkit.diagnostics import Location, check_source_language, error
from langkit.envs import (
    AddEnv,
    AddToEnv,
    Do,
    EnvAction,
    EnvSpec,
    HandleChildren,
    RefEnvs,
    RefKind,
    SetInitialEnv,
)
import langkit.expressions as E
from langkit.expressions import (
    AbstractExpression,
    AbstractKind,
    AbstractVariable,
    Cast,
    Let,
    LocalVars,
    NullCond,
    PropertyDef,
    lazy_field,
)
from langkit.frontend.annotations import (
    AnnotationSpec,
    FlagAnnotationSpec,
    ParsedAnnotations,
    StringLiteralAnnotationSpec,
    check_no_annotations,
    parse_annotations,
)
import langkit.frontend.func_signatures as S
from langkit.frontend.resolver import Resolver
from langkit.frontend.scopes import Scope
from langkit.frontend.static import (
    denoted_char,
    denoted_str,
    parse_static_bool,
    parse_static_str,
)
from langkit.frontend.utils import (
    lkt_context,
    lkt_doc,
    name_from_camel,
    name_from_lower,
)
from langkit.generic_interface import GenericArgument, GenericInterface
import langkit.names as names


def extract_var_name(ctx: CompileCtx, id: L.Id) -> tuple[str, names.Name]:
    """
    Turn the lower cased name ``n`` into a valid Ada identifier (for code
    generation).
    """
    source_name = id.text
    var_name = (
        names.Name("Ignored")
        if source_name == "_" else
        names.Name("Local") + name_from_lower(ctx, "variable", id)
    )
    return source_name, var_name


class BuiltinAttribute(enum.Enum):
    as_bare_entity = enum.auto()
    as_entity = enum.auto()
    children = enum.auto()
    env_node = enum.auto()
    env_parent = enum.auto()
    is_null = enum.auto()
    parent = enum.auto()
    rebindings_new_env = enum.auto()
    rebindings_old_env = enum.auto()
    rebindings_parent = enum.auto()
    root = enum.auto()
    symbol = enum.auto()
    to_symbol = enum.auto()


class BuiltinMethod(enum.Enum):
    all = enum.auto()
    any = enum.auto()
    append_rebinding = enum.auto()
    as_array = enum.auto()
    as_big_int = enum.auto()
    as_int = enum.auto()
    concat_rebindings = enum.auto()
    contains = enum.auto()
    do = enum.auto()
    empty = enum.auto()
    env_group = enum.auto()
    env_orphan = enum.auto()
    filter = enum.auto()
    filtermap = enum.auto()
    find = enum.auto()
    get = enum.auto()
    get_first = enum.auto()
    get_value = enum.auto()
    iall = enum.auto()
    iany = enum.auto()
    ifilter = enum.auto()
    ifiltermap = enum.auto()
    ilogic_all = enum.auto()
    ilogic_any = enum.auto()
    imap = enum.auto()
    imapcat = enum.auto()
    is_visible_from = enum.auto()
    itake_while = enum.auto()
    join = enum.auto()
    length = enum.auto()
    logic_all = enum.auto()
    logic_any = enum.auto()
    map = enum.auto()
    mapcat = enum.auto()
    rebind_env = enum.auto()
    shed_rebindings = enum.auto()
    singleton = enum.auto()
    solve = enum.auto()
    solve_with_diagnostics = enum.auto()
    super = enum.auto()
    take_while = enum.auto()
    to_builder = enum.auto()
    unique = enum.auto()
    update = enum.auto()


class ExternalAnnotationSpec(AnnotationSpec):
    """
    Interpreter for the @external annotation on properties.
    """

    @dataclasses.dataclass
    class Value:
        uses_envs: bool = False
        uses_entity_info: bool = False

    def __init__(self) -> None:
        super().__init__(
            "external", unique=True, require_args=True, default_value=None
        )

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        for arg in args:
            with lkt_context(arg):
                error("no positional argument expected")

        result = self.Value()
        for k, v in kwargs.items():
            if k == "uses_envs":
                result.uses_envs = parse_static_bool(ctx, v)
            elif k == "uses_entity_info":
                result.uses_entity_info = parse_static_bool(ctx, v)
            else:
                error(f"invalid keyword argument: {k}")
        return result


class GenericInterfaceAnnotationSpec(AnnotationSpec):
    """
    Interpreter for the @generic_interface annotation on triats.
    """

    @dataclasses.dataclass
    class Value:
        node_only: bool = False

    def __init__(self) -> None:
        super().__init__(
            "generic_interface",
            unique=True,
            require_args=True,
            default_value=None,
        )

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        for arg in args:
            with lkt_context(arg):
                error("no positional argument expected")

        result = self.Value()
        for k, v in kwargs.items():
            if k == "node_only":
                result.node_only = parse_static_bool(ctx, v)
            else:
                error(f"invalid keyword argument: {k}")
        return result


class WithDefaultAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_default annotations for enum types.
    """
    def __init__(self) -> None:
        super().__init__("with_default", unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        check_source_language(
            len(args) == 1 and not kwargs,
            "exactly one positional argument expected",
        )
        return args[0]


class WithDynvarsAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_dynvars annotations for properties.
    """
    def __init__(self) -> None:
        super().__init__("with_dynvars", unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        result: list[
            tuple[Scope.BuiltinDynVar | Scope.DynVar, L.Expr | None]
        ] = []

        def add(
            entity: Scope.Entity,
            default_value: L.Expr | None = None,
        ) -> None:
            """
            Append a dynamic variable to ``result``. This also performs
            validity checks on the arguments.

            :param entity: Entity that is supposed to be a dynamic variable
                (this is checked).
            :param default_value: If this dynamic variable is optional, default
                value for it.
            """
            if not isinstance(entity, (Scope.BuiltinDynVar, Scope.DynVar)):
                error(
                    f"dynamic variable expected, got {entity.diagnostic_name}"
                )
            if entity in result:
                error("dynamic variables can appear at most once")
            result.append((entity, default_value))

        # Positional arguments are supposed to be just dynamic variable names
        for arg in args:
            with lkt_context(arg):
                entity = scope.resolve(arg)
                add(entity)

        # Keyword arguments are supposed to associate a dynamic variable name
        # ("name" below) to a default value for the dynamic variable in the
        # current property ("default_value" below).
        for name, default_value in kwargs.items():
            try:
                entity = scope.lookup(name)
            except KeyError as exc:
                error(exc.args[0])
            add(entity, default_value)

        return result


@dataclasses.dataclass
class BaseNodeAnnotations(ParsedAnnotations):
    custom_short_image: bool
    generic_list_type: str | None
    with_abstract_list: bool
    ple_unit_root: bool
    rebindable: bool
    repr_name: str | None
    snaps: bool
    synthetic: bool
    annotations = [
        FlagAnnotationSpec("custom_short_image"),
        StringLiteralAnnotationSpec("generic_list_type"),
        FlagAnnotationSpec("with_abstract_list"),
        FlagAnnotationSpec("ple_unit_root"),
        StringLiteralAnnotationSpec("repr_name"),
        FlagAnnotationSpec("rebindable"),
        FlagAnnotationSpec("snaps"),
        FlagAnnotationSpec('synthetic'),
    ]


@dataclasses.dataclass
class TraitAnnotations(ParsedAnnotations):
    builtin: bool
    generic_interface: GenericInterfaceAnnotationSpec.Value | None
    annotations = [
        FlagAnnotationSpec("builtin"),
        GenericInterfaceAnnotationSpec(),
    ]


@dataclasses.dataclass
class NodeAnnotations(BaseNodeAnnotations):
    abstract: bool
    annotations = BaseNodeAnnotations.annotations + [
        FlagAnnotationSpec('abstract')
    ]


@dataclasses.dataclass
class EnumNodeAnnotations(BaseNodeAnnotations):
    qualifier: bool
    annotations = BaseNodeAnnotations.annotations + [
        FlagAnnotationSpec('qualifier')
    ]


@dataclasses.dataclass
class FieldAnnotations(ParsedAnnotations):
    abstract: bool
    exported: bool
    final: bool
    lazy: bool
    null_field: bool
    nullable: bool
    parse_field: bool
    traced: bool
    used_in_equality: bool
    annotations = [FlagAnnotationSpec('abstract'),
                   FlagAnnotationSpec('exported'),
                   FlagAnnotationSpec('final'),
                   FlagAnnotationSpec('lazy'),
                   FlagAnnotationSpec('null_field'),
                   FlagAnnotationSpec('nullable'),
                   FlagAnnotationSpec('parse_field'),
                   FlagAnnotationSpec('traced'),
                   FlagAnnotationSpec('used_in_equality')]


@dataclasses.dataclass
class EnumAnnotations(ParsedAnnotations):
    with_default: L.Expr | None
    annotations = [
        WithDefaultAnnotationSpec()
    ]


@dataclasses.dataclass
class StructAnnotations(ParsedAnnotations):
    metadata: bool
    annotations = [FlagAnnotationSpec("metadata")]


@dataclasses.dataclass
class FunArgAnnotations(ParsedAnnotations):
    ignored: bool
    annotations = [FlagAnnotationSpec("ignored")]


@dataclasses.dataclass
class FunAnnotations(ParsedAnnotations):
    abstract: bool
    call_memoizable: bool
    call_non_memoizable_because: str | None
    exported: bool
    external: ExternalAnnotationSpec.Value | None
    final: bool
    ignored: bool
    memoized: bool
    predicate_error: str | None
    property: bool
    traced: bool
    with_dynvars: list[tuple[Scope.DynVar, L.Expr | None]] | None
    annotations = [
        FlagAnnotationSpec('abstract'),
        FlagAnnotationSpec('call_memoizable'),
        StringLiteralAnnotationSpec('call_non_memoizable_because'),
        FlagAnnotationSpec('exported'),
        ExternalAnnotationSpec(),
        FlagAnnotationSpec('final'),
        FlagAnnotationSpec('ignored'),
        FlagAnnotationSpec('memoized'),
        StringLiteralAnnotationSpec('predicate_error'),
        FlagAnnotationSpec('property'),
        FlagAnnotationSpec('traced'),
        WithDynvarsAnnotationSpec(),
    ]


@dataclasses.dataclass
class FieldKinds:
    """
    Set of field kinds. Used to filter what kind of fields are legal depending
    on the context.
    """
    properties: bool = False
    parse_fields: bool = False
    user_fields: bool = False
    metadata_fields: bool = False

    def has(self, cls: Type[AbstractNodeData]) -> bool:
        """
        Return whether this set of field kinds accepts ``cls``.
        """
        return (
            (self.properties and issubclass(cls, PropertyDef))
            or (self.parse_fields and issubclass(cls, Field))
            or (self.user_fields and issubclass(cls, UserField))
            or (self.metadata_fields and issubclass(cls, MetadataField))
        )


# Mapping to associate declarations to the corresponding AbstractVariable
# instances. This is useful when lowering expressions.
LocalsEnv = dict[L.BaseValDecl, AbstractVariable]


class LktTypesLoader:
    """
    Helper class to instantiate ``CompiledType`` for all types described in
    Lkt.
    """

    # Map Lkt type declarations to the corresponding CompiledType instances, or
    # to None when the type declaration is currently being lowered. Keeping a
    # None entry in this case helps detecting illegal circular type
    # dependencies.
    compiled_types: dict[L.TypeDecl, CompiledType | None]

    #############################
    # Property lowering helpers #
    #############################

    @dataclasses.dataclass
    class PropertyToLower:
        prop: PropertyDef
        """
        The property whose expression must be lowered.
        """

        arguments: list[L.FunArgDecl]
        """
        Arguments for this property.
        """

        dynamic_vars: list[tuple[E.DynamicVariable, L.Expr | None]] | None
        """
        Dynamic variables for this property, and optional default value for
        each one. If None, inherit dynamic variables from the base property.
        """

    @dataclasses.dataclass
    class PropertyAndExprToLower(PropertyToLower):
        body: L.Expr
        """
        Root expression to lower.
        """

        scope: Scope
        """
        Scope to use during lowering. The property arguments must be available
        in it.
        """

    @dataclasses.dataclass
    class FieldToLower:
        field: UserField
        """
        Field to lower.
        """

        default_value: L.Expr
        """
        Expression to lower for this fields' default value.
        """

    @dataclasses.dataclass
    class BuiltinCallInfo:
        """
        Information about the call to a builtin operation that takes a lambda
        as the first argument, plus optional keyword arguments.
        """
        kwargs: dict[str, L.Expr]
        """
        Keyword arguments passed after the lambda expression.
        """

        scope: Scope
        """
        New scope to lower lambda function arguments and inner expression.
        """

        largs: list[L.LambdaArgDecl]
        """
        List of arguments for this lambda expression.
        """

        expr: L.Expr
        """
        Lambda expression "body".
        """

    @dataclasses.dataclass
    class CollectionLoweringResult:
        """
        Container for the result of the "lower_collection_iter" function.
        """

        inner_expr: AbstractExpression
        """
        Expression to evaluate each element of the array the collection
        expression computes.
        """

        lambda_arg_infos: list[E.LambdaArgInfo]
        """
        Information about all lambda arguments involved in this expression.
        """

        element_var: AbstractVariable
        """
        Iteration variable to hold each collection element.
        """

        index_var: AbstractVariable | None
        """
        Iteration variable to hold each collection element index, if needed.
        """

    @dataclasses.dataclass
    class DeclAction:
        """
        Helper for block lowering. Represents a declaration in a block
        expression.
        """

        var: AbstractVariable
        """
        Abstract variable corresponding to an entity declared in a block.
        """

        init_expr: AbstractExpression
        """
        Initialization expression for this variable.
        """

        location: Location
        """
        Location of this variable declaration. This may not be ``var``'s
        location for "declarations" that represent dynamic variable bindings:
        the dynamic variable is declared at the module level, whereas this
        "declaration" is located inside a property ("VarBind" Lkt node).
        """

    def __init__(self, resolver: Resolver):
        """
        :param resolver: Context in which to create these types.
        """
        self.resolver = resolver
        self.ctx = resolver.context

        self.root_scope = self.resolver.root_scope
        self.refd_env_scope = self.resolver.refd_env_scope

        self.generics = resolver.builtins.generics

        self.dynamic_lexical_env = (
            resolver.builtins.functions.dynamic_lexical_env
        )

        self.node_builtin = resolver.builtins.values.node
        self.self_builtin = resolver.builtins.values.self

        self.error_location_builtin = resolver.builtins.dyn_vars.error_location
        self.logic_context_builtin = resolver.builtins.dyn_vars.logic_context

        self.precondition_failure = (
            resolver.builtins.exceptions.precondition_failure
        )
        self.property_error = resolver.builtins.exceptions.property_error

        self.named_types: dict[str, L.TypeDecl] = {}
        self.compiled_types: dict[L.Decl, CompiledType | None] = {}
        self.internal_property_counter = iter(itertools.count(0))
        self.error_nodes: list[ASTNodeType] = []

        type_decls: list[L.TypeDecl] = []
        dyn_vars: list[L.DynVarDecl] = []
        root_node_decl: L.BasicClassDecl | None = None
        self.gen_iface_decls: list[tuple[GenericInterface, L.TraitDecl]] = []

        # Look for generic interfaces defined in the prelude
        assert isinstance(resolver.lkt_units[0].root, L.LangkitRoot)
        prelude = resolver.lkt_units[0].root.p_fetch_prelude
        assert isinstance(prelude.root, L.LangkitRoot)
        for full_decl in prelude.root.f_decls:
            if isinstance(full_decl.f_decl, L.TraitDecl):
                self.process_prelude_decl(full_decl)

        # Go through all units and register all top-level definitions in the
        # root scope. This first pass allows to check for name uniqueness,
        # create TypeRepo.Defer objects and build the list of types to lower.
        for unit in resolver.lkt_units:
            assert isinstance(unit.root, L.LangkitRoot)
            for full_decl in unit.root.f_decls:
                decl = full_decl.f_decl
                name = decl.f_syn_name.text
                if isinstance(decl, L.LexerDecl):
                    self.root_scope.add(Scope.Lexer(name, decl))
                elif isinstance(decl, L.GrammarDecl):
                    self.root_scope.add(Scope.Grammar(name, decl))
                elif isinstance(decl, L.TraitDecl):
                    self.process_user_trait(decl)
                elif isinstance(decl, L.TypeDecl):
                    self.named_types[name] = decl
                    type_decls.append(decl)

                    # Keep track of anyhing that looks like the root node
                    if (
                        isinstance(decl, L.BasicClassDecl)
                        and decl.p_base_type is None
                    ):
                        root_node_decl = decl

                elif isinstance(decl, L.DynVarDecl):
                    dyn_vars.append(decl)

                else:
                    error(
                        "invalid top-level declaration:"
                        f" {decl.p_decl_type_name}"
                    )

        # There is little point going further if we have not found the root
        # node type.
        if root_node_decl is None:
            error(
                "no node type declaration found",
                location=resolver.root_lkt_source_loc,
            )

        # At this stage, all generic interfaces are lowered, so we can process
        # all deferred references.
        self.ctx.deferred.implemented_interfaces.resolve()

        #
        # TYPES_LOWERING
        #

        # Now create CompiledType instances for each user type, and
        # GenericInterface instances for the relevant traits. To properly
        # handle node derivation, this recurses on bases first and reject
        # inheritance loops.
        self.properties_to_lower: list[LktTypesLoader.PropertyToLower] = []
        self.env_specs_to_lower: list[tuple[ASTNodeType, L.EnvSpecDecl]] = []
        self.fields_to_lower: list[LktTypesLoader.FieldToLower] = []
        for type_decl in type_decls:
            self.lower_type_decl(type_decl)

        # If user code does not define one, create a default Metadata struct
        # and make it visible in the root scope. Otherwise, validate it.
        if CompiledTypeRepo.env_metadata is None:
            self.ctx.create_default_metadata()
            self.root_scope.mapping["Metadata"] = Scope.BuiltinType(
                "Metadata", T.Metadata
            )

        #
        # DYNVAR_LOWERING
        #

        # Create dynamic variables
        for dyn_var_decl in dyn_vars:
            name_node = dyn_var_decl.f_syn_name

            # Ensure the dynamic variable name has proper casing
            _ = name_from_lower(self.ctx, "dynamic variable", name_node)

            name = name_node.text
            dyn_var = E.DynamicVariable(
                name=name,
                type=self.resolver.resolve_type(
                    dyn_var_decl.f_decl_type, self.root_scope
                ),
                doc=lkt_doc(dyn_var_decl),
            )
            self.root_scope.add(Scope.DynVar(name, dyn_var_decl, dyn_var))

        #
        # TYPE_MEMBERS_LOWERING
        #

        # Now that there is a CompiledType instance for all builtin and named
        # types, it is possible to instantiate all type members: do that for
        # type members that were deferred so far.
        self.ctx.deferred.type_members.resolve()

        # Finally, now that type members are populated, make sure the metadata
        # struct fields are legal.
        self.ctx.check_env_metadata(CompiledTypeRepo.env_metadata)

        # Reject non-null fields for error nodes. Non-null fields can come from
        # this node's own declaration, or they can come from inheritance.
        for node in self.error_nodes:
            error_msg = "Error nodes can only have null fields"
            for f in node.get_parse_fields(include_inherited=True):
                if not (f.null or f.abstract):
                    if f.struct != node:
                        error(
                            f"{error_msg}: {f.qualname} is not null",
                            location=node.location,
                        )
                    else:
                        error(error_msg, location=f.location)

        #
        # GENERIC_INTERFACE_MEMBERS_LOWERING
        #

        # Lower generic interface members
        for gen_iface, gen_iface_decl in self.gen_iface_decls:
            self.lower_generic_interface_members(gen_iface, gen_iface_decl)

        # Now that all generic interface members are known, evaluate the
        # deferred references to them.
        self.ctx.deferred.implemented_methods.resolve()

        #
        # ENV_SPECS_LOWERING
        #

        for node, env_spec_decl in self.env_specs_to_lower:
            env_spec = self.lower_env_spec(node, env_spec_decl)
            node.env_spec = env_spec
            env_spec.ast_node = node
            env_spec.register_categories(self.ctx)

        #
        # EXPR_LOWERING
        #

        # Now that all user-defined compiled types are known, we can start
        # lowering expressions and env specs. Start with default values for
        # property arguments and dynamic variables.
        for p_to_lower in self.properties_to_lower:
            for arg_decl, arg in zip(
                p_to_lower.arguments, p_to_lower.prop.arguments
            ):
                if arg_decl.f_default_val is not None:
                    value = self.lower_static_expr(
                        arg_decl.f_default_val, self.root_scope
                    )
                    value.prepare()
                    arg.set_default_value(value)

            if p_to_lower.dynamic_vars is not None:
                p_to_lower.prop.set_dynamic_vars(
                    [
                        (
                            dynvar,
                            None
                            if init_expr is None else
                            self.lower_static_expr(init_expr, self.root_scope)
                        )
                        for dynvar, init_expr in p_to_lower.dynamic_vars
                    ]
                )

        # Now that all types and properties ("declarations") are available,
        # lower the property expressions themselves.
        for p_to_lower in self.properties_to_lower:
            if isinstance(p_to_lower, self.PropertyAndExprToLower):
                with p_to_lower.prop.bind():
                    self.reset_names_counter()
                    p_to_lower.prop.expr = self.lower_expr(
                        p_to_lower.body, p_to_lower.scope, p_to_lower.prop.vars
                    )

        # Finally, lower default values for fields
        for f_to_lower in self.fields_to_lower:
            f_to_lower.field.abstract_default_value = self.lower_expr(
                f_to_lower.default_value, self.root_scope, None
            )

    def resolve_base_node(self, name: L.TypeRef) -> ASTNodeType:
        """
        Resolve a type reference and lower it, checking that it is a node type.

        Note: This method is meant to be used instead of ``resolve_node``
        during the TYPES_LOWERING pass since scopes are not populated yet at
        this stage, and yet to handle inheritance correctly, we need to
        resolve reference to base classes. This is done using the
        ``named_types`` map.
        """
        # There are only two legal cases: the base type is just a node class
        # defined in user code (SimpleTypeRef) or it is a bare node list
        # instantiation (GenericTypeRef). Reject everything else.
        if isinstance(name, L.SimpleTypeRef):
            # We have a direct node class reference: first fetch the Lkt
            # declaration for it.
            try:
                base_type_decl = self.named_types[name.text]
            except KeyError:
                error(f"no such node type: '{name.text}'", location=name)

            # Then, force its lowering
            base_type = self.lower_type_decl(base_type_decl)
            if not isinstance(base_type, ASTNodeType):
                error("node type expected", location=name)
            return base_type

        elif isinstance(name, L.GenericTypeRef):
            # This must be a node list instantiation: validate the
            # instantiation itself.
            astlist_name = self.generics.ast_list.name
            if name.f_type_name.text != astlist_name:
                error(
                    "the only generic allowed in this context is"
                    f" {astlist_name}",
                    location=name,
                )

            # Lower type arguments
            type_args = [self.resolve_base_node(t) for t in name.f_params]
            check_source_language(
                len(type_args) == 1,
                f"{astlist_name} expects type argument: the list element type",
                location=name,
            )
            return type_args[0].list

        else:
            error("invalid node type reference", location=name)

    def lower_type_decl(self, decl: L.TypeDecl) -> CompiledType:
        """
        Create the CompiledType instance corresponding to the given Lkt type
        declaration. Do nothing if it has been already lowered, and stop with
        an error if the lowering for this type is already running (case of
        invalid circular type dependency).
        """
        with lkt_context(decl):
            # Sentinel for the dict lookup below, as compiled_type can contain
            # None entries.
            try:
                t = self.compiled_types[decl]
            except KeyError:
                # The type is not lowered yet: let's do it. Add the sentinel to
                # reject type inheritance loop during recursion.
                self.compiled_types[decl] = None
            else:
                if t is None:
                    error('Type inheritance loop detected')
                else:
                    # The type is already lowered: there is nothing to do
                    return t

            # Dispatch now to the appropriate lowering helper
            result: CompiledType
            full_decl = decl.parent
            assert isinstance(full_decl, L.FullDecl)
            if isinstance(decl, L.BasicClassDecl):

                specs = (EnumNodeAnnotations
                         if isinstance(decl, L.EnumClassDecl)
                         else NodeAnnotations)
                result = self.create_node(
                    decl,
                    parse_annotations(
                        self.ctx, specs, full_decl, self.root_scope
                    ),
                )

            elif isinstance(decl, L.EnumTypeDecl):
                check_source_language(
                    len(decl.f_traits) == 0,
                    "No traits allowed on enum types",
                    location=decl.f_traits,
                )
                result = self.create_enum(
                    decl,
                    parse_annotations(
                        self.ctx, EnumAnnotations, full_decl, self.root_scope
                    )
                )

            elif isinstance(decl, L.StructDecl):
                result = self.create_struct(
                    decl,
                    parse_annotations(
                        self.ctx, StructAnnotations, full_decl, self.root_scope
                    )
                )

            else:
                raise NotImplementedError(
                    'Unhandled type declaration: {}'.format(decl)
                )

            self.compiled_types[decl] = result
            self.root_scope.add(
                Scope.UserType(decl.f_syn_name.text, decl, result)
            )
            return result

    def lower_base_field(
        self,
        owner: CompiledType,
        full_decl: L.FullDecl,
        allowed_field_kinds: FieldKinds,
    ) -> AbstractNodeData:
        """
        Lower the field described in ``decl``.

        :param allowed_field_kinds: Set of field kinds allowed for the fields
            to load.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FieldDecl)

        # Ensure the dynamic variable name has proper casing
        name = name_from_lower(self.ctx, "field", decl.f_syn_name)

        annotations = parse_annotations(
            self.ctx, FieldAnnotations, full_decl, self.root_scope
        )
        field_type = self.resolver.resolve_type(
            decl.f_decl_type, self.root_scope
        )
        doc = lkt_doc(decl)

        cls: Type[AbstractNodeData]
        constructor: Callable[..., AbstractNodeData]
        kwargs: dict[str, Any] = {'type': field_type, 'doc': doc}

        check_source_language(
            annotations.parse_field or not annotations.null_field,
            '@nullable is valid only for parse fields'
        )

        names: MemberNames
        body: L.Expr | None = None
        if annotations.lazy:
            check_source_language(
                not annotations.null_field,
                'Lazy fields cannot be null'
            )
            check_source_language(
                not annotations.final,
                'Lazy fields are implicitly final'
            )
            cls = PropertyDef
            constructor = lazy_field
            names = MemberNames.for_lazy_field(owner, name)

            body = decl.f_default_val

            kwargs = {
                'expr': None,
                'doc': doc,
                'public': annotations.exported,
                'return_type': field_type,
                'kind': (AbstractKind.abstract
                         if annotations.abstract
                         else AbstractKind.concrete),
                'activate_tracing': annotations.traced,
            }

        elif annotations.parse_field:
            assert decl.f_default_val is None
            check_source_language(
                not annotations.exported,
                'Parse fields are implicitly exported'
            )
            check_source_language(
                not annotations.final,
                'Concrete parse fields are implicitly final'
            )
            check_source_language(
                not annotations.lazy,
                'Parse fields cannot be lazy'
            )
            check_source_language(
                not annotations.traced,
                'Parse fields cannot be traced'
            )
            cls = constructor = Field
            names = MemberNames.for_node_field(owner, name)
            kwargs['abstract'] = annotations.abstract
            kwargs['null'] = annotations.null_field
            kwargs['nullable'] = annotations.nullable

        else:
            check_source_language(
                not annotations.abstract,
                'Regular fields cannot be abstract'
            )
            check_source_language(
                not annotations.exported,
                'Regular fields are implicitly exported'
            )
            check_source_language(
                not annotations.final,
                'Regular fields are implicitly final'
            )
            check_source_language(
                not annotations.lazy,
                'Regular fields cannot be lazy'
            )
            check_source_language(
                not annotations.null_field,
                'Regular fields cannot be null'
            )
            check_source_language(
                not annotations.traced,
                'Regular fields cannot be traced'
            )
            cls = constructor = UserField
            names = (
                MemberNames.for_node_field(owner, name)
                if isinstance(owner, ASTNodeType) else
                MemberNames.for_struct_field(name)
            )
            kwargs['public'] = not isinstance(owner, ASTNodeType)

            # If this field belongs to the metadata struct, use the appropriate
            # constructor. Reject @used_in_equality annotations otherwise, as
            # they are valid only for metadata fields.
            if allowed_field_kinds.metadata_fields:
                cls = constructor = MetadataField
                kwargs["use_in_equality"] = annotations.used_in_equality
            else:
                check_source_language(
                    not annotations.used_in_equality,
                    "Only metadata fields can have the @used_in_equality"
                    " annotation",
                )

        check_source_language(
            allowed_field_kinds.has(cls), 'Invalid field type in this context'
        )

        result = constructor(names=names, **kwargs)
        result.location = Location.from_lkt_node(decl)

        if decl.f_trait_ref is not None:
            assert isinstance(result, (PropertyDef, BaseField))
            self.set_implemented_method(result, decl.f_trait_ref)

        # If this field has an initialization expression implemented as
        # property, plan to lower it later.
        if isinstance(result, PropertyDef):
            assert body is not None
            arguments, scope = self.lower_property_arguments(
                prop=result,
                arg_decl_list=None,
                label=f"initializer for lazy field {result.qualname}",
            )
            self.properties_to_lower.append(
                self.PropertyAndExprToLower(
                    result, arguments, None, body, scope
                )
            )

        if isinstance(result, UserField) and decl.f_default_val is not None:
            self.fields_to_lower.append(
                self.FieldToLower(result, decl.f_default_val)
            )

        return result

    def lower_static_expr(
        self,
        expr: L.Expr,
        env: Scope,
    ) -> AbstractExpression:
        """
        Lower the given expression, checking that it is a valid compile time
        known value.
        """
        return self.lower_expr(
            expr, env, local_vars=None, static_required=True
        )

    def create_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        lower_expr: Callable[[PropertyDef], AbstractExpression],
        rtype: CompiledType | None,
        location: Location,
    ) -> PropertyDef:
        """
        Create an internal property.

        This is similar to ``lower_expr_to_internal_property``, but with a
        callback to get the lowered expression body.
        """
        result = PropertyDef(
            names=MemberNames.for_internal(name),
            expr=None,
            public=False,
            type=rtype,
        )

        # Internal properties never have dynamic variables
        result.set_dynamic_vars([])

        self.reset_names_counter()

        with result.bind():
            result.expr = lower_expr(result)

        # Register this new property as a field of the owning node
        node.add_field(result)

        result.location = location
        return result

    def reset_names_counter(self) -> None:
        """
        Reset the counter used to generate names that are unique inside a
        property (e.g. for local variables).

        This method must be called each time we are about to lower a property's
        body expression.
        """
        self.names_counter = itertools.count(0)

    @overload
    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        expr: L.Expr | AbstractExpression,
        rtype: CompiledType | None,
    ) -> PropertyDef: ...

    @overload
    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        expr: None,
        rtype: CompiledType | None,
    ) -> None: ...

    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        expr: L.Expr | AbstractExpression | None,
        rtype: CompiledType | None,
    ) -> PropertyDef | None:
        """
        Create an internal property to lower an expression.

        For convenience, accept a null body expression: return None in that
        case (create no property).

        :param node: Node for which we want to create this property.
        :param name: Name prefix, used to generate the actual property name.
        :param expr: Body for this proprety.
        :param rtype: Return type for this property.
        """
        if expr is None:
            return None
        not_none_expr = expr

        def lower_expr(p: PropertyDef) -> AbstractExpression:
            expr = not_none_expr

            # If the body is a Lkt expression, lower it. Use it unchanged
            # otherwise.
            return (
                self.lower_expr(expr, self.root_scope, p.vars)
                if isinstance(expr, L.Expr) else
                expr
            )

        return self.create_internal_property(
            node,
            name,
            lower_expr,
            rtype,
            location=(
                Location.from_lkt_node(expr)
                if isinstance(expr, L.Expr) else
                Location.builtin
            )
        )

    def extract_call_args(
        self,
        expr: L.CallExpr,
    ) -> tuple[list[L.Expr], dict[str, L.Expr]]:
        """
        Extract positional and keyword arguments from a call expression.
        """
        args = []
        kwargs = {}
        for arg in expr.f_args:
            value = arg.f_value
            if arg.f_name:
                kwargs[arg.f_name.text] = value
            elif kwargs:
                with lkt_context(arg):
                    error(
                        "positional arguments are forbidden after the first"
                        " keyword argument"
                    )
            else:
                args.append(value)
        return args, kwargs

    def lower_call_args(
        self,
        expr: L.CallExpr,
        lower: Callable[[L.Expr], AbstractExpression],
    ) -> tuple[list[AbstractExpression], dict[str, AbstractExpression]]:
        """
        Collect call positional and keyword arguments.
        """
        arg_nodes, kwarg_nodes = self.extract_call_args(expr)
        args = [lower(v) for v in arg_nodes]
        kwargs = {k: lower(v) for k, v in kwarg_nodes.items()}
        return args, kwargs

    def lower_method_call(
        self,
        call_expr: L.CallExpr,
        env: Scope,
        local_vars: LocalVars | None,
    ) -> AbstractExpression:
        """
        Subroutine for "lower_expr": lower specifically a method call.

        :param call_expr: Method call to lower.
        :param env: Scope to use when resolving references.
        :param local_vars: If lowering a property expression, set of local
            variables for this property.
        """

        result: AbstractExpression

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Convenience wrapper around "self.lower_expr" to set the expression
            location.
            """
            with AbstractExpression.with_location(
                Location.from_lkt_node(expr)
            ):
                return self.lower_expr(expr, env, local_vars)

        def add_lambda_arg_to_scope(
            scope: Scope,
            arg: L.LambdaArgDecl,
            var: AbstractVariable
        ) -> None:
            """
            Helper to register a lambda expression argument in a scope.
            """
            scope.add(Scope.LocalVariable(arg.f_syn_name.text, arg, var))

        def append_lambda_arg_info(
            infos: list[E.LambdaArgInfo],
            arg: L.LambdaArgDecl,
            var: AbstractVariable,
        ) -> None:
            if arg.f_decl_type is not None:
                infos.append(
                    E.LambdaArgInfo(
                        var,
                        self.resolver.resolve_type(arg.f_decl_type, env),
                        Location.from_lkt_node(arg.f_decl_type),
                    )
                )

        def var_for_lambda_arg(
            scope: Scope,
            arg: L.LambdaArgDecl,
            infos: list[E.LambdaArgInfo],
            prefix: str,
            type: CompiledType | None = None,
            create_local: bool = False,
        ) -> AbstractVariable:
            """
            Create an AbstractVariable to translate a lambda argument.

            This also registers this decl/variable association in ``env``.

            :param scope: Scope in which to register this variable.
            :param arg: Lambda argument to lower.
            :param infos: List of lambda argument information in which to
                append information for this argument, in case the argument
                declaration contains a type annotation.
            :param prefix: Lower-case prefix for the name of the variable in
                the generated code.
            :param type: Optional type information to associate to this
                variable.
            :param create_local: See the corresponding AbstractVariable
                constructor argument.
            """
            source_name, _ = extract_var_name(self.ctx, arg.f_syn_name)
            with AbstractExpression.with_location(Location.from_lkt_node(arg)):
                result = AbstractVariable(
                    names.Name.from_lower(
                        f"{prefix}_{next(self.names_counter)}"
                    ),
                    source_name=source_name,
                    type=type,
                    create_local=create_local,
                )
            add_lambda_arg_to_scope(scope, arg, result)
            append_lambda_arg_info(infos, arg, result)
            return result

        def extract_lambda(
            expr: L.LambdaExpr,
            lambda_n_args: int,
        ) -> tuple[Scope, list[L.LambdaArgDecl], L.Expr]:
            """
            Extract arguments/expr from a lambda expression.

            :param expr: Lambda expression to analyze.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            actual_n_args = len(expr.f_params)
            with lkt_context(expr.f_params):
                check_source_language(
                    actual_n_args == lambda_n_args,
                    f"{lambda_n_args} arguments expected, got {actual_n_args}",
                )
            for larg in expr.f_params:
                with lkt_context(larg):
                    check_source_language(
                        larg.f_default_val is None,
                        "default values are not allowed here",
                    )

            loc = Location.from_lkt_node(expr)
            scope = env.create_child(
                f"scope for lambda expression at {loc.gnu_style_repr()}"
            )

            return (scope, list(expr.f_params), expr.f_body)

        def extract_lambda_and_kwargs(
            expr: L.CallExpr,
            signature: S.FunctionSignature,
            arg_for_lambda: str,
            lambda_n_args: int,
        ) -> LktTypesLoader.BuiltinCallInfo:
            """
            Extract arguments from a call expression, expecting the first
            positional argument to be a lambda expression.

            :param expr: Call expression that is supposed to pass the lambda
                expression.
            :param signature: Signature for the builtin function that is
                called.
            :param arg_for_lambda: Name of the argument in ``signature`` that
                must contain the lambda.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            # Make sure the only positional argument is a lambda expression
            args, _ = signature.match(self.ctx, expr)
            lambda_expr = args[arg_for_lambda]
            if not isinstance(lambda_expr, L.LambdaExpr):
                with lkt_context(lambda_expr):
                    error("lambda expression expected")

            # Extract info from the lambda expression itself
            scope, lambda_args, lambda_body = extract_lambda(
                lambda_expr, lambda_n_args
            )

            return LktTypesLoader.BuiltinCallInfo(
                args, scope, lambda_args, lambda_body
            )

        def lower_collection_iter(
            has_index: bool,
        ) -> LktTypesLoader.CollectionLoweringResult:
            """
            Helper to lower a method call that implements a collection
            iteration.

            This assumes that that ``call_expr`` is such a method call: the
            signature for this method is ``S.collection_iter_signature``, and
            its ``expr`` argument is expected to be a lambda function to
            process one collection element. That lambda function must accept
            the collection element itself only (if ``has_index`` is false) or
            an additional element index (if ``has_index`` is true).

            Return the lowered expression for the lambda, information for
            lambda args, the variable for the iteration element, and an
            optional variable for the iteration index.
            """
            # We expect a single argument: a lambda (itself taking the
            # collection element plus optionally its index).
            lambda_info = extract_lambda_and_kwargs(
                call_expr,
                S.collection_iter_signature,
                "expr",
                2 if has_index else 1
            )
            lambda_arg_infos: list[E.LambdaArgInfo] = []
            element_arg = lambda_info.largs[0]
            if has_index:
                index_arg = lambda_info.largs[1]

            # There is always an iteration variable for the collection element
            element_var = var_for_lambda_arg(
                lambda_info.scope, element_arg, lambda_arg_infos, 'item'
            )

            # The iteration variable for the iteration index is optional: we
            # create one only if the lambda has the corresponding element.
            index_var: AbstractVariable | None = None
            if has_index:
                index_var = var_for_lambda_arg(
                    lambda_info.scope,
                    index_arg,
                    lambda_arg_infos,
                    'index',
                    T.Int,
                )

            # Lower the body expression for that lambda
            inner_expr = self.lower_expr(
                lambda_info.expr, lambda_info.scope, local_vars
            )
            return LktTypesLoader.CollectionLoweringResult(
                inner_expr, lambda_arg_infos, element_var, index_var
            )

        def lower_node_builder(prefix: L.Expr) -> AbstractExpression:
            """
            Helper to lower the creation of a synthetizing node builder.

            :param prefix: Prefix for the ".builder()" method, i.e. the
                expected synthetic node type reference.
            """
            with lkt_context(prefix):
                if not isinstance(prefix, (L.DotExpr, L.TypeRef, L.RefId)):
                    error("Prefix for .builder expressions must be a node")

            node_type = self.resolver.resolve_node_type_expr(prefix, env)

            args, kwargs = self.lower_call_args(call_expr, lower)
            with lkt_context(call_expr.f_args):
                if len(args) != 0:
                    error("Positional arguments not allowed for .builder")

            return E.CreateSynthNodeBuilder(node_type, **kwargs)

        call_name = call_expr.f_name
        assert isinstance(call_name, L.BaseDotExpr)

        method_name = call_name.f_suffix.text

        # Handle node builder creation from node types
        if method_name == "builder":
            return lower_node_builder(call_name.f_prefix)

        # TODO (eng/libadalang/langkit#728): introduce a pre-lowering pass to
        # extract the list of types and their fields/methods so that we can
        # perform validation here.
        method_prefix = lower(call_name.f_prefix)

        # Add the right wrappers to handle null conditional constructs. Note
        # that anything going through "getattr" will take care of validating
        # Check and adding a Prefix wrapper: adjust wrappers accordingly for
        # them.
        getattr_prefix = method_prefix
        if isinstance(call_name, L.NullCondDottedName):
            getattr_prefix = NullCond.Check(getattr_prefix, validated=False)
            method_prefix = NullCond.Check(method_prefix, validated=True)
        method_prefix = NullCond.Prefix(method_prefix)

        # Make sure this is not an attempt to call a builin field
        try:
            BuiltinAttribute[method_name]
        except KeyError:
            pass
        else:
            with lkt_context(call_name.f_suffix):
                error("this is a builtin attribute, it should not be called")

        # Handle calls to builtin methods and regular properties separately
        try:
            builtin = BuiltinMethod[method_name]
        except KeyError:
            call_args, call_kwargs = self.lower_call_args(call_expr, lower)
            return E.FieldAccess(
                method_prefix,
                method_name,
                E.FieldAccess.Arguments(call_args, call_kwargs),
                check_call_syntax=True,
            )

        # Past this point, we know that this is a builtin method call
        if builtin in (
            BuiltinMethod.all,
            BuiltinMethod.any,
            BuiltinMethod.iall,
            BuiltinMethod.iany,
        ):
            clr = lower_collection_iter(
                has_index=builtin in (BuiltinMethod.iall, BuiltinMethod.iany),
            )
            result = E.Quantifier(
                (
                    "all"
                    if builtin in (BuiltinMethod.all, BuiltinMethod.iall) else
                    "any"
                ),
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
            )

        elif builtin == BuiltinMethod.append_rebinding:
            args, _ = S.append_rebinding_signature.match(self.ctx, call_expr)
            result = getattr_prefix.append_rebinding(
                lower(args["old_env"]), lower(args["new_env"])
            )

        elif builtin == BuiltinMethod.as_array:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.as_array

        elif builtin == BuiltinMethod.as_big_int:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.BigIntLiteral(method_prefix)

        elif builtin == BuiltinMethod.as_int:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.as_int

        elif builtin == BuiltinMethod.concat_rebindings:
            args, _ = S.concat_rebindings_signature.match(self.ctx, call_expr)
            result = getattr_prefix.concat_rebindings(
                lower(args["rebindings"])
            )

        elif builtin == BuiltinMethod.contains:
            args, _ = S.contains_signature.match(self.ctx, call_expr)
            result = E.Contains(getattr_prefix, lower(args["value"]))

        elif builtin == BuiltinMethod.do:
            lambda_info = extract_lambda_and_kwargs(
                call_expr, S.do_signature, "expr", 1
            )
            arg_node = lambda_info.largs[0]

            lambda_arg_infos: list[E.LambdaArgInfo] = []
            arg_var = var_for_lambda_arg(
                lambda_info.scope,
                arg_node,
                lambda_arg_infos,
                "var_expr",
                create_local=True,
            )
            then_expr = self.lower_expr(
                lambda_info.expr, lambda_info.scope, local_vars
            )

            default_val = (
                lower(lambda_info.kwargs["default_val"])
                if "default_val" in lambda_info.kwargs else
                None
            )

            result = E.Then(
                method_prefix,
                arg_var,
                lambda_arg_infos,
                then_expr,
                default_val,
            )

        elif builtin == BuiltinMethod.empty:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, "empty")

        elif builtin == BuiltinMethod.env_group:
            args, _ = S.env_group_signature.match(self.ctx, call_expr)
            with_md_expr = args.get("with_md")
            with_md = None if with_md_expr is None else lower(with_md_expr)
            result = getattr_prefix.env_group(with_md=with_md)

        elif builtin == BuiltinMethod.env_orphan:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.env_orphan

        elif builtin in (BuiltinMethod.filter, BuiltinMethod.ifilter):
            clr = lower_collection_iter(
                has_index=builtin == BuiltinMethod.ifilter
            )
            result = E.Map(
                kind=builtin.name,
                collection=method_prefix,
                expr=clr.element_var,
                lambda_arg_infos=clr.lambda_arg_infos,
                element_var=clr.element_var,
                index_var=clr.index_var,
                filter_expr=clr.inner_expr,
            )

        elif builtin in (BuiltinMethod.filtermap, BuiltinMethod.ifiltermap):
            has_index = builtin == BuiltinMethod.ifiltermap
            lambda_n_args = 2 if has_index else 1

            # Validate arguments for ".[i]filtermap()" itself
            args, _ = S.filtermap_signature.match(self.ctx, call_expr)
            for arg in [args["expr"], args["filter"]]:
                if not isinstance(arg, L.LambdaExpr):
                    with lkt_context(arg):
                        error("lambda expressions expceted")

            # Validate and analyze the two lambda expressions
            lambda_0 = args["expr"]
            assert isinstance(lambda_0, L.LambdaExpr)
            map_scope, map_args, map_body = extract_lambda(
                lambda_0, lambda_n_args
            )

            lambda_1 = args["filter"]
            assert isinstance(lambda_1, L.LambdaExpr)
            filter_scope, filter_args, filter_body = extract_lambda(
                lambda_1, lambda_n_args
            )

            # We need to have two different scopes for the two lambda
            # expressions, but need to create common iteration variables for
            # both.
            lambda_arg_infos = []
            element_var = var_for_lambda_arg(
                map_scope,
                map_args[0],
                lambda_arg_infos,
                "item",
            )
            name_from_lower(self.ctx, "argument", filter_args[0].f_syn_name)
            add_lambda_arg_to_scope(filter_scope, filter_args[0], element_var)
            append_lambda_arg_info(
                lambda_arg_infos, filter_args[0], element_var
            )

            index_var: AbstractVariable | None = None
            if has_index:
                index_var = var_for_lambda_arg(
                    map_scope, map_args[1], lambda_arg_infos, "index", T.Int
                )
                name_from_lower(
                    self.ctx, "argument", filter_args[1].f_syn_name
                )
                add_lambda_arg_to_scope(
                    filter_scope, filter_args[1], index_var
                )
                append_lambda_arg_info(
                    lambda_arg_infos, filter_args[1], index_var
                )

            # Lower their expressions
            map_expr = self.lower_expr(map_body, map_scope, local_vars)
            filter_expr = self.lower_expr(
                filter_body, filter_scope, local_vars
            )

            return E.Map(
                kind=builtin.name,
                collection=method_prefix,
                expr=map_expr,
                lambda_arg_infos=lambda_arg_infos,
                element_var=element_var,
                index_var=index_var,
                filter_expr=filter_expr,
            )

        elif builtin == BuiltinMethod.find:
            lambda_info = extract_lambda_and_kwargs(
                call_expr, S.collection_iter_signature, "expr", 1
            )
            elt_arg = lambda_info.largs[0]

            lambda_arg_infos = []
            elt_var = var_for_lambda_arg(
                lambda_info.scope,
                elt_arg,
                lambda_arg_infos,
                'item',
            )
            inner_expr = self.lower_expr(
                lambda_info.expr, lambda_info.scope, local_vars
            )

            result = E.Find(
                method_prefix,
                inner_expr,
                lambda_arg_infos,
                elt_var,
                index_var=None,
            )
        elif builtin in (BuiltinMethod.get, BuiltinMethod.get_first):
            args, _ = S.get_signature.match(self.ctx, call_expr)
            symbol = lower(args["symbol"])

            lookup_expr = args.get("lookup")
            lookup: AbstractExpression | None = (
                None if lookup_expr is None else lower(lookup_expr)
            )

            from_node_expr = args.get("from")
            from_node: AbstractExpression | None = (
                None if from_node_expr is None else lower(from_node_expr)
            )

            categories_expr = args.get("categories")
            categories: AbstractExpression | None = (
                None if categories_expr is None else lower(categories_expr)
            )

            return (
                getattr_prefix.get(symbol, lookup, from_node, categories)
                if method_name == "get" else
                getattr_prefix.get_first(symbol, lookup, from_node, categories)
            )

        elif builtin == BuiltinMethod.get_value:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.get_value

        elif builtin == BuiltinMethod.is_visible_from:
            args, _ = S.is_visible_from_signature.match(self.ctx, call_expr)
            result = getattr_prefix.is_visible_from(lower(args["unit"]))

        elif builtin == BuiltinMethod.join:
            args, _ = S.join_signature.match(self.ctx, call_expr)
            result = getattr_prefix.join(lower(args["strings"]))

        elif builtin == BuiltinMethod.length:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, "length")

        elif builtin in (
            BuiltinMethod.ilogic_all,
            BuiltinMethod.ilogic_any,
            BuiltinMethod.logic_all,
            BuiltinMethod.logic_any,
        ):
            import langkit.expressions.logic as LE

            has_index = builtin in (
                BuiltinMethod.ilogic_all, BuiltinMethod.ilogic_any
            )
            is_all = builtin in (
                BuiltinMethod.ilogic_all, BuiltinMethod.logic_all
            )

            clr = lower_collection_iter(has_index=has_index)
            map_expr = E.Map(
                builtin.name,
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
            )
            result = (
                LE.All(map_expr)
                if is_all else
                LE.Any(map_expr)
            )

        elif builtin in (
            BuiltinMethod.imap,
            BuiltinMethod.imapcat,
            BuiltinMethod.map,
            BuiltinMethod.mapcat,
        ):
            clr = lower_collection_iter(
                has_index=builtin in (
                    BuiltinMethod.imap, BuiltinMethod.imapcat
                )
            )
            result = E.Map(
                builtin.name,
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                do_concat=builtin in (
                    BuiltinMethod.mapcat, BuiltinMethod.imapcat
                ),
            )

        elif builtin == BuiltinMethod.rebind_env:
            args, _ = S.rebind_env_signature.match(self.ctx, call_expr)
            result = getattr_prefix.rebind_env(lower(args["env"]))

        elif builtin == BuiltinMethod.singleton:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, "singleton")

        elif builtin == BuiltinMethod.shed_rebindings:
            args, _ = S.shed_rebindings_signature.match(self.ctx, call_expr)
            result = getattr_prefix.shed_rebindings(lower(args["entity_info"]))

        elif builtin in (
            BuiltinMethod.solve, BuiltinMethod.solve_with_diagnostics
        ):
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, method_name)

        elif builtin == BuiltinMethod.super:
            call_args, call_kwargs = self.lower_call_args(call_expr, lower)
            result = E.Super(method_prefix, *call_args, **call_kwargs)

        elif builtin in (
            BuiltinMethod.itake_while, BuiltinMethod.take_while
        ):
            clr = lower_collection_iter(
                has_index=builtin == BuiltinMethod.itake_while
            )
            result = E.Map(
                builtin.name,
                method_prefix,
                clr.element_var,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                take_while_expr=clr.inner_expr,
            )

        elif builtin == BuiltinMethod.to_builder:
            S.empty_signature.match(self.ctx, call_expr)
            result = E.CreateCopyNodeBuilder(method_prefix)

        elif builtin == BuiltinMethod.unique:
            S.empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.unique

        elif builtin == BuiltinMethod.update:
            arg_nodes, kwarg_nodes = self.extract_call_args(call_expr)
            if arg_nodes:
                error(
                    ".update() accepts keyword arguments only",
                    location=arg_nodes[0],
                )
            field_exprs = {k: lower(v) for k, v in kwarg_nodes.items()}
            result = E.StructUpdate(method_prefix, **field_exprs)

        else:
            assert False, f"unhandled builitn call: {call_name.f_suffix}"

        return result

    def lower_expr(self,
                   expr: L.Expr,
                   env: Scope,
                   local_vars: LocalVars | None,
                   static_required: bool = False) -> AbstractExpression:
        """
        Lower the given expression.

        :param expr: Expression to lower.
        :param env: Scope to use when resolving references.
        :param local_vars: If lowering a property expression, set of local
            variables for this property.
        :param static_required: Whether "expr" is required to be a static
            expression.
        """

        def abort_if_static_required(expr: L.Expr) -> None:
            """
            Abort lowering if a static expression is required for "expr".
            """
            if static_required:
                with lkt_context(expr):
                    error("static expression expected in this context")

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Wrapper around "_lower" to set the expression location.

            Calling this function instead of ``_lower`` below to lower
            individual expression nodes is what correctly assigns the Lkt
            location to each instantiated ``AbstractExpression``.
            """
            with AbstractExpression.with_location(
                Location.from_lkt_node(expr)
            ):
                return _lower(expr)

        def _lower(expr: L.Expr) -> AbstractExpression:
            """
            Do the actual expression lowering. Since all recursive calls use
            the same environment, this helper allows to skip passing it.
            """
            result: AbstractExpression

            if isinstance(expr, L.AnyOf):
                abort_if_static_required(expr)

                prefix = lower(expr.f_expr)
                return E.AnyOf(
                    lower(expr.f_expr),
                    *[lower(v) for v in expr.f_values],
                )

            elif isinstance(expr, L.ArrayLiteral):
                abort_if_static_required(expr)

                elts = [lower(e) for e in expr.f_exprs]
                element_type = (
                    None
                    if expr.f_element_type is None else
                    self.resolver.resolve_type(expr.f_element_type, env)
                )
                return E.ArrayLiteral(elts, element_type=element_type)

            elif isinstance(expr, L.BigNumLit):
                abort_if_static_required(expr)

                text = expr.text
                assert text[-1] == 'b'
                return E.BigIntLiteral(int(text[:-1]))

            elif isinstance(expr, L.BinOp):
                abort_if_static_required(expr)

                # Lower both operands
                left = lower(expr.f_left)
                right = lower(expr.f_right)

                # Dispatch to the appropriate abstract expression constructor
                if isinstance(expr.f_op, L.OpEq):
                    return E.Eq(left, right)

                elif isinstance(expr.f_op, L.OpNe):
                    return E.Not(E.Eq(left, right))

                elif isinstance(expr.f_op, (L.OpLt, L.OpGt, L.OpLte, L.OpGte)):
                    operator = {
                        L.OpLt: E.OrderingTest.LT,
                        L.OpLte: E.OrderingTest.LE,
                        L.OpGt: E.OrderingTest.GT,
                        L.OpGte: E.OrderingTest.GE,
                    }[type(expr.f_op)]
                    return E.OrderingTest(operator, left, right)

                elif isinstance(expr.f_op, L.OpAnd):
                    return E.BooleanBinaryOp(E.BinaryOpKind.AND, left, right)

                elif isinstance(expr.f_op, L.OpOr):
                    return E.BooleanBinaryOp(E.BinaryOpKind.OR, left, right)

                elif isinstance(expr.f_op, L.OpLogicAnd):
                    return E.LogicBinaryOp(E.BinaryOpKind.AND, left, right)

                elif isinstance(expr.f_op, L.OpLogicOr):
                    return E.LogicBinaryOp(E.BinaryOpKind.OR, left, right)

                elif isinstance(expr.f_op, L.OpOrInt):
                    # Create a variable to store the evaluation of the left
                    # operand, then use a Then construct to conditionally
                    # evaluate (and return) the right operand if the left one
                    # turns out to be null.
                    left_var = E.AbstractVariable(
                        names.Name("Left_Var"), create_local=True
                    )
                    return E.Then(
                        base=left,
                        var_expr=left_var,
                        lambda_arg_infos=[],
                        then_expr=left_var,
                        default_expr=right,
                    )

                else:
                    operator = {
                        L.OpAmp: '&',
                        L.OpPlus: '+',
                        L.OpMinus: '-',
                        L.OpMult: '*',
                        L.OpDiv: '/',
                    }[type(expr.f_op)]
                    return E.Arithmetic(left, right, operator)

            elif isinstance(expr, L.BlockExpr):
                abort_if_static_required(expr)

                assert local_vars is not None
                loc = Location.from_lkt_node(expr)
                sub_env = env.create_child(
                    f"scope for block at {loc.gnu_style_repr()}"
                )

                actions: list[LktTypesLoader.DeclAction] = []

                for v in expr.f_val_defs:
                    var: AbstractVariable
                    init_abstract_expr: L.Expr
                    scope_var: Scope.UserValue

                    if isinstance(v, L.ValDecl):
                        # Create the AbstractVariable for this declaration
                        source_name = v.f_syn_name.text
                        source_name, v_name = extract_var_name(
                            self.ctx, v.f_syn_name
                        )
                        v_type = (
                            self.resolver.resolve_type(v.f_decl_type, env)
                            if v.f_decl_type else
                            None
                        )
                        with AbstractExpression.with_location(
                            Location.from_lkt_node(v)
                        ):
                            var = AbstractVariable(
                                v_name,
                                v_type,
                                create_local=True,
                                source_name=source_name,
                            )
                        init_abstract_expr = v.f_expr
                        scope_var = Scope.LocalVariable(source_name, v, var)

                    elif isinstance(v, L.VarBind):
                        # Look for the corresponding dynamic variable, either
                        # unbound (BuiltinDynVar or DynVar, that we will bound)
                        # or already bounded (BoundDynVar, that we will rebind
                        # in this scope).
                        entity = self.resolver.resolve_entity(
                            v.f_name, sub_env
                        )
                        if not isinstance(
                            entity,
                            (
                                Scope.BuiltinDynVar,
                                Scope.DynVar,
                                Scope.BoundDynVar,
                            ),
                        ):
                            with lkt_context(v.f_name):
                                error(
                                    "dynamic variable expected, got"
                                    f" {entity.diagnostic_name}"
                                )

                        var = entity.variable
                        init_abstract_expr = v.f_expr
                        scope_var = Scope.BoundDynVar(v.f_name.text, v, var)

                    else:
                        assert False, f'Unhandled def in BlockExpr: {v}'

                    # Lower the declaration/bind initialization expression
                    init_expr = self.lower_expr(
                        init_abstract_expr, sub_env, local_vars
                    )

                    # Make the declared value/dynamic variable available to the
                    # remaining expressions.
                    sub_env.add(scope_var)
                    actions.append(
                        LktTypesLoader.DeclAction(
                            var, init_expr, Location.from_lkt_node(v)
                        )
                    )

                # Lower the block main expression and wrap it in declarative
                # blocks.
                result = self.lower_expr(expr.f_expr, sub_env, local_vars)
                for action in reversed(actions):
                    with AbstractExpression.with_location(action.location):
                        if isinstance(action.var, E.DynamicVariable):
                            result = getattr(action.var, "bind")(
                                action.init_expr, result
                            )
                        else:
                            result = Let(
                                [(action.var, action.init_expr)], result
                            )
                return result

            elif isinstance(expr, L.CallExpr):
                call_expr = expr
                call_name = call_expr.f_name

                def lower_new(t: CompiledType) -> AbstractExpression:
                    """
                    Consider that this call creates a new struct, return the
                    corresponding New expression.
                    """
                    # Non-struct/node types have their own constructor
                    if t == T.RefCategories:
                        arg_nodes, kwarg_nodes = self.extract_call_args(
                            call_expr
                        )
                        if arg_nodes:
                            error(
                                "Positional arguments not allowed for"
                                " RefCategories",
                                location=call_expr,
                            )

                        default_expr = kwarg_nodes.pop("_", None)
                        enabled_categories = {
                            k: parse_static_bool(self.ctx, v)
                            for k, v in kwarg_nodes.items()
                        }
                        return E.RefCategories(
                            default=(
                                False
                                if default_expr is None else
                                parse_static_bool(self.ctx, default_expr)
                            ),
                            **enabled_categories,
                        )
                    else:
                        abort_if_static_required(expr)

                        args, kwargs = self.lower_call_args(call_expr, lower)
                        if args:
                            error(
                                "Positional arguments not allowed for struct"
                                " constructors",
                                location=call_expr,
                            )
                        return E.New(t, **kwargs)

                # Depending on its name, a call can have different meanings...

                # If it is a simple identifier...
                if isinstance(call_name, L.RefId):
                    entity = self.resolver.resolve_entity(call_name, env)

                    # It can be a call to a built-in function
                    if entity == self.dynamic_lexical_env:
                        abort_if_static_required(expr)

                        args, _ = S.dynamic_lexical_env_signature.match(
                            self.ctx, call_expr
                        )
                        trans_parent_expr = args.get("transitive_parent")
                        return E.DynamicLexicalEnv(
                            assocs_getter=self.resolver.resolve_property(
                                args["assocs"]
                            ),
                            assoc_resolver=self.resolver.resolve_property(
                                args.get("assoc_resolver")
                            ),
                            transitive_parent=(
                                E.Literal(True)
                                if trans_parent_expr is None else
                                lower(trans_parent_expr)
                            ),
                        )

                    # It can be a New expression
                    elif isinstance(
                        entity, (Scope.BuiltinType, Scope.UserType)
                    ):
                        return lower_new(entity.t)

                    # Everything else is illegal
                    with lkt_context(call_name):
                        error("invalid call prefix")

                # If the call name is a generic instantiation, it has to be a
                # reference to a struct type, and thus the call is a New
                # expression.
                elif isinstance(call_name, L.GenericInstantiation):
                    abort_if_static_required(expr)

                    generic = self.resolver.resolve_generic(
                        call_name.f_name, env
                    )
                    type_args = call_name.f_args
                    if generic != self.generics.entity:
                        error(
                            f"only {self.generics.entity.name} is the only"
                            " legal generic in this context"
                        )
                    with lkt_context(type_args):
                        check_source_language(
                            len(type_args) == 1,
                            f"{generic.name} expects one type argument:"
                            " the node type"
                        )

                    node_arg = type_args[0]
                    node_type = self.resolver.resolve_node(node_arg, env)
                    return lower_new(node_type.entity)

                # Otherwise the call has to be a dot expression, for a method
                # invocation.
                elif not isinstance(call_name, L.BaseDotExpr):
                    with lkt_context(call_name):
                        error("invalid call prefix")

                abort_if_static_required(expr)

                return self.lower_method_call(call_expr, env, local_vars)

            elif isinstance(expr, L.CastExpr):
                abort_if_static_required(expr)

                subexpr = lower(expr.f_expr)
                excludes_null = expr.f_excludes_null.p_as_bool
                dest_type = self.resolver.resolve_type(expr.f_dest_type, env)
                return Cast(subexpr, dest_type, do_raise=excludes_null)

            elif isinstance(expr, L.CharLit):
                return E.CharacterLiteral(denoted_char(expr))

            elif isinstance(expr, L.BaseDotExpr):
                null_cond = isinstance(expr, L.NullCondDottedName)

                # Dotted expressions can designate an enum value (if the prefix
                # is a type name) or a member access.
                prefix_node = expr.f_prefix
                if isinstance(prefix_node, L.RefId):
                    try:
                        entity = env.lookup(prefix_node.text)
                    except KeyError:
                        pass
                    else:
                        if isinstance(
                            entity, (Scope.BuiltinType, Scope.UserType)
                        ):
                            check_source_language(
                                not null_cond,
                                "null-conditional dotted name notation is"
                                " illegal to designate an enum value",
                                location=expr.f_suffix,
                            )

                            # The suffix refers to the declaration of an enum
                            # value: the prefix must designate the
                            # corresponding enum type.
                            if not isinstance(entity.t, EnumType):
                                error(
                                    "enum type expected",
                                    location=expr.f_prefix,
                                )
                            try:
                                return entity.t.resolve_value(
                                    expr.f_suffix.text
                                )
                            except KeyError:
                                error(
                                    "no such enum value",
                                    location=expr.f_suffix,
                                )

                # Otherwise, the prefix is a regular expression, so this dotted
                # expression is an access to a member.
                abort_if_static_required(expr)

                prefix = lower(expr.f_prefix)
                suffix = expr.f_suffix.text

                # Make sure this is not an attempt to access a builtin method
                try:
                    BuiltinMethod[suffix]
                except KeyError:
                    pass
                else:
                    with lkt_context(expr.f_suffix):
                        error("this is a builtin method, it should be called")

                # Handle accesses to builtin attributes and regular field
                # access separately.
                #
                # In both cases, add the NullCond.Check wrapper when needed. In
                # the case of builtin attributes, getattr will take care of
                # introducing the NullCheck.Prefix wrapper and validating the
                # .Check one.
                try:
                    BuiltinAttribute[suffix]
                except KeyError:
                    if null_cond:
                        prefix = NullCond.Check(prefix, validated=True)
                    return E.FieldAccess(
                        NullCond.Prefix(prefix),
                        suffix,
                        check_call_syntax=True,
                    )
                else:
                    if null_cond:
                        prefix = NullCond.Check(prefix, validated=False)
                    return getattr(prefix, suffix)

            elif isinstance(expr, L.IfExpr):
                abort_if_static_required(expr)

                # We want to turn the following pattern::
                #
                #   IfExpr(C1, E1, [(C2, E2), (C3, E3), ...], E_last)
                #
                # into the following expression tree::
                #
                #   If(C1, E1,
                #      If(C2, E2,
                #         If(C3, E3,
                #            ... E_Last)))
                #
                # so first translate the "else" expression (E_last), then
                # reverse iterate on the alternatives to wrap this expression
                # with the conditional checks.
                result = lower(expr.f_else_expr)
                conditions = (
                    [(expr.f_cond_expr, expr.f_then_expr)]
                    + [
                        (alt.f_cond_expr, alt.f_then_expr)
                        for alt in expr.f_alternatives
                    ]
                )
                for c, e in reversed(conditions):
                    result = E.If(lower(c), lower(e), result)
                return result

            elif isinstance(expr, L.Isa):
                abort_if_static_required(expr)

                subexpr = lower(expr.f_expr)
                nodes = [
                    self.resolver.resolve_type(type_ref, env)
                    for type_ref in expr.f_dest_type
                ]
                return E.IsA(subexpr, *nodes)

            elif isinstance(expr, L.LogicAssign):
                dest_var = lower(expr.f_dest_var)
                value_expr = lower(expr.f_value)
                return E.Bind(
                    dest_var,
                    value_expr,
                    logic_ctx=self.logic_context_builtin.variable,
                    kind=E.BindKind.assign,
                )

            elif isinstance(expr, L.LogicExpr):
                abort_if_static_required(expr)

                logic_expr = expr
                expr = expr.f_expr
                if isinstance(expr, L.RefId):
                    if expr.text == "true":
                        return E.LogicTrue()
                    elif expr.text == "false":
                        return E.LogicFalse()

                elif isinstance(expr, L.CallExpr):
                    call_name = expr.f_name
                    if not isinstance(call_name, L.RefId):
                        with lkt_context(expr):
                            error("invalid logic expression")

                    if call_name.text in ("all", "any"):
                        _, vargs = S.logic_all_any_signature.match(
                            self.ctx, expr
                        )
                        op_kind = (
                            E.BinaryOpKind.AND
                            if call_name.text == "all" else
                            E.BinaryOpKind.OR
                        )
                        with lkt_context(logic_expr):
                            check_source_language(
                                bool(vargs), "at least one equation expected"
                            )
                        return reduce(
                            lambda lhs, rhs: E.LogicBinaryOp(
                                op_kind, lhs, rhs
                            ),
                            [lower(a) for a in vargs]
                        )

                    elif call_name.text == "domain":
                        args, _ = S.domain_signature.match(self.ctx, expr)
                        logic_var = lower(args["var"])
                        domain_expr = lower(args["domain"])
                        return logic_var.domain(domain_expr)

                with lkt_context(expr):
                    error("invalid logic expression")

            elif isinstance(expr, L.LogicPredicate):
                pred_prop = self.resolver.resolve_property(expr.f_name)
                arg_exprs = [lower(arg.f_value) for arg in expr.f_args]
                if len(arg_exprs) == 0:
                    with lkt_context(expr.f_args):
                        error("at least one argument expected")
                node_expr = arg_exprs.pop(0)
                for arg in expr.f_args:
                    if arg.f_name is not None:
                        with lkt_context(arg.f_name):
                            error(
                                "parameter names are not allowed in logic"
                                " propagates"
                            )
                return E.Predicate(
                    pred_prop,
                    self.error_location_builtin.variable,
                    node_expr,
                    *arg_exprs,
                )

            elif isinstance(expr, L.LogicPropagate):
                dest_var = lower(expr.f_dest_var)
                comb_prop = self.resolver.resolve_property(expr.f_call.f_name)
                arg_vars = [lower(arg.f_value) for arg in expr.f_call.f_args]
                for arg in expr.f_call.f_args:
                    if arg.f_name is not None:
                        with lkt_context(arg.f_name):
                            error(
                                "parameter names are not allowed in logic"
                                " propagates"
                            )
                return E.NPropagate(
                    dest_var,
                    comb_prop,
                    *arg_vars,
                    logic_ctx=self.logic_context_builtin.variable,
                )

            elif isinstance(expr, L.LogicUnify):
                lhs_var = lower(expr.f_lhs)
                rhs_var = lower(expr.f_rhs)
                return E.Bind(
                    lhs_var,
                    rhs_var,
                    logic_ctx=self.logic_context_builtin.variable,
                    kind=E.BindKind.unify,
                )

            elif isinstance(expr, L.KeepExpr):
                abort_if_static_required(expr)

                subexpr = lower(expr.f_expr)
                keep_type = self.resolver.resolve_type(expr.f_keep_type, env)
                iter_var = E.Map.create_iteration_var(
                    existing_var=None, name_prefix="Item"
                )
                return E.Map(
                    kind="keep",
                    collection=subexpr,
                    expr=iter_var.cast(keep_type),
                    lambda_arg_infos=[],
                    element_var=iter_var,
                    filter_expr=iter_var.is_a(keep_type),
                )

            elif isinstance(expr, L.MatchExpr):
                abort_if_static_required(expr)
                assert local_vars is not None

                prefix_expr = lower(expr.f_match_expr)

                # Lower each individual matcher
                matchers: list[
                    tuple[
                        CompiledType | None,
                        AbstractVariable,
                        AbstractExpression,
                    ]
                ] = []
                for i, m in enumerate(expr.f_branches):
                    # Make sure the identifier has the expected casing
                    decl_id = m.f_decl.f_syn_name
                    if decl_id.text != "_":
                        with lkt_context(decl_id):
                            names.Name.check_from_lower(decl_id.text)

                    # Fetch the type to match, if any
                    syn_type = m.f_decl.f_decl_type
                    matched_type = (
                        None
                        if syn_type is None else
                        self.resolver.resolve_type(syn_type, env)
                    )

                    # Create the match variable
                    var_name = names.Name(f"Match_{i}")
                    with AbstractExpression.with_location(
                        Location.from_lkt_node(m.f_decl)
                    ):
                        match_var = AbstractVariable(
                            name=var_name,
                            type=matched_type,
                            source_name=decl_id.text,
                        )
                    match_var.create_local_variable()

                    # Lower the matcher expression, making the match variable
                    # available if intended.
                    loc = Location.from_lkt_node(m)
                    sub_env = env.create_child(
                        f"scope for match branch at {loc.gnu_style_repr()}"
                    )
                    if decl_id.text != "_":
                        sub_env.add(
                            Scope.UserValue(decl_id.text, m.f_decl, match_var)
                        )
                    match_expr = self.lower_expr(m.f_expr, sub_env, local_vars)

                    matchers.append((matched_type, match_var, match_expr))

                return E.Match(prefix_expr, matchers)

            elif isinstance(expr, L.NotExpr):
                abort_if_static_required(expr)

                return E.Not(lower(expr.f_expr))

            elif isinstance(expr, L.NullLit):
                result_type = self.resolver.resolve_type(expr.f_dest_type, env)
                return E.No(result_type)

            elif isinstance(expr, L.NumLit):
                return E.Literal(int(expr.text))

            elif isinstance(expr, L.ParenExpr):
                return E.Paren(lower(expr.f_expr))

            elif isinstance(expr, L.RaiseExpr):
                abort_if_static_required(expr)

                # A raise expression can only contain a PropertyError struct
                # constructor.
                cons_expr = expr.f_except_expr
                if not isinstance(cons_expr, L.CallExpr):
                    error("'raise' must be followed by a call expression")
                call_name = cons_expr.f_name
                entity = self.resolver.resolve_entity(call_name, env)
                if not isinstance(entity, Scope.Exception):
                    error(f"exception expected, got {entity.diagnostic_name}")

                # Get the exception message argument
                args_nodes, kwargs_nodes = self.extract_call_args(cons_expr)
                msg_expr: L.Expr | None = None
                if args_nodes:
                    msg_expr = args_nodes.pop()
                elif kwargs_nodes:
                    msg_expr = kwargs_nodes.pop("exception_message")
                with lkt_context(cons_expr.f_args):
                    check_source_language(
                        not args_nodes and not kwargs_nodes,
                        "at most one argument expected: the exception message",
                    )

                if msg_expr is None:
                    msg = "PropertyError exception"
                else:
                    # TODO (S321-013): handle dynamic error message
                    msg = parse_static_str(self.ctx, msg_expr)

                return entity.constructor(
                    self.resolver.resolve_type(expr.f_dest_type, env), msg
                )

            elif isinstance(expr, L.RefId):
                entity = self.resolver.resolve_entity(expr, env)
                if isinstance(entity, Scope.BuiltinValue):
                    if not isinstance(entity.value, E.Literal):
                        abort_if_static_required(expr)

                    result = E.Ref(entity.value)
                elif isinstance(entity, Scope.UserValue):
                    abort_if_static_required(expr)
                    result = E.Ref(entity.variable)
                else:
                    with lkt_context(expr):
                        if isinstance(entity, Scope.DynVar):
                            error(
                                f"{entity.name} is not bound in this context:"
                                " please use the 'bind' construct to bind is"
                                " first."
                            )
                        else:
                            error(
                                f"value expected, got {entity.diagnostic_name}"
                            )
                return result

            elif isinstance(expr, L.StringLit):
                abort_if_static_required(expr)

                string_prefix = expr.p_prefix
                string_value = denoted_str(expr)
                if string_prefix == "\x00":
                    return E.String(string_value)
                elif string_prefix == "s":
                    return E.SymbolLiteral(string_value)
                else:
                    error("invalid string prefix")

            elif isinstance(expr, L.SubscriptExpr):
                abort_if_static_required(expr)

                null_cond = isinstance(expr, L.NullCondSubscriptExpr)
                prefix = lower(expr.f_prefix)
                index = lower(expr.f_index)
                return (
                    prefix.at(index)
                    if isinstance(expr, L.NullCondSubscriptExpr) else
                    prefix.at_or_raise(index)
                )

            elif isinstance(expr, L.TryExpr):
                abort_if_static_required(expr)

                return E.Try(
                    try_expr=lower(expr.f_try_expr),
                    else_expr=(
                        None
                        if expr.f_or_expr is None
                        else lower(expr.f_or_expr)
                    ),
                )

            elif isinstance(expr, L.UnOp):
                assert isinstance(expr.f_op, L.OpMinus)
                return E.UnaryNeg(lower(expr.f_expr))

            else:
                assert False, 'Unhandled expression: {}'.format(expr)

        return lower(expr)

    def lower_property_arguments(
        self,
        prop: PropertyDef,
        arg_decl_list: L.FunArgDeclList | None,
        label: str,
    ) -> tuple[list[L.FunArgDecl], Scope]:
        """
        Lower a property's arguments and create the root scope used to lower
        the property's root expression.
        """
        arguments: list[L.FunArgDecl] = []
        scope = self.root_scope.create_child(f"scope for {label}")

        # Lower arguments and register them both in the property's argument
        # list and in the root property scope.
        for a in arg_decl_list or []:
            arguments.append(a)

            annotations = parse_annotations(
                self.ctx,
                FunArgAnnotations,
                a.f_decl_annotations,
                self.root_scope,
            )

            source_name = a.f_syn_name.text
            reserved = PropertyDef.reserved_arg_lower_names
            with lkt_context(a.f_syn_name):
                check_source_language(
                    source_name not in reserved,
                    "Arguments cannot have reserved names ({})".format(
                        ", ".join(reserved)
                    ),
                )
            with AbstractExpression.with_location(Location.from_lkt_node(a)):
                arg = Argument(
                    name=name_from_lower(self.ctx, "argument", a.f_syn_name),
                    type=self.resolver.resolve_type(a.f_decl_type, scope),
                    source_name=source_name,
                )
            if annotations.ignored:
                arg.var.tag_ignored()
            prop.append_argument(arg)
            scope.add(Scope.Argument(source_name, a, arg.var))

        return arguments, scope

    def lower_property(
        self,
        owner: CompiledType,
        full_decl: L.FullDecl,
    ) -> PropertyDef:
        """
        Lower the property described in ``decl``.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FunDecl)
        annotations = parse_annotations(
            self.ctx, FunAnnotations, full_decl, self.root_scope,
        )
        return_type = self.resolver.resolve_type(
            decl.f_return_type, self.root_scope
        )

        external = False
        uses_entity_info: bool | None = None
        uses_envs: bool | None = None
        if annotations.external is not None:
            external = True
            uses_entity_info = annotations.external.uses_entity_info
            uses_envs = annotations.external.uses_envs

        # Create the property to return
        result = PropertyDef(
            names=MemberNames.for_property(
                owner,
                name_from_lower(self.ctx, "field", decl.f_syn_name),
            ),
            expr=None,
            doc=lkt_doc(decl),

            # When the @export annotation is missing, use "None" to mean
            # "public status unspecified", as the property can still be public
            # thanks to inheritance.
            public=annotations.exported or None,

            abstract=annotations.abstract,
            type=return_type,
            abstract_runtime_check=False,
            memoized=annotations.memoized,
            call_memoizable=annotations.call_memoizable,
            memoize_in_populate=False,
            external=external,
            uses_entity_info=uses_entity_info,
            uses_envs=uses_envs,
            optional_entity_info=False,

            # When the @ignored annotation is missing, use "None" to mean
            # "same as from base node".
            warn_on_unused=not annotations.ignored and None,

            call_non_memoizable_because=(
                annotations.call_non_memoizable_because
            ),
            activate_tracing=annotations.traced,
            dump_ir=False,
            lazy_field=False,
            final=annotations.final,
            predicate_error=annotations.predicate_error,
            has_property_syntax=annotations.property,
        )
        result.location = Location.from_lkt_node(decl)
        result._doc_location = Location.from_lkt_node_or_none(full_decl.f_doc)

        # If this property implements a generic interface method, keep track of
        # it: generic interface methods declarations are not lowered yet.
        if decl.f_trait_ref is not None:
            self.set_implemented_method(result, decl.f_trait_ref)

        # Lower its arguments
        arguments, scope = self.lower_property_arguments(
            result, decl.f_args, f"property {result.qualname}"
        )
        if annotations.property and arguments:
            error(
                "the @property annotation is valid only for properties with no"
                " argument"
            )

        # Keep track of the requested set of dynamic variables
        dynvars: list[
            tuple[E.DynamicVariable, L.Expr | None]
        ] | None = None
        if annotations.with_dynvars is not None:
            dynvars = []
            for dynvar, init_expr in annotations.with_dynvars:
                dynvars.append((dynvar.variable, init_expr))
                diag_node = (
                    dynvar.diagnostic_node
                    if isinstance(dynvar, Scope.DynVar) else
                    decl
                )
                scope.add(
                    Scope.BoundDynVar(dynvar.name, diag_node, dynvar.variable)
                )

        # Plan to lower its expressions later
        self.properties_to_lower.append(
            self.PropertyToLower(result, arguments, dynvars)
            if decl.f_body is None else
            self.PropertyAndExprToLower(
                result, arguments, dynvars, decl.f_body, scope
            )
        )

        return result

    def lower_env_spec(
        self,
        node: ASTNodeType,
        env_spec: L.EnvSpecDecl,
    ) -> EnvSpec:
        """
        Lower an env spec for a node.

        :param node: Node for which we want to lower the env spec.
        :param env_spec: Env spec to lower.
        """
        actions = []

        for syn_action in env_spec.f_actions:
            location = Location.from_lkt_node(syn_action)
            assert isinstance(syn_action.f_name, L.RefId)
            action_kind = syn_action.f_name.text
            action: EnvAction
            if action_kind == "add_env":
                args, _ = S.add_env_signature.match(self.ctx, syn_action)
                action = AddEnv(
                    no_parent=(
                        parse_static_bool(self.ctx, args["no_parent"])
                        if "no_parent" in args else
                        False
                    ),
                    transitive_parent=self.lower_expr_to_internal_property(
                        node,
                        "env_trans_parent",
                        args.get(
                            "transitive_parent"
                        ) or E.Literal(False),
                        T.Bool,
                    ),
                    names=self.lower_expr_to_internal_property(
                        node,
                        "env_names",
                        args.get("names"),
                        T.Symbol.array,
                    ),
                    location=location,
                )

            elif action_kind == "add_to_env_kv":
                args, _ = S.add_to_env_kv_signature.match(self.ctx, syn_action)

                def lower_expr(
                    p: PropertyDef,
                    e: L.Expr,
                ) -> AbstractExpression:
                    """
                    Shortcut for ``self.lower_expr``.
                    """
                    return self.lower_expr(e, self.root_scope, p.vars)

                def lower_prop_expr(p: PropertyDef) -> AbstractExpression:
                    """
                    Lower the body expression of the "mappings" internal
                    property.
                    """
                    return E.New(
                        T.EnvAssoc,
                        key=lower_expr(p, args["key"]),
                        value=lower_expr(p, args["value"]),
                        dest_env=(
                            lower_expr(p, args["dest_env"])
                            if "dest_env" in args else
                            E.current_env()
                        ),
                        metadata=(
                            lower_expr(p, args["metadata"])
                            if "metadata" in args else
                            E.No(T.env_md)
                        ),
                    )

                action = AddToEnv(
                    mappings=self.create_internal_property(
                        node=node,
                        name="env_mappings",
                        lower_expr=lower_prop_expr,
                        rtype=T.EnvAssoc,
                        location=Location.from_lkt_node(syn_action),
                    ),
                    resolver=self.resolver.resolve_property(
                        args.get("resolver")
                    ),
                    location=location,
                )

            elif action_kind == "add_single_to_env":
                args, _ = S.add_single_to_env_signature.match(
                    self.ctx, syn_action
                )

                action = AddToEnv(
                    mappings=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_mappings",
                        expr=args["mapping"],
                        rtype=T.EnvAssoc,
                    ),
                    resolver=self.resolver.resolve_property(
                        args.get("resolver")
                    ),
                    location=location,
                )

            elif action_kind == "add_all_to_env":
                args, _ = S.add_all_to_env_signature.match(
                    self.ctx, syn_action
                )

                action = AddToEnv(
                    mappings=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_mappings",
                        expr=args["mappings"],
                        rtype=T.EnvAssoc.array,
                    ),
                    resolver=self.resolver.resolve_property(
                        args.get("resolver")
                    ),
                    location=location,
                )

            elif action_kind == "do":
                args, _ = S.do_env_signature.match(self.ctx, syn_action)
                action = Do(
                    expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_do",
                        expr=args["expr"],
                        rtype=None,
                    ),
                    location=location,
                )

            elif action_kind == "handle_children":
                args, _ = S.empty_signature.match(self.ctx, syn_action)
                action = HandleChildren(location=location)

            elif action_kind == "reference":
                args, _ = S.reference_signature.match(self.ctx, syn_action)

                kind_expr = args.get("kind")
                category_expr = args.get("category")
                shed_rebindings_expr = args.get(
                    "shed_corresponding_rebindings"
                )

                kind = RefKind.normal
                if kind_expr is not None:
                    kind_entity = self.resolver.resolve_entity(
                        kind_expr, self.refd_env_scope
                    )
                    assert isinstance(kind_entity, Scope.RefKindValue)
                    kind = kind_entity.value

                shed_rebindings = False
                if shed_rebindings_expr is not None:
                    shed_rebindings = parse_static_bool(
                        self.ctx, shed_rebindings_expr
                    )

                category: str | None = None
                if category_expr is not None:
                    category = parse_static_str(self.ctx, category_expr)

                action = RefEnvs(
                    resolver=self.resolver.resolve_property(args["resolver"]),
                    nodes_expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="ref_env_nodes",
                        expr=args["nodes"],
                        rtype=T.root_node.array,
                    ),
                    kind=kind,
                    dest_env=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_dest",
                        expr=args.get("dest_env"),
                        rtype=T.LexicalEnv,
                    ),
                    cond=self.lower_expr_to_internal_property(
                        node=node,
                        name="ref_cond",
                        expr=args.get("cond"),
                        rtype=T.Bool,
                    ),
                    category=category,
                    shed_rebindings=shed_rebindings,
                    location=location,
                )

            elif action_kind == "set_initial_env":
                args, _ = S.set_initial_env_signature.match(
                    self.ctx, syn_action
                )
                action = SetInitialEnv(
                    env_expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_init",
                        expr=args["env"],
                        rtype=T.DesignatedEnv,
                    ),
                    location=location,
                )

            else:
                with lkt_context(syn_action.f_name):
                    error("invalid env action name")
            actions.append(action)

        with lkt_context(env_spec):
            result = EnvSpec(*actions)
        result.location = Location.from_lkt_node(env_spec)
        result.properties_created = True
        return result

    def defer_type_members(
        self,
        owner: CompiledType,
        decls: L.DeclBlock,
        allowed_field_kinds: FieldKinds,
    ) -> None:
        """
        Create deferred type members lowering for members found in the given
        ``DeclBlock`` node.

        :param ownner: The compiled type that owns these fields.
        :param decls: Declarations to process.
        :param allowed_field_kinds: Set of field kinds allowed for the fields
            to load.
        """
        # Declaration nodes for fields and properties found in ``decls``
        member_decls: list[L.FullDecl] = []

        # Whether one env spec was found
        has_env_spec = False

        # Whether we have found a ``can_reach`` property
        has_can_reach = False

        for full_decl in decls:
            with lkt_context(full_decl):
                decl = full_decl.f_decl

                # If this is actually an env spec, run the dedicated lowering
                # code.
                if isinstance(decl, L.EnvSpecDecl):
                    if not isinstance(owner, ASTNodeType):
                        error("env specs are allowed in nodes only")
                    check_source_language(
                        not has_env_spec,
                        "only one env_spec block allowed per type",
                    )
                    has_env_spec = True
                    self.env_specs_to_lower.append((owner, decl))
                    continue

                # Otherwise, this is a field or a property
                if isinstance(decl, L.FunDecl):
                    check_source_language(
                        allowed_field_kinds.properties,
                        'Properties not allowed in this context'
                    )
                    member_decls.append(full_decl)
                else:
                    member_decls.append(full_decl)

                if decl.f_syn_name.text == "can_reach":
                    has_can_reach = True

        def fields_cb() -> list[AbstractNodeData]:
            result: list[AbstractNodeData] = []

            for full_decl in member_decls:
                with lkt_context(full_decl):
                    if isinstance(full_decl.f_decl, L.FunDecl):
                        result.append(self.lower_property(owner, full_decl))
                    else:
                        result.append(
                            self.lower_base_field(
                                owner,
                                full_decl,
                                allowed_field_kinds,
                            )
                        )

            # If we are adding fields for the root node type and there is no
            # ``can_reach`` property, create the default one.
            if (
                isinstance(owner, ASTNodeType)
                and owner.is_root_node
                and not has_can_reach
            ):
                result.append(owner.create_default_can_reach())
            return result

        self.ctx.deferred.type_members.add(owner, fields_cb)

    def create_node(self,
                    decl: L.BasicClassDecl,
                    annotations: BaseNodeAnnotations) -> ASTNodeType:
        """
        Create an ASTNodeType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        is_enum_node = isinstance(annotations, EnumNodeAnnotations)
        loc = Location.from_lkt_node(decl)

        # Resolve the base node (if any)
        base_type: ASTNodeType | None

        # Check the set of traits that this node implements
        node_trait_ref: L.LktNode | None = None
        token_node_trait_ref: L.LktNode | None = None
        error_node_trait_ref: L.LktNode | None = None
        generic_interfaces: list[GenericInterface] = []
        for trait_ref in decl.f_traits:
            if isinstance(trait_ref, L.SimpleTypeRef):
                if trait_ref.text == "TokenNode":
                    token_node_trait_ref = trait_ref

                elif trait_ref.text == "ErrorNode":
                    error_node_trait_ref = trait_ref

                else:
                    generic_interfaces.append(
                        self.resolver.resolve_generic_interface(
                            trait_ref.f_type_name, self.root_scope
                        )
                    )

            elif not isinstance(trait_ref, L.GenericTypeRef):
                error("Nodes cannot implement this trait", location=trait_ref)

            else:
                # This is a generic instantiation
                generic_trait = trait_ref.f_type_name
                type_args = list(trait_ref.f_params)
                if generic_trait.text == "Node":
                    # If this trait is an instantiation of the Node trait, make
                    # sure it is instantiated on the root node itself (i.e.
                    # "decl").
                    with lkt_context(trait_ref):
                        decl_name = decl.f_syn_name.text
                        check_source_language(
                            len(type_args) == 1
                            and type_args[0].text == decl_name,
                            "The Node generic trait must be instantiated with"
                            f" the root node ({decl_name})",
                        )
                    node_trait_ref = trait_ref

                else:
                    with lkt_context(trait_ref):
                        error("Nodes cannot implement this trait")

        def check_trait(trait_ref: L.LktNode | None,
                        expected: bool,
                        message: str) -> None:
            """
            If ``expected`` is ``True``, emit an error if ``trait_ref`` is
            ``None``. If ``expected`` is ``False``, emit an error if
            ``trait_ref`` is not ``None``. In both cases, use ``message`` as
            the error message.
            """
            if expected:
                check_source_language(trait_ref is not None, message)
            elif trait_ref is not None:
                with lkt_context(trait_ref):
                    error(message)

        # Root node case
        base_type_node = decl.p_base_type
        if base_type_node is None:
            check_trait(
                node_trait_ref,
                True,
                "The root node must implement the Node trait"
            )
            check_trait(
                token_node_trait_ref,
                False,
                "The root node cannot be a token node"
            )
            check_trait(
                error_node_trait_ref,
                False,
                "The root node cannot be an error node"
            )

            if CompiledTypeRepo.root_grammar_class is not None:
                check_source_language(
                    False,
                    'There can be only one root node ({})'.format(
                        CompiledTypeRepo.root_grammar_class.dsl_name
                    )
                )

            base_type = None
            is_token_node = is_error_node = False
        else:
            base_type = self.resolve_base_node(base_type_node)

            check_trait(
                node_trait_ref,
                False,
                "Only the root node can implement the Node trait"
            )

            # This is a token node if either the TokenNode trait is implemented
            # or if the base node is a token node itself. Likewise for
            # ErrorNode.
            is_token_node = token_node_trait_ref is not None
            is_error_node = error_node_trait_ref is not None

            check_source_language(
                base_type is not base_type.is_enum_node,
                'Inheritting from an enum node is forbidden'
            )

        with lkt_context(error_node_trait_ref):
            # Determine whether this node is abstract. Remember that base enum
            # node types are abstract (it is their derivations that are
            # concrete).
            is_abstract = (
                not isinstance(annotations, NodeAnnotations)
                or annotations.abstract
            )
            if is_abstract and is_error_node:
                error("Error nodes cannot be abstract")

            # Determine whether this node is synthetic
            is_synthetic = annotations.synthetic
            if is_synthetic and is_error_node:
                error("Error nodes cannot be synthetic")

            if base_type and base_type.is_list and is_error_node:
                error("Error nodes cannot be lists")

            if is_token_node and is_error_node:
                error("Error nodes cannot be token nodes")

        is_bool_node = (
            isinstance(annotations, EnumNodeAnnotations)
            and annotations.qualifier
        )

        result = ASTNodeType(
            self.ctx,
            name_from_camel(self.ctx, "node type", decl.f_syn_name),
            location=loc,
            doc=lkt_doc(decl),
            base=base_type,
            annotations=Annotations(
                repr_name=annotations.repr_name,
                generic_list_type=annotations.generic_list_type,
                rebindable=annotations.rebindable,
                custom_short_image=annotations.custom_short_image,
                snaps=annotations.snaps,
                ple_unit_root=annotations.ple_unit_root,
            ),
            is_abstract=is_abstract,
            is_token_node=is_token_node,
            is_error_node=is_error_node,
            is_synthetic=is_synthetic,
            with_abstract_list=annotations.with_abstract_list,
            is_enum_node=is_enum_node,
            is_bool_node=is_bool_node,
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
        )

        # The generic list type was just created: add it to the root scope so
        # that the language spec can reference it.
        if base_type is None:
            type_name = result.generic_list_type.dsl_name
            self.root_scope.mapping[type_name] = Scope.BuiltinType(
                type_name, result.generic_list_type
            )

        # Lower fields. Regular nodes can hold all types of fields, but token
        # nodes and enum nodes can hold only user field and properties.
        self.defer_type_members(
            result,
            decl.f_decls,
            allowed_field_kinds=(
                FieldKinds(properties=True, user_fields=True)
                if is_token_node or is_enum_node
                else FieldKinds(
                    properties=True,
                    parse_fields=True,
                    user_fields=True,
                )
            ),
        )

        # Register the generic interfaces that this type implements
        self.ctx.deferred.implemented_interfaces.add(
            result, lambda: generic_interfaces
        )

        # For qualifier enum nodes, add the synthetic "as_bool" abstract
        # property that each alternative will override.
        if is_bool_node:
            self.ctx.deferred.type_members.add(
                result, result.create_abstract_as_bool_cb(loc)
            )

        # Create alternatives for enum nodes
        if isinstance(annotations, EnumNodeAnnotations):
            assert isinstance(decl, L.EnumClassDecl)
            self.create_enum_node_alternatives(
                alternatives=sum(
                    (list(b.f_decls) for b in decl.f_branches), []
                ),
                enum_node=result,
                qualifier=annotations.qualifier
            )

        if is_error_node:
            self.error_nodes.append(result)

        return result

    def create_enum_node_alternatives(
        self,
        alternatives: list[L.EnumClassAltDecl],
        enum_node: ASTNodeType,
        qualifier: bool
    ) -> None:
        """
        Create ASTNodeType instances for the alternatives of an enum node.

        :param alternatives: Declarations for the alternatives to lower.
        :param enum_node: Enum node that owns these alternatives.
        :param qualifier: Whether this enum node has the "@qualifier"
            annotation.
        """
        # RA22-015: initialize this to True for enum nodes directly in
        # ASTNodeType's constructor.
        enum_node.is_type_resolved = True

        enum_node._alternatives = []
        enum_node._alternatives_map = {}

        # All enum classes must have at least one alternative, except those
        # with the "@qualifier" annotation, which implies automatic
        # alternatives.
        if qualifier:
            check_source_language(
                not len(alternatives),
                'Enum nodes with @qualifier cannot have explicit alternatives'
            )
            alt_descriptions = [
                EnumNodeAlternative(names.Name(alt_name),
                                    enum_node,
                                    None,
                                    enum_node.location)
                for alt_name in ('Present', 'Absent')
            ]
        else:
            check_source_language(
                len(alternatives) > 0,
                'Missing alternatives for this enum node'
            )
            alt_descriptions = [
                EnumNodeAlternative(
                    name_from_camel(
                        self.ctx,
                        "enum node alternative",
                        alt.f_syn_name,
                    ),
                    enum_node,
                    None,
                    Location.from_lkt_node(alt)
                )
                for alt in alternatives
            ]

        # Now create the ASTNodeType instances themselves
        alt_nodes: list[ASTNodeType] = []
        for i, alt in enumerate(alt_descriptions):
            alt.alt_node = ASTNodeType(
                self.ctx,
                name=alt.full_name,
                location=enum_node.location,
                doc='',
                base=enum_node,
                dsl_name='{}.{}'.format(enum_node.dsl_name,
                                        alt.base_name.camel)
            )
            alt_nodes.append(alt.alt_node)

            if qualifier:
                # Override the abstract "as_bool" property that all qualifier
                # enum nodes define.
                self.ctx.deferred.type_members.add(
                    alt.alt_node,
                    alt.alt_node.create_concrete_as_bool_cb(
                        is_present=i == 0,
                        location=enum_node.location,
                    )
                )

        # Finally create enum node-local indexes to easily fetch the
        # ASTNodeType instances later on.
        enum_node._alternatives = alt_nodes
        enum_node._alternatives_map = {
            alt.base_name.camel: alt_node
            for alt, alt_node in zip(alt_descriptions, alt_nodes)
        }

    def create_enum(self,
                    decl: L.EnumTypeDecl,
                    annotations: EnumAnnotations) -> EnumType:
        """
        Create an EnumType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        # Decode the list of enum literals and validate them
        value_names = []
        for lit in decl.f_literals:
            name = name_from_lower(self.ctx, "enum value", lit.f_syn_name)
            check_source_language(
                name not in value_names,
                'The "{}" literal is present twice'
            )
            value_names.append(name)

        # If present, validate the default value
        default_value: names.Name | None = None
        default_expr = annotations.with_default
        if default_expr is not None:
            with lkt_context(default_expr):
                if not isinstance(default_expr, L.RefId):
                    error("enum value identifier expected")
                default_value = names.Name.from_lower(default_expr.text)
                if default_value not in value_names:
                    error("no such value in this enum")

        result = EnumType(
            self.ctx,
            name=name_from_camel(self.ctx, "enum type", decl.f_syn_name),
            location=Location.from_lkt_node(decl),
            doc=lkt_doc(decl),
            value_names=value_names,
            default_val_name=default_value,
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
        )
        return result

    def create_struct(self,
                      decl: L.StructDecl,
                      annotations: StructAnnotations) -> StructType:
        """
        Create a StructType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        # Check the set of traits that this node implements
        generic_interfaces: list[GenericInterface] = []
        for trait_ref in decl.f_traits:
            if isinstance(trait_ref, L.SimpleTypeRef):
                generic_interfaces.append(
                    self.resolver.resolve_generic_interface(
                        trait_ref.f_type_name, self.root_scope
                    )
                )
            else:
                error(
                    "Structs cannot implement this trait", location=trait_ref
                )

        result = StructType(
            self.ctx,
            name_from_camel(self.ctx, "struct type", decl.f_syn_name),
            location=Location.from_lkt_node(decl),
            doc=lkt_doc(decl),
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
        )
        if annotations.metadata:
            check_source_language(
                CompiledTypeRepo.env_metadata is None,
                "Only one struct can be the env metadata",
            )
            CompiledTypeRepo.env_metadata = result

        # Lower fields
        self.defer_type_members(
            result,
            decl.f_decls,
            allowed_field_kinds=(
                FieldKinds(metadata_fields=True)
                if annotations.metadata else
                FieldKinds(user_fields=True)
            ),
        )

        # Register the generic interfaces that this type implements
        self.ctx.deferred.implemented_interfaces.add(
            result, lambda: generic_interfaces
        )

        return result

    def process_prelude_decl(self, full_decl: L.FullDecl) -> None:
        """
        Process a declaration from the prelude. Currently, this only creates
        builtin generic interfaces.
        """
        # Ignore non-traits, and traits that are not generic interfaces
        decl = full_decl.f_decl
        if not isinstance(decl, L.TraitDecl):
            return

        annotations = parse_annotations(
            self.ctx, TraitAnnotations, full_decl, self.root_scope
        )
        if annotations.generic_interface is None:
            return

        self.register_generic_interface(decl, annotations.generic_interface)

    def process_user_trait(self, decl: L.TraitDecl) -> None:
        """
        Process a trait declared in user code.
        """
        full_decl = decl.parent
        assert isinstance(full_decl, L.FullDecl)

        # The only traits that are supported there are generic interfaces
        annotations = parse_annotations(
            self.ctx, TraitAnnotations, full_decl, self.root_scope
        )
        if annotations.generic_interface is None:
            error(
                "only generic interface traits are allowed",
                location=decl,
            )

        self.register_generic_interface(decl, annotations.generic_interface)

    def register_generic_interface(
        self,
        decl: L.TraitDecl,
        annotations: GenericInterfaceAnnotationSpec.Value,
    ) -> None:
        """
        Create a generic interface and schedule the lowering of their members
        later.

        :param decl: Trait declaration for this generic interface.
        :param annotations: Annotations for this generic interface.
        """
        # Create the GenericInterface instance itself
        name = name_from_camel(
            self.ctx, "generic_interface", decl.f_syn_name
        ).camel
        gen_iface = GenericInterface(
            name=name,
            ctx=self.ctx,
            is_always_node=annotations.node_only,
            doc=lkt_doc(decl),
        )

        # Register it in the root scope
        self.root_scope.add(
            Scope.GenericInterface(
                name=name,
                diagnostic_node=decl,
                generic_interface=gen_iface,
            )
        )

        # Schedule the lowering of its member in the
        # GENERIC_INTERFACE_MEMBERS_LOWERING pass, when all compiled types will
        # be known.
        self.gen_iface_decls.append((gen_iface, decl))

    def lower_generic_interface_members(
        self,
        gen_iface: GenericInterface,
        decl: L.TraitDecl,
    ) -> None:
        """
        Lower all the members of the given generic interface.

        :param gen_iface: Generic interface whose members must be lowered.
        :param decl: Lkt parse node for the generic interface itself. The
            members to lower are searched from there.
        """
        for full_member_decl in decl.f_decls:
            # The only legal declarations inside a generic interface are
            # annotation-less functions with no body.
            check_no_annotations(full_member_decl)
            member_decl = full_member_decl.f_decl
            if not isinstance(member_decl, L.FunDecl):
                error(
                    "only function declarations are allowed in generic"
                    " interfaces",
                    location=member_decl,
                )
            if member_decl.f_body is not None:
                error(
                    "functions in generic interfaces cannot have bodies",
                    location=member_decl.f_body,
                )

            # Decode the method name and signature
            method_name = name_from_lower(
                self.ctx, "function", member_decl.f_syn_name
            ).lower
            method_doc = lkt_doc(member_decl)
            return_type = self.resolver.resolve_type_or_gen_iface(
                member_decl.f_return_type, self.root_scope
            )
            args: list[GenericArgument] = []
            for a in member_decl.f_args:
                check_no_annotations(a.f_decl_annotations)
                if a.f_default_val is not None:
                    error(
                        "default argument values not allowed in generic"
                        " interfaces",
                        location=a.f_default_val,
                    )
                args.append(
                    GenericArgument(
                        name=name_from_lower(
                            self.ctx, "argument", a.f_syn_name
                        ).lower,
                        type=self.resolver.resolve_type_or_gen_iface(
                            a.f_decl_type, self.root_scope
                        ),
                    )
                )

            # Finally register the method in its owning generic interfac
            gen_iface.add_method(method_name, args, return_type, method_doc)

    def set_implemented_method(
        self,
        member: AbstractNodeData,
        method_name: L.DotExpr,
    ) -> None:
        """
        Mark the generic interface designated by ``method_name`` as implemented
        by ``member``.
        """
        self.ctx.deferred.implemented_methods.add(
            member,
            lambda: self.resolver.resolve_generic_interface_method(
                method_name, self.root_scope
            ),
        )


def create_types(resolver: Resolver) -> None:
    """
    Create types from Lktlang units.

    :param resolver: Resolver for Lkt entities.
    """
    loader = LktTypesLoader(resolver)
    resolver.context.lkt_types_loader = loader
