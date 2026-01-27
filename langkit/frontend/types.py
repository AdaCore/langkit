from __future__ import annotations

import dataclasses
import functools
import itertools
from typing import Any, Callable, TYPE_CHECKING, Type, overload

import liblktlang as L

from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    Annotations,
    Argument,
    BaseField,
    CompiledType,
    EnumNodeAlternative,
    EnumType,
    Field,
    MemberNames,
    MetadataField,
    StructType,
    T,
    UserField,
)
from langkit.diagnostics import (
    DiagnosticContext,
    Location,
    check_source_language,
    error,
)
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
from langkit.expressions import PropertyDef, lazy_field
from langkit.frontend.annotations import (
    AnnotationSpec,
    FlagAnnotationSpec,
    ParsedAnnotations,
    StringLiteralAnnotationSpec,
    check_no_annotations,
    parse_annotations,
)
from langkit.frontend.expressions import ExpressionCompiler
import langkit.frontend.func_signatures as S
from langkit.frontend.resolver import Resolver
from langkit.frontend.scopes import Scope
from langkit.frontend.static import parse_static_bool, parse_static_str
from langkit.frontend.utils import (
    arg_name_from_expr,
    determine_casing,
    lkt_doc,
    name_from_camel,
    name_from_lower,
)
from langkit.generic_interface import GenericArgument, GenericInterface
import langkit.names as names


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
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        for arg in args:
            error(
                "no positional argument expected", location=annotation.f_name
            )

        result = self.Value()
        for k, v in kwargs.items():
            if k == "uses_envs":
                result.uses_envs = parse_static_bool(ctx, v)
            elif k == "uses_entity_info":
                result.uses_entity_info = parse_static_bool(ctx, v)
            else:
                error(
                    "invalid keyword argument", location=arg_name_from_expr(v)
                )
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
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        for arg in args:
            error(
                "no positional argument expected", location=annotation.f_name
            )

        result = self.Value()
        for k, v in kwargs.items():
            if k == "node_only":
                result.node_only = parse_static_bool(ctx, v)
            else:
                error(
                    "invalid keyword argument", location=arg_name_from_expr(v)
                )
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
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        check_source_language(
            len(args) == 1 and not kwargs,
            "exactly one positional argument expected",
            location=annotation.f_name,
        )
        return args[0]


class WithDynvarsAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_dynvars annotations for properties.
    """

    @dataclasses.dataclass(frozen=True)
    class Value:
        """
        Like ``LktTypesLoader.DynVarAsArg``, but before expression lowering.
        """

        dynvar: Scope.BuiltinDynVar | Scope.DynVar
        """
        Dynamic variable to use as a property argument.
        """

        decl_node: L.LktNode
        """
        Lkt node that acts as a declaration of this dynamic variable as an
        argument for the property.
        """

        default_value: L.Expr | None
        """
        Default value that is bound to this dynamic variable when calling the
        property, if there is one.
        """

        @property
        def location(self) -> Location:
            return Location.from_lkt_node(self.decl_node)

    def __init__(self) -> None:
        super().__init__("with_dynvars", unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        result: list[WithDynvarsAnnotationSpec.Value] = []

        def add(
            node: L.LktNode,
            entity: Scope.Entity,
            default_value: L.Expr | None = None,
        ) -> None:
            """
            Append a dynamic variable to ``result``. This also performs
            validity checks on the arguments.

            :param node: Node corresponding to the declaration of the dynamic
                variable as a property argument, used to get a corresponding
                source location.
            :param entity: Entity that is supposed to be a dynamic variable
                (this is checked).
            :param default_value: If this dynamic variable is optional, default
                value for it.
            """
            if not isinstance(entity, (Scope.BuiltinDynVar, Scope.DynVar)):
                error(
                    f"dynamic variable expected, got {entity.diagnostic_name}",
                    location=node,
                )
            if entity in result:
                error(
                    "dynamic variables can appear at most once",
                    location=node,
                )
            result.append(
                WithDynvarsAnnotationSpec.Value(entity, node, default_value)
            )

        # Positional arguments are supposed to be just dynamic variable names
        for arg in args:
            entity = scope.resolve(arg)
            add(arg, entity)

        # Keyword arguments are supposed to associate a dynamic variable name
        # ("name" below) to a default value for the dynamic variable in the
        # current property ("default_value" below).
        for name, default_value in kwargs.items():
            try:
                entity = scope.lookup(name)
            except KeyError as exc:
                error(exc.args[0], location=arg_name_from_expr(default_value))

            # Recover the location of the argument name
            argument = default_value.parent
            assert isinstance(argument, L.Argument)
            arg_name = argument.f_name

            add(arg_name, entity, default_value)

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
        FlagAnnotationSpec("synthetic"),
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
        FlagAnnotationSpec("abstract")
    ]


@dataclasses.dataclass
class EnumNodeAnnotations(BaseNodeAnnotations):
    qualifier: bool
    annotations = BaseNodeAnnotations.annotations + [
        FlagAnnotationSpec("qualifier")
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
    annotations = [
        FlagAnnotationSpec("abstract"),
        FlagAnnotationSpec("exported"),
        FlagAnnotationSpec("final"),
        FlagAnnotationSpec("lazy"),
        FlagAnnotationSpec("null_field"),
        FlagAnnotationSpec("nullable"),
        FlagAnnotationSpec("parse_field"),
        FlagAnnotationSpec("traced"),
        FlagAnnotationSpec("used_in_equality"),
    ]


@dataclasses.dataclass
class EnumAnnotations(ParsedAnnotations):
    with_default: L.Expr | None
    annotations = [WithDefaultAnnotationSpec()]


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
    with_dynvars: list[WithDynvarsAnnotationSpec.Value] | None
    annotations = [
        FlagAnnotationSpec("abstract"),
        FlagAnnotationSpec("call_memoizable"),
        StringLiteralAnnotationSpec("call_non_memoizable_because"),
        FlagAnnotationSpec("exported"),
        ExternalAnnotationSpec(),
        FlagAnnotationSpec("final"),
        FlagAnnotationSpec("ignored"),
        FlagAnnotationSpec("memoized"),
        StringLiteralAnnotationSpec("predicate_error"),
        FlagAnnotationSpec("property"),
        FlagAnnotationSpec("traced"),
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


@dataclasses.dataclass(frozen=True)
class ToLowerDynVar:
    """
    Helper to store information about a dynamic variable to create.

    This is instantiated when processing all top-level declarations in a unit,
    and is used once types are lowered, when the dyn vars can be actually
    created.
    """

    name: str
    """
    Name for the dynamic variable (already validated).
    """

    decl: L.DynVarDecl
    """
    Lkt declaration for this dynamic variable.
    """

    scope: Scope
    """
    Scope in which this dynamic variable is defined.
    """

    entity: Scope.DynVar
    """
    Scope entity created for this dynamic variable.
    """

    @classmethod
    def from_lkt_node(
        cls,
        decl: L.DynVarDecl,
        scope: Scope,
        resolver: Resolver,
    ) -> ToLowerDynVar:
        name_node = decl.f_syn_name
        name = name_node.text

        # Ensure the dynamic variable name has proper casing
        _ = name_from_lower("dynamic variable", name_node)

        entity = Scope.DynVar(name, decl)
        scope.add(entity)

        # Reject homonymous dynamic vars, even if they are declared in
        # different scopes.
        resolver.global_scope.add(entity)

        return cls(name, decl, scope, entity)


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
        decl: L.Decl
        """
        Declaration node for this property.
        """

        scope: Scope
        """
        Scope to resolve references in this property.
        """

        prop: PropertyDef
        """
        The property whose expression must be lowered.
        """

        arguments: list[L.FunParamDecl]
        """
        Arguments for this property.
        """

        dynamic_vars: list[WithDynvarsAnnotationSpec.Value] | None
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

        body_scope: Scope
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

        scope: Scope
        """
        Scope to resolve references in this field declaration.
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

        self.compiled_types: dict[L.Decl, CompiledType | None] = {}
        self.internal_property_counter = iter(itertools.count(0))
        self.error_nodes: list[ASTNodeType] = []
        self.gen_iface_decls: list[tuple[GenericInterface, L.TraitDecl]] = []

        type_decls: list[Scope.UserType] = []
        dyn_vars: list[ToLowerDynVar] = []
        root_node_decl: L.BasicClassDecl | None = None
        metadata_found = False

        units_and_scopes = [
            (module.unit, module.unit_scope)
            for module in resolver.lkt_modules.values()
        ]

        # Look for generic interfaces defined in the prelude
        assert isinstance(resolver.lkt_units[0].root, L.LangkitRoot)
        prelude = resolver.lkt_units[0].root.p_fetch_prelude
        assert isinstance(prelude.root, L.LangkitRoot)
        for full_decl in prelude.root.f_decls:
            if isinstance(full_decl.f_decl, L.TraitDecl):
                self.process_prelude_decl(full_decl)

        # Go through all units and register all top-level definitions in the
        # unit scopes. This first pass allows to check for name uniqueness,
        # create TypeRepo.Defer objects and build the list of types to lower.
        for unit, unit_scope in units_and_scopes:
            assert isinstance(unit.root, L.LangkitRoot)

            for full_decl in unit.root.f_decls:
                decl = full_decl.f_decl
                name = decl.f_syn_name.text
                if isinstance(decl, L.LexerDecl):
                    unit_scope.add(Scope.Lexer(name, decl))
                elif isinstance(decl, L.GrammarDecl):
                    unit_scope.add(Scope.Grammar(name, decl))
                elif isinstance(decl, L.TraitDecl):
                    self.process_user_trait(decl, self.root_scope)
                elif isinstance(decl, L.TypeDecl):
                    entity = Scope.UserType(name, decl, None, unit_scope)
                    unit_scope.add(entity)

                    # Reject homonymous type names, even if they are declared
                    # in different scopes.
                    self.resolver.global_scope.add(entity)

                    # Keep track of anything that looks like the root node, or
                    # the Metadata struct.
                    insert_first = False
                    if (
                        isinstance(decl, L.BasicClassDecl)
                        and decl.p_base_type is None
                    ):
                        root_node_decl = decl
                        insert_first = True
                    elif name == "Metadata":
                        metadata_found = True

                    # Plan to lower the root node first, so that the generic
                    # list type is registered in scopes before we need to
                    # lookup types.
                    type_decls.insert(
                        0 if insert_first else len(type_decls) + 1, entity
                    )

                elif isinstance(decl, L.DynVarDecl):
                    dyn_vars.append(
                        ToLowerDynVar.from_lkt_node(
                            decl, unit_scope, self.resolver
                        )
                    )

                else:
                    error(
                        "invalid top-level declaration:"
                        f" {decl.p_decl_type_name}",
                        location=decl,
                    )

        # There is little point going further if we have not found the root
        # node type.
        if root_node_decl is None:
            error(
                "no node type declaration found",
                location=resolver.root_lkt_source_loc,
            )

        # If user code does not define one, create a default Metadata struct
        # and make it visible in the root scope.
        if not metadata_found:
            self.ctx.env_metadata = StructType(
                context=self.ctx,
                name=names.Name("Metadata"),
                location=Location.builtin,
                doc="",
                fields=None,
            )
            self.root_scope.mapping["Metadata"] = Scope.BuiltinType(
                "Metadata", self.ctx.env_metadata
            )
            self.has_env_metadata = True

        # Finishing touch for unit scopes: apply imports
        self.apply_module_imports(units_and_scopes)

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
        self.env_specs_to_lower: list[
            tuple[ASTNodeType, L.EnvSpecDecl, Scope]
        ] = []
        self.fields_to_lower: list[LktTypesLoader.FieldToLower] = []
        for entity in type_decls:
            self.lower_type_decl(entity, entity.scope)

        #
        # DYNVAR_LOWERING
        #

        # Create dynamic variables
        for dv in dyn_vars:
            dyn_var = E.DynamicVariable(
                location=Location.from_lkt_node(dv.decl),
                name=dv.name,
                type=self.resolver.resolve_type(dv.decl.f_decl_type, dv.scope),
                doc=lkt_doc(dv.decl),
            )
            dv.entity.variable_or_none = dyn_var

        #
        # TYPE_MEMBERS_LOWERING
        #

        # Now that there is a CompiledType instance for all builtin and named
        # types, it is possible to instantiate all type members: do that for
        # type members that were deferred so far.
        self.ctx.deferred.type_members.resolve()

        # Likewise for the type of dynamic variables
        self.ctx.deferred.dynamic_variable_types.resolve()

        # Finally, now that type members are populated, make sure the metadata
        # struct fields are legal.
        self.check_env_metadata()

        # Reject non-null fields for error nodes. Non-null fields can come from
        # this node's own declaration, or they can come from inheritance.
        for node in self.error_nodes:
            error_msg = "Error nodes can only have null fields"
            for f in node.get_parse_fields(include_inherited=True):
                if not (f.null or f.abstract):
                    if f.owner != node:
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
            self.lower_generic_interface_members(
                gen_iface, gen_iface_decl, self.root_scope
            )

        # Now that all generic interface members are known, evaluate the
        # deferred references to them.
        self.ctx.deferred.implemented_methods.resolve()

        #
        # ENV_SPECS_LOWERING
        #

        for node, env_spec_decl, scope in self.env_specs_to_lower:
            env_spec = self.lower_env_spec(node, env_spec_decl, scope)
            node.env_spec = env_spec
            env_spec.register_categories(self.ctx)

        #
        # STATIC_EXPR_LOWERING
        #

        # Now that all user-defined compiled types are known, we can start
        # lowering expressions and env specs. Start with default values for
        # property arguments and dynamic variables.
        for p_to_lower in self.properties_to_lower:
            for arg_decl, arg in zip(
                p_to_lower.arguments, p_to_lower.prop.arguments
            ):
                if arg_decl.f_default_val is not None:
                    arg.default_value = self.lower_static_expr(
                        arg_decl.f_default_val, arg.type, p_to_lower.scope
                    )

            if p_to_lower.dynamic_vars is not None:
                p_to_lower.prop.set_dynamic_var_args(
                    [
                        E.DynamicVariable.ArgumentDecl(
                            dynvar=v.dynvar.variable,
                            location=v.location,
                            default_value=(
                                None
                                if v.default_value is None
                                else self.lower_static_expr(
                                    v.default_value,
                                    v.dynvar.variable.type,
                                    p_to_lower.scope,
                                )
                            ),
                        )
                        for v in p_to_lower.dynamic_vars
                    ]
                )

        # Finally, lower default values for fields
        for f_to_lower in self.fields_to_lower:
            f_to_lower.field.default_value = self.lower_static_expr(
                f_to_lower.default_value,
                f_to_lower.field.type,
                f_to_lower.scope,
            )

    def apply_module_imports(
        self,
        units_and_scopes: list[tuple[L.AnalysisUnit, Scope]],
    ) -> None:
        """
        Add imported entities to the relevant unit scopes.
        """
        if TYPE_CHECKING:
            Resolver = Callable[[], Scope.Entity]

        def make_resolver(unit_scope: Scope, imported_name: L.Id) -> Resolver:
            """
            Return a resolver for the entity in the given scope and with the
            given name. The return is intended to be used as a callback for
            ``Scope.Imported.resolver``.
            """

            def resolver() -> Scope.Entity:
                entity = unit_scope.resolve(imported_name, recursive=False)
                if isinstance(entity, Scope.Imported):
                    import_loc = Location.from_lkt_node(entity.import_node)
                    error(
                        "cannot re-import entities (imported at"
                        f" {import_loc.gnu_style_repr()})",
                        imported_name,
                    )
                return entity

            return resolver

        imported_entities: list[Scope.Imported] = []

        # Create Scope.Imported instances for all imported entities and store
        # them in the unit scopes that imported them.
        for unit, unit_scope in units_and_scopes:
            assert isinstance(unit.root, L.LangkitRoot)
            mapping = unit_scope.mapping

            def add(
                name: str,
                entity: Scope.Entity | Resolver,
                diagnostic_node: L.LktNode,
                renaming: L.DefId | None = None,
            ) -> None:
                """
                Add an imported entity to the current unit scope.
                """
                # If the imported entity is renamed, ensure the casing for the
                # renaming is consistent with the casing of the original entity
                # name.
                target_name = name if renaming is None else renaming.text

                # Reject the import if the target scope already has an entity
                # with the same name.
                other_entity = mapping.get(target_name)
                if other_entity is not None:
                    error(
                        "this import conflicts with"
                        f" {other_entity.diagnostic_name}",
                        diagnostic_node,
                    )

                # If we got an entity, store it directly in the scope.
                # Otherwise, go through a resolver so that we resolve it only
                # once all imports are done.
                actual_entity: Scope.Entity | None
                if isinstance(entity, Scope.Entity):
                    actual_entity = entity

                    def resolver() -> Scope.Entity:
                        assert isinstance(entity, Scope.Entity)
                        return entity

                else:
                    actual_entity = None
                    resolver = entity

                # Create an Scope.Imported instance, to clearly materialize
                # that this entity has been imported (it is not
                # defined/exported by this module itself).
                imported = Scope.Imported(
                    target_name,
                    actual_entity,
                    diagnostic_node,
                    renaming,
                    resolver,
                )
                mapping[target_name] = imported
                imported_entities.append(imported)

            for clause in unit.root.f_imports:
                match clause:
                    case L.Import():
                        for imported_name in clause.f_imported_names:
                            original_name = imported_name.f_original_name
                            renaming = imported_name.f_renaming
                            module = self.resolver.resolve_module(
                                original_name
                            )
                            add(
                                imported_name.f_original_name.text,
                                module,
                                renaming or original_name,
                                renaming,
                            )

                    case L.ImportFrom():
                        module = self.resolver.resolve_module(
                            clause.f_module_name
                        )
                        for imported_name in clause.f_imported_names:
                            original_name = imported_name.f_original_name
                            add(
                                original_name.text,
                                make_resolver(
                                    module.unit_scope, original_name
                                ),
                                original_name,
                                imported_name.f_renaming,
                            )

                    case L.ImportAllFrom():
                        module = self.resolver.resolve_module(
                            clause.f_module_name
                        )
                        for e in module.unit_scope.mapping.values():
                            if not isinstance(e, Scope.Imported):
                                add(e.name, e, diagnostic_node=clause)

                    case _:
                        raise AssertionError()

        # Resolve all imported entities. It is essential to do it now so that
        # all Scope.Imported.entity_or_none attributes are set to non-None
        # values (i.e. so that Scope.Imported.entity properties are guaranteed
        # to return an entity).
        for entity in imported_entities:
            entity.resolve()

            # Now that the imported entity is resolved, we can assume that its
            # casing is valid. Check that the casing of the renaming is
            # consistent with it.
            if entity.renaming_node:
                original_casing = determine_casing(entity.entity.name)
                consistent = True
                if original_casing == names.lower:
                    try:
                        names.check_lower(entity.name)
                    except ValueError:
                        consistent = False
                else:
                    try:
                        names.check_camel(entity.name)
                    except ValueError:
                        consistent = False
                if not consistent:
                    error(
                        "casing convention inconsistent with imported entity",
                        entity.renaming_node,
                    )

    def lower_expressions(self) -> None:
        #
        # EXPR_LOWERING
        #

        for p_to_lower in self.properties_to_lower:
            if not isinstance(
                p_to_lower, LktTypesLoader.PropertyAndExprToLower
            ):
                continue

            prop = p_to_lower.prop

            # Now that we have LocalVar instances for the dynamic variables
            # used as arguments, add the corresponding bindings to the property
            # root scope.
            #
            # To access the list of dynamic variables for "prop", go through
            # "prop.dynamic_var_args" instead of "p_to_lower.dynamic_vars" so
            # that, so that we process "implicit" dynvar args (i.e. not
            # declared, but inheritted).
            if prop.dynamic_var_args:
                # For properties that are part of a derivation hierarchy (i.e.
                # either overriden or overriding), we artifically mark them as
                # looked up, so that we never flag them as unused. In a tree of
                # properties, it just too common to have at least one property
                # not using all of its dynamic variables, but since all
                # properties in that tree must have the same set of dynamic
                # variable arguments, the warning would not be actionnable.
                force_look_up = prop.base or prop.overridings

                for i, dv_arg in enumerate(prop.dynamic_var_args):
                    name = dv_arg.dynvar.name.lower

                    # For diagnostic purposes, relate to the @with_dynvars
                    # annotation when it is present, and refer to the parent
                    # property otherwise.
                    decl_node = (
                        p_to_lower.dynamic_vars[i].decl_node
                        if p_to_lower.dynamic_vars
                        else p_to_lower.decl
                    )
                    p_to_lower.body_scope.add(
                        Scope.BoundDynVar(
                            name,
                            decl_node,
                            dv_arg.local_var.ref_expr,
                            dv_arg.dynvar,
                        )
                    )
                    if force_look_up:
                        p_to_lower.body_scope.looked_up.add(name)

            # Now that all types and properties ("declarations") are available,
            # lower the property expressions themselves.
            with prop.bind(bind_dynamic_vars=True):
                self.reset_names_counter()
                prop.set_expr(
                    p_to_lower.body,
                    self.lower_expr(
                        p_to_lower.body, p_to_lower.body_scope, prop
                    ),
                )
                p_to_lower.body_scope.report_unused()

        self.ctx.deferred.property_expressions.resolve()

    def resolve_base_node(self, name: L.TypeRef, scope: Scope) -> ASTNodeType:
        """
        Resolve a type reference and lower it, checking that it is a node type.

        :param name: Type reference to resolve.
        :param scope: Scope in which to look for the type.

        Note: This method is meant to be used instead of ``resolve_node``
        during the TYPES_LOWERING pass since scopes are not populated yet with
        ``CompiledType`` instances at this stage.
        """
        diag_ctx = DiagnosticContext(name)

        # There are only two legal cases: the base type is just a node class
        # defined in user code (SimpleTypeRef) or it is a bare node list
        # instantiation (GenericTypeRef). Reject everything else.
        if isinstance(name, L.SimpleTypeRef):
            # We have a direct node class reference: first fetch the Lkt
            # declaration for it. Lower it if needed.
            entity = self.resolver.resolve_type_entity(name.f_type_name, scope)
            if entity.t_or_none is None:
                assert isinstance(entity.diagnostic_node, L.TypeDecl)
                base_type = self.lower_type_decl(entity, entity.scope)
            else:
                base_type = entity.t_or_none

            # Only nodes can be used as base types
            if not isinstance(base_type, ASTNodeType):
                diag_ctx.error("node type expected")
            return base_type

        elif isinstance(name, L.GenericTypeRef):
            # This must be a node list instantiation: validate the
            # instantiation itself.
            astlist_name = self.generics.ast_list.name
            if name.f_type_name.text != astlist_name:
                diag_ctx.error(
                    "the only generic allowed in this context is"
                    f" {astlist_name}"
                )

            # Lower type arguments
            type_args = [self.resolve_base_node(t, scope) for t in name.f_args]
            diag_ctx.check_source_language(
                len(type_args) == 1,
                f"{astlist_name} expects type argument: the list element type",
            )
            return type_args[0].list

        else:
            diag_ctx.error("invalid node type reference")

    def lower_type_decl(
        self,
        entity: Scope.UserType,
        scope: Scope,
    ) -> CompiledType:
        """
        Create the CompiledType instance corresponding to the given Lkt type
        declaration. Do nothing if it has been already lowered, and stop with
        an error if the lowering for this type is already running (case of
        invalid circular type dependency).

        :param entity: Scope entity for the type to lower. Scopes
            initialization already created this entity: if this type was not
            yet lowered, lowering must associate the ``CompiledType`` instance
            to the entity.
        :param scope: Scope used to resolve references in this type
            declaration.
        """
        decl = entity.diagnostic_node
        assert isinstance(decl, L.TypeDecl)

        # Sentinel for the dict lookup below, as compiled_type can contain None
        # entries.
        try:
            t = self.compiled_types[decl]
        except KeyError:
            # The type is not lowered yet: let's do it. Add the sentinel to
            # reject type inheritance loop during recursion.
            self.compiled_types[decl] = None
        else:
            if t is None:
                error("Type inheritance loop detected", location=decl)
            else:
                # The type is already lowered: there is nothing to do
                return t

        # Dispatch now to the appropriate lowering helper
        result: CompiledType
        full_decl = decl.parent
        assert isinstance(full_decl, L.FullDecl)
        if isinstance(decl, L.BasicClassDecl):

            specs = (
                EnumNodeAnnotations
                if isinstance(decl, L.EnumClassDecl)
                else NodeAnnotations
            )
            result = self.create_node(
                decl,
                parse_annotations(self.ctx, specs, full_decl, scope),
                scope,
            )

        elif isinstance(decl, L.EnumTypeDecl):
            check_source_language(
                len(decl.f_traits) == 0,
                "No traits allowed on enum types",
                location=decl.f_traits,
            )
            result = self.create_enum(
                decl,
                parse_annotations(self.ctx, EnumAnnotations, full_decl, scope),
            )

        elif isinstance(decl, L.StructDecl):
            result = self.create_struct(
                decl,
                parse_annotations(
                    self.ctx, StructAnnotations, full_decl, scope
                ),
                scope,
            )

        else:
            raise NotImplementedError(
                "Unhandled type declaration: {}".format(decl)
            )

        self.compiled_types[decl] = result
        entity.t_or_none = result
        return result

    def lower_base_field(
        self,
        owner: CompiledType,
        full_decl: L.FullDecl,
        allowed_field_kinds: FieldKinds,
        scope: Scope,
    ) -> AbstractNodeData:
        """
        Lower the field described in ``decl``.

        :param allowed_field_kinds: Set of field kinds allowed for the fields
            to load.
        :param scope: Scope used to resolve references in this field
            declaration.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FieldDecl)

        # Ensure the dynamic variable name has proper casing
        name = name_from_lower("field", decl.f_syn_name)

        annotations = parse_annotations(
            self.ctx, FieldAnnotations, full_decl, scope
        )
        field_type = self.resolver.resolve_type(decl.f_decl_type, scope)
        doc = lkt_doc(decl)

        cls: Type[AbstractNodeData]
        constructor: Callable[..., AbstractNodeData]
        kwargs: dict[str, Any] = {"type": field_type, "doc": doc}

        check_source_language(
            annotations.parse_field or not annotations.null_field,
            "@nullable is valid only for parse fields",
            location=decl,
        )

        names: MemberNames
        body: L.Expr | None = None
        if annotations.lazy:
            check_source_language(
                not annotations.null_field,
                "Lazy fields cannot be null",
                location=annotations.syn_annotations["null_field"],
            )
            check_source_language(
                not annotations.final,
                "Lazy fields are implicitly final",
                location=annotations.syn_annotations["final"],
            )
            cls = PropertyDef
            constructor = lazy_field
            names = MemberNames.for_lazy_field(owner, name)

            body = decl.f_default_val

            kwargs = {
                "expr": None,
                "doc": doc,
                "public": annotations.exported,
                "return_type": field_type,
                "abstract": annotations.abstract,
                "activate_tracing": annotations.traced,
            }

        elif annotations.parse_field:
            assert decl.f_default_val is None
            check_source_language(
                not annotations.exported,
                "Parse fields are implicitly exported",
                location=annotations.syn_annotations["exported"],
            )
            check_source_language(
                not annotations.final,
                "Concrete parse fields are implicitly final",
                location=annotations.syn_annotations["final"],
            )
            check_source_language(
                not annotations.lazy,
                "Parse fields cannot be lazy",
                location=annotations.syn_annotations["lazy"],
            )
            check_source_language(
                not annotations.traced,
                "Parse fields cannot be traced",
                location=annotations.syn_annotations["traced"],
            )
            cls = constructor = Field
            names = MemberNames.for_node_field(owner, name)
            kwargs["abstract"] = annotations.abstract
            kwargs["null"] = annotations.null_field
            kwargs["nullable"] = annotations.nullable

        else:
            check_source_language(
                not annotations.abstract,
                "Regular fields cannot be abstract",
                location=annotations.syn_annotations["abstract"],
            )
            check_source_language(
                not annotations.exported,
                "Regular fields are implicitly exported",
                location=annotations.syn_annotations["exported"],
            )
            check_source_language(
                not annotations.final,
                "Regular fields are implicitly final",
                location=annotations.syn_annotations["final"],
            )
            check_source_language(
                not annotations.lazy,
                "Regular fields cannot be lazy",
                location=annotations.syn_annotations["lazy"],
            )
            check_source_language(
                not annotations.null_field,
                "Regular fields cannot be null",
                location=annotations.syn_annotations["null_field"],
            )
            check_source_language(
                not annotations.traced,
                "Regular fields cannot be traced",
                location=annotations.syn_annotations["traced"],
            )
            cls = constructor = UserField
            names = (
                MemberNames.for_node_field(owner, name)
                if isinstance(owner, ASTNodeType)
                else MemberNames.for_struct_field(name)
            )
            kwargs["public"] = not isinstance(owner, ASTNodeType)

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
                    location=annotations.syn_annotations["used_in_equality"],
                )

        check_source_language(
            allowed_field_kinds.has(cls),
            "Invalid field type in this context",
            location=decl,
        )

        field_loc = Location.from_lkt_node(decl)
        result = constructor(
            owner=owner,
            names=names,
            location=field_loc,
            **kwargs,
        )

        if decl.f_trait_ref is not None:
            assert isinstance(result, (PropertyDef, BaseField))
            self.set_implemented_method(result, decl.f_trait_ref, scope)

        # If this field has an initialization expression implemented as
        # property, plan to lower it later.
        if isinstance(result, PropertyDef):
            assert body is not None
            arguments, body_scope = self.lower_property_arguments(
                prop=result,
                arg_decl_list=None,
                label=f"initializer for lazy field {result.qualname}",
                scope=scope,
            )
            self.properties_to_lower.append(
                self.PropertyAndExprToLower(
                    decl, scope, result, arguments, None, body, body_scope
                )
            )

        if isinstance(result, UserField):
            if decl.f_default_val is not None:
                self.fields_to_lower.append(
                    self.FieldToLower(result, decl.f_default_val, scope)
                )
            elif isinstance(owner, ASTNodeType):
                check_source_language(
                    result.type.has_nullexpr,
                    f"{field_type.lkt_name} does not have a null value, so"
                    f" {result.qualname} must have a default value",
                    location=field_loc,
                )

        return result

    def lower_expr(
        self,
        expr: L.Expr,
        scope: Scope,
        prop: PropertyDef,
    ) -> E.Expr:
        """
        Lower the given expression, assumed to be the body for the given
        property.
        """
        return ExpressionCompiler(self.resolver, prop).lower_expr(expr, scope)

    def lower_static_expr(
        self,
        expr: L.Expr,
        t: CompiledType,
        scope: Scope,
    ) -> E.BindableLiteralExpr:
        """
        Lower the given expression, checking that it is a valid compile time
        known value of the given type.

        :param scope: Scope used to resolve references in this expression.
        """
        # We lower an expression out of a property (prop=None), so the
        # expression compiler checks that the expression is static. Only
        # BindableLiteralExpr expressions are static, so the assertion must
        # hold.
        result = ExpressionCompiler(self.resolver, prop=None).lower_expr(
            expr, scope
        )
        assert isinstance(result, E.BindableLiteralExpr)
        if result.type != t:
            error(
                f"Expected type {t.lkt_name}, got {result.type.lkt_name}",
                location=expr,
            )
        return result

    def create_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        rtype: CompiledType,
        lower_expr: Callable[[PropertyDef, Scope], E.Expr],
        location: Location,
        scope: Scope,
    ) -> PropertyDef:
        """
        Create an internal property.

        This is similar to ``lower_expr_to_internal_property``, but with a
        callback to get the lowered expression body.
        """
        result = PropertyDef(
            owner=node,
            names=MemberNames.for_internal(self.ctx, name),
            location=Location.builtin,
            expr=None,
            public=False,
            type=rtype,
            # Internal properties never have dynamic variables
            dynamic_vars=[],
        )
        result.location = location

        expr_scope = scope.create_child(
            f"scope for {node.lkt_name}'s env spec"
        )
        self.add_auto_property_arguments(result, expr_scope)

        # Property attributes are not computed yet, so it is too early to lower
        # the property body expression: defer it.

        def create_expr() -> E.Expr:
            self.reset_names_counter()
            with result.bind():
                expr = lower_expr(result, expr_scope)
            expr_scope.report_unused()
            return expr

        self.ctx.deferred.property_expressions.add(result, create_expr)
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
        rtype: CompiledType,
        expr: L.Expr | E.Expr,
        scope: Scope,
    ) -> PropertyDef: ...

    @overload
    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        rtype: CompiledType,
        expr: None,
        scope: Scope,
    ) -> None: ...

    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        rtype: CompiledType,
        expr: L.Expr | E.Expr | None,
        scope: Scope,
    ) -> PropertyDef | None:
        """
        Create an internal property to lower an expression.

        For convenience, accept a null body expression: return None in that
        case (create no property).

        :param node: Node for which we want to create this property.
        :param name: Name prefix, used to generate the actual property name.
        :param expr: Body for this proprety.
        :param rtype: Return type for this property.
        :param scope: Scope used to resolve references in this expression.
        """
        if expr is None:
            return None
        not_none_expr = expr

        def lower_expr(p: PropertyDef, scope: Scope) -> E.Expr:
            expr = not_none_expr

            # If the body is a Lkt expression, lower it. Use it unchanged
            # otherwise.
            return (
                self.lower_expr(expr, scope, p)
                if isinstance(expr, L.Expr)
                else expr
            )

        return self.create_internal_property(
            node,
            name,
            rtype,
            lower_expr,
            location=(
                Location.from_lkt_node(expr)
                if isinstance(expr, L.Expr)
                else Location.builtin
            ),
            scope=scope,
        )

    @staticmethod
    def add_auto_property_arguments(prop: PropertyDef, scope: Scope) -> None:
        """
        Add automatic arguments (``node`` and possibly ``self``, according to
        what is available to ``prop``'s body expression) to the given scope.
        """
        assert prop.has_node_var
        scope.mapping["node"] = Scope.BuiltinValue(
            "node", prop.node_var.ref_expr
        )
        if prop.has_self_var:
            scope.mapping["self"] = Scope.SelfVariable(
                "self", prop.self_var.ref_expr
            )

    def lower_property_arguments(
        self,
        prop: PropertyDef,
        arg_decl_list: L.FunParamDeclList | None,
        label: str,
        scope: Scope,
    ) -> tuple[list[L.FunParamDecl], Scope]:
        """
        Lower a property's arguments and create the root scope used to lower
        the property's root expression.

        :param scope: Scope used to resolve references in this property.
        """
        arguments: list[L.FunParamDecl] = []
        scope = scope.create_child(f"scope for {label}")
        self.add_auto_property_arguments(prop, scope)

        # Lower arguments and register them both in the property's argument
        # list and in the root property scope.
        for a in arg_decl_list or []:
            arguments.append(a)

            annotations = parse_annotations(
                self.ctx,
                FunArgAnnotations,
                a.f_decl_annotations,
                scope,
            )

            name = a.f_syn_name
            reserved = PropertyDef.reserved_arg_lower_names
            check_source_language(
                name.text not in reserved,
                "Arguments cannot have reserved names ({})".format(
                    ", ".join(reserved)
                ),
                location=name,
            )
            arg = Argument(
                Location.from_lkt_node(a),
                name=name_from_lower("argument", name),
                type=self.resolver.resolve_type(a.f_decl_type, scope),
            )
            prop.append_argument(arg)
            scope.add(
                Scope.Argument(name.text, a, arg.var),
                ignored=annotations.ignored,
            )

        return arguments, scope

    def lower_property(
        self,
        owner: CompiledType,
        full_decl: L.FullDecl,
        scope: Scope,
    ) -> PropertyDef:
        """
        Lower the property described in ``decl``.

        :param scope: Scope used to resolve references in this property.
        """
        from langkit.expressions.logic import PredicateErrorDiagnosticTemplate

        decl = full_decl.f_decl
        assert isinstance(decl, L.FunDecl)
        annotations = parse_annotations(
            self.ctx, FunAnnotations, full_decl, scope
        )
        return_type = self.resolver.resolve_type(decl.f_return_type, scope)

        external = False
        uses_entity_info: bool | None = None
        uses_envs: bool | None = None
        if annotations.external is not None:
            external = True
            uses_entity_info = annotations.external.uses_entity_info
            uses_envs = annotations.external.uses_envs

        check_source_language(
            not annotations.final or not annotations.abstract,
            "Final properties cannot be abstract",
            location=annotations.syn_annotations["abstract"],
        )

        # Create the property to return
        result = PropertyDef(
            owner=owner,
            names=MemberNames.for_property(
                owner,
                name_from_lower("field", decl.f_syn_name),
            ),
            location=Location.from_lkt_node(decl),
            expr=None,
            doc=lkt_doc(decl),
            # When the @export annotation is missing, use "None" to mean
            # "public status unspecified", as the property can still be public
            # thanks to inheritance.
            public=annotations.exported or None,
            abstract=annotations.abstract,
            type=return_type,
            memoized=annotations.memoized,
            call_memoizable=annotations.call_memoizable,
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
            has_property_syntax=annotations.property,
        )
        result._doc_location = Location.from_lkt_node_or_none(full_decl.f_doc)

        # If this property implements a generic interface method, keep track of
        # it: generic interface methods declarations are not lowered yet.
        if decl.f_trait_ref is not None:
            self.set_implemented_method(result, decl.f_trait_ref, scope)

        # Lower its arguments
        arguments, body_scope = self.lower_property_arguments(
            result, decl.f_params, f"property {result.qualname}", scope
        )
        if annotations.property and arguments:
            error(
                "the @property annotation is valid only for properties with no"
                " argument",
                location=annotations.syn_annotations["property"],
            )

        # Parse its predicate error template, if any
        if annotations.predicate_error:
            result.predicate_error = PredicateErrorDiagnosticTemplate.parse(
                result, annotations.predicate_error
            )

        # Plan to lower its expressions later
        self.properties_to_lower.append(
            self.PropertyToLower(
                decl, scope, result, arguments, annotations.with_dynvars
            )
            if decl.f_body is None
            else self.PropertyAndExprToLower(
                decl,
                scope,
                result,
                arguments,
                annotations.with_dynvars,
                decl.f_body,
                body_scope,
            )
        )

        return result

    def lower_env_spec(
        self,
        node: ASTNodeType,
        env_spec: L.EnvSpecDecl,
        scope: Scope,
    ) -> EnvSpec:
        """
        Lower an env spec for a node.

        :param node: Node for which we want to lower the env spec.
        :param env_spec: Env spec to lower.
        :param scope: Scope used to resolve references in this env spec.
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
                    context=self.ctx,
                    location=location,
                    no_parent=(
                        parse_static_bool(self.ctx, args["no_parent"])
                        if "no_parent" in args
                        else False
                    ),
                    transitive_parent=self.lower_expr_to_internal_property(
                        node,
                        "env_trans_parent",
                        T.Bool,
                        args.get("transitive_parent"),
                        scope,
                    ),
                    names=self.lower_expr_to_internal_property(
                        node,
                        "env_names",
                        T.Symbol.array,
                        args.get("names"),
                        scope,
                    ),
                )

            elif action_kind == "add_to_env_kv":
                args, _ = S.add_to_env_kv_signature.match(self.ctx, syn_action)

                def lower_prop_expr(
                    p: PropertyDef,
                    scope: Scope,
                    key: L.Expr,
                    value: L.Expr,
                    dest_env: L.Expr | None,
                    metadata: L.Expr | None,
                ) -> E.Expr:
                    """
                    Lower the body expression of the "mappings" internal
                    property.
                    """
                    key_expr = E.maybe_cast(
                        key, self.lower_expr(key, scope, p), T.Symbol
                    )
                    value_expr = E.maybe_cast(
                        value, self.lower_expr(value, scope, p), T.root_node
                    )
                    des_env = T.DesignatedEnv
                    assert isinstance(des_env, StructType)
                    des_env_kind = T.DesignatedEnvKind
                    assert isinstance(des_env_kind, EnumType)
                    dest_env_expr = (
                        E.New.StructExpr(
                            None,
                            des_env,
                            {
                                "kind": (
                                    des_env_kind.resolve_value(
                                        None, "current_env"
                                    )
                                ),
                                "env_name": E.NullExpr(None, T.Symbol),
                                "direct_env": E.NullExpr(None, T.LexicalEnv),
                            },
                        )
                        if dest_env is None
                        else E.maybe_cast(
                            dest_env,
                            self.lower_expr(dest_env, scope, p),
                            T.DesignatedEnv,
                        )
                    )
                    metadata_expr = (
                        E.NullExpr(None, T.env_md)
                        if metadata is None
                        else E.maybe_cast(
                            metadata,
                            self.lower_expr(metadata, scope, p),
                            T.env_md,
                        )
                    )
                    env_assoc_type = T.EnvAssoc
                    assert isinstance(env_assoc_type, StructType)
                    return E.New.StructExpr(
                        None,
                        env_assoc_type,
                        {
                            "key": key_expr,
                            "value": value_expr,
                            "dest_env": dest_env_expr,
                            "metadata": metadata_expr,
                        },
                    )

                action = AddToEnv(
                    context=self.ctx,
                    location=location,
                    mappings=self.create_internal_property(
                        node=node,
                        name="env_mappings",
                        rtype=T.EnvAssoc,
                        lower_expr=functools.partial(
                            lower_prop_expr,
                            key=args["key"],
                            value=args["value"],
                            dest_env=args.get("dest_env"),
                            metadata=args.get("metadata"),
                        ),
                        location=Location.from_lkt_node(syn_action),
                        scope=scope,
                    ),
                    resolver=self.resolver.resolve_property(
                        args.get("resolver"), scope
                    ),
                )

            elif action_kind == "add_single_to_env":
                args, _ = S.add_single_to_env_signature.match(
                    self.ctx, syn_action
                )

                action = AddToEnv(
                    context=self.ctx,
                    location=location,
                    mappings=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_mappings",
                        rtype=T.EnvAssoc,
                        expr=args["mapping"],
                        scope=scope,
                    ),
                    resolver=self.resolver.resolve_property(
                        args.get("resolver"), scope
                    ),
                )

            elif action_kind == "add_all_to_env":
                args, _ = S.add_all_to_env_signature.match(
                    self.ctx, syn_action
                )

                action = AddToEnv(
                    context=self.ctx,
                    location=location,
                    mappings=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_mappings",
                        rtype=T.EnvAssoc.array,
                        expr=args["mappings"],
                        scope=scope,
                    ),
                    resolver=self.resolver.resolve_property(
                        args.get("resolver"), scope
                    ),
                )

            elif action_kind == "do":
                args, _ = S.do_env_signature.match(self.ctx, syn_action)
                # The expression in "do" actions can have any type: use
                # NoCompiledType for now, and let property construction set the
                # type from the expression.
                action = Do(
                    context=self.ctx,
                    location=location,
                    expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_do",
                        rtype=T.NoCompiledType,
                        expr=args["expr"],
                        scope=scope,
                    ),
                )

            elif action_kind == "handle_children":
                args, _ = S.empty_signature.match(self.ctx, syn_action)
                action = HandleChildren(self.ctx, location)

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
                    context=self.ctx,
                    location=location,
                    resolver=self.resolver.resolve_property(
                        args["resolver"], scope
                    ),
                    nodes_expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="ref_env_nodes",
                        rtype=T.root_node.array,
                        expr=args["nodes"],
                        scope=scope,
                    ),
                    kind=kind,
                    dest_env=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_dest",
                        rtype=T.LexicalEnv,
                        expr=args.get("dest_env"),
                        scope=scope,
                    ),
                    cond=self.lower_expr_to_internal_property(
                        node=node,
                        name="ref_cond",
                        rtype=T.Bool,
                        expr=args.get("cond"),
                        scope=scope,
                    ),
                    category=category,
                    shed_rebindings=shed_rebindings,
                )

            elif action_kind == "set_initial_env":
                args, _ = S.set_initial_env_signature.match(
                    self.ctx, syn_action
                )
                action = SetInitialEnv(
                    context=self.ctx,
                    location=location,
                    env_expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_init",
                        rtype=T.DesignatedEnv,
                        expr=args["env"],
                        scope=scope,
                    ),
                )

            else:
                error("invalid env action name", location=syn_action.f_name)
            actions.append(action)

        result = EnvSpec(node, Location.from_lkt_node(env_spec), *actions)
        result.properties_created = True
        return result

    def defer_type_members(
        self,
        owner: CompiledType,
        base_type_ref: L.TypeRef | None,
        decls: L.DeclBlock,
        allowed_field_kinds: FieldKinds,
        scope: Scope,
    ) -> None:
        """
        Create deferred type members lowering for members found in the given
        ``DeclBlock`` node.

        :param ownner: The compiled type that owns these fields.
        :param base_type_ref: Reference node for ``owner``'s base type, if
            any.
        :param decls: Declarations to process.
        :param allowed_field_kinds: Set of field kinds allowed for the fields
            to load.
        :param scope: Scope used to resolve references in these members.
        """
        # Declaration nodes for fields and properties found in ``decls``
        member_decls: list[L.FullDecl] = []

        # Whether one env spec was found
        has_env_spec = False

        # Whether we have found a ``can_reach`` property
        has_can_reach = False

        for full_decl in decls:
            decl = full_decl.f_decl
            diag_ctx = DiagnosticContext(decl)

            # If this is actually an env spec, run the dedicated lowering code
            if isinstance(decl, L.EnvSpecDecl):
                if not isinstance(owner, ASTNodeType):
                    diag_ctx.error("env specs are allowed in nodes only")
                diag_ctx.check_source_language(
                    not has_env_spec,
                    "only one env_spec block allowed per type",
                )
                has_env_spec = True
                self.env_specs_to_lower.append((owner, decl, scope))
                continue

            # Otherwise, this is a field or a property
            if isinstance(decl, L.FunDecl):
                diag_ctx.check_source_language(
                    allowed_field_kinds.properties,
                    "Properties not allowed in this context",
                )
                member_decls.append(full_decl)
            else:
                member_decls.append(full_decl)

            if decl.f_syn_name.text == "can_reach":
                has_can_reach = True

        def fields_cb() -> list[AbstractNodeData]:
            result: list[AbstractNodeData] = []

            # Token nodes cannot have parse fields (inherited or not). Check
            # the inherited ones here (i.e. now that the base node is
            # guaranteed to have its list of fields populated), and leave
            # checks for fields specific to this type to fields lowering below.
            if (
                isinstance(owner, ASTNodeType)
                and owner.is_token_node
                and owner.base
            ):
                assert base_type_ref is not None
                parse_fields = owner.base.get_parse_fields()
                if parse_fields:
                    error(
                        "token nodes cannot inherit from nodes that have parse"
                        " fields",
                        location=base_type_ref,
                    )

            for full_decl in member_decls:
                if isinstance(full_decl.f_decl, L.FunDecl):
                    result.append(self.lower_property(owner, full_decl, scope))
                else:
                    result.append(
                        self.lower_base_field(
                            owner,
                            full_decl,
                            allowed_field_kinds,
                            scope,
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

    def create_node(
        self,
        decl: L.BasicClassDecl,
        annotations: BaseNodeAnnotations,
        scope: Scope,
    ) -> ASTNodeType:
        """
        Create an ASTNodeType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        :param scope: Scope used to resolve references in this type
            declaration.
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
            diag_ctx = DiagnosticContext(trait_ref)
            if isinstance(trait_ref, L.SimpleTypeRef):
                if trait_ref.text == "TokenNode":
                    token_node_trait_ref = trait_ref

                elif trait_ref.text == "ErrorNode":
                    error_node_trait_ref = trait_ref

                else:
                    generic_interfaces.append(
                        self.resolver.resolve_generic_interface(
                            trait_ref.f_type_name, scope
                        )
                    )

            elif not isinstance(trait_ref, L.GenericTypeRef):
                diag_ctx.error("Nodes cannot implement this trait")

            else:
                # This is a generic instantiation
                generic_trait = trait_ref.f_type_name
                type_args = list(trait_ref.f_args)
                if generic_trait.text == "Node":
                    # If this trait is an instantiation of the Node trait, make
                    # sure it is instantiated on the root node itself (i.e.
                    # "decl").
                    decl_name = decl.f_syn_name.text
                    diag_ctx.check_source_language(
                        len(type_args) == 1 and type_args[0].text == decl_name,
                        "The Node generic trait must be instantiated with the"
                        f" root node ({decl_name})",
                    )
                    node_trait_ref = trait_ref

                else:
                    diag_ctx.error("Nodes cannot implement this trait")

        def check_trait(
            trait_ref: L.LktNode | None, expected: bool, message: str
        ) -> None:
            """
            If ``expected`` is ``True``, emit an error if ``trait_ref`` is
            ``None``. If ``expected`` is ``False``, emit an error if
            ``trait_ref`` is not ``None``. In both cases, use ``message`` as
            the error message.
            """
            if expected:
                check_source_language(
                    trait_ref is not None, message, location=decl
                )
            elif trait_ref is not None:
                error(message, location=trait_ref)

        # Root node case
        base_type_node = decl.p_base_type
        if base_type_node is None:
            check_trait(
                node_trait_ref,
                True,
                "The root node must implement the Node trait",
            )
            check_trait(
                token_node_trait_ref,
                False,
                "The root node cannot be a token node",
            )
            check_trait(
                error_node_trait_ref,
                False,
                "The root node cannot be an error node",
            )

            if self.ctx.has_root_node_type:
                error(
                    "There can be only one root node"
                    f" ({self.ctx.root_node_type.lkt_name})",
                    location=decl,
                )

            base_type = None
            is_token_node = is_error_node = False
        else:
            base_type = self.resolve_base_node(base_type_node, scope)

            check_source_language(
                annotations.generic_list_type is None,
                "Only the root AST node can hold the name of the generic list"
                " type",
                location=annotations.syn_annotations["generic_list_type"],
            )
            check_source_language(
                not base_type.is_generic_list_type,
                "Only root list types can derive from the generic list type",
                location=base_type_node,
            )

            check_trait(
                node_trait_ref,
                False,
                "Only the root node can implement the Node trait",
            )

            # This is a token node if either the TokenNode trait is implemented
            # or if the base node is a token node itself. Likewise for
            # ErrorNode.
            is_token_node = (
                token_node_trait_ref is not None or base_type.is_token_node
            )
            is_error_node = (
                error_node_trait_ref is not None or base_type.is_error_node
            )

            check_source_language(
                base_type is not base_type.is_enum_node,
                "Inheritting from an enum node is forbidden",
                location=base_type_node,
            )

        # Determine whether this node is abstract. Remember that base enum node
        # types are abstract (it is their derivations that are concrete).
        is_abstract = (
            not isinstance(annotations, NodeAnnotations)
            or annotations.abstract
        )

        # Determine whether this node is synthetic
        is_synthetic = annotations.synthetic

        if is_error_node:
            diag_ctx = DiagnosticContext(
                error_node_trait_ref
                if error_node_trait_ref
                else base_type_node
            )

            if is_abstract:
                diag_ctx.error("Error nodes cannot be abstract")
            if is_synthetic:
                diag_ctx.error("Error nodes cannot be synthetic")

            if base_type and base_type.is_list:
                diag_ctx.error("Error nodes cannot be lists")

            if is_token_node:
                diag_ctx.error("Error nodes cannot be token nodes")

        is_bool_node = (
            isinstance(annotations, EnumNodeAnnotations)
            and annotations.qualifier
        )

        result = ASTNodeType(
            self.ctx,
            name_from_camel("node type", decl.f_syn_name),
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
            type_name = result.generic_list_type.lkt_name
            self.root_scope.mapping[type_name] = Scope.BuiltinType(
                type_name, result.generic_list_type
            )

        # Lower fields. Regular nodes can hold all types of fields, but token
        # nodes and enum nodes can hold only user field and properties.
        self.defer_type_members(
            result,
            base_type_node,
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
            scope=scope,
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
                qualifier=annotations.qualifier,
            )

        if is_error_node:
            self.error_nodes.append(result)

        return result

    def create_enum_node_alternatives(
        self,
        alternatives: list[L.EnumClassAltDecl],
        enum_node: ASTNodeType,
        qualifier: bool,
    ) -> None:
        """
        Create ASTNodeType instances for the alternatives of an enum node.

        :param alternatives: Declarations for the alternatives to lower.
        :param enum_node: Enum node that owns these alternatives.
        :param qualifier: Whether this enum node has the "@qualifier"
            annotation.
        """
        enum_node._alternatives = []
        enum_node._alternatives_map = {}

        # All enum classes must have at least one alternative, except those
        # with the "@qualifier" annotation, which implies automatic
        # alternatives.
        if qualifier:
            if alternatives:
                error(
                    "Enum nodes with @qualifier cannot have explicit"
                    " alternatives",
                    location=alternatives[0],
                )
            alt_descriptions = [
                EnumNodeAlternative(
                    names.Name(alt_name), enum_node, None, enum_node.location
                )
                for alt_name in ("Present", "Absent")
            ]
        else:
            check_source_language(
                len(alternatives) > 0,
                "Missing alternatives for this enum node",
                location=enum_node.location,
            )
            alt_descriptions = [
                EnumNodeAlternative(
                    name_from_camel("enum node alternative", alt.f_syn_name),
                    enum_node,
                    None,
                    Location.from_lkt_node(alt),
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
                doc="",
                base=enum_node,
                lkt_name="{}.{}".format(
                    enum_node.lkt_name, alt.base_name.camel
                ),
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
                    ),
                )

        # Finally create enum node-local indexes to easily fetch the
        # ASTNodeType instances later on.
        enum_node._alternatives = alt_nodes
        enum_node._alternatives_map = {
            alt.base_name.camel: alt_node
            for alt, alt_node in zip(alt_descriptions, alt_nodes)
        }

    def create_enum(
        self, decl: L.EnumTypeDecl, annotations: EnumAnnotations
    ) -> EnumType:
        """
        Create an EnumType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        # Decode the list of enum literals and validate them
        value_names = []
        for lit in decl.f_literals:
            name = name_from_lower("enum value", lit.f_syn_name)
            check_source_language(
                name not in value_names,
                "This literal is present twice",
                location=lit,
            )
            value_names.append(name)

        # If present, validate the default value
        default_value: names.Name | None = None
        default_expr = annotations.with_default
        if default_expr is not None:
            if not isinstance(default_expr, L.RefId):
                error("enum value identifier expected", location=default_expr)
            default_value = names.Name.from_lower(default_expr.text)
            if default_value not in value_names:
                error("no such value in this enum", location=default_expr)

        result = EnumType(
            self.ctx,
            name=name_from_camel("enum type", decl.f_syn_name),
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

    def create_struct(
        self,
        decl: L.StructDecl,
        annotations: StructAnnotations,
        scope: Scope,
    ) -> StructType:
        """
        Create a StructType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        :param scope: Scope used to resolve references in this struct
            declaration.
        """
        # Check the set of traits that this node implements
        generic_interfaces: list[GenericInterface] = []
        for trait_ref in decl.f_traits:
            if isinstance(trait_ref, L.SimpleTypeRef):
                generic_interfaces.append(
                    self.resolver.resolve_generic_interface(
                        trait_ref.f_type_name, scope
                    )
                )
            else:
                error(
                    "Structs cannot implement this trait", location=trait_ref
                )

        result = StructType(
            self.ctx,
            name_from_camel("struct type", decl.f_syn_name),
            location=Location.from_lkt_node(decl),
            doc=lkt_doc(decl),
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
        )
        if annotations.metadata:
            assert not self.ctx.has_env_metadata
            check_source_language(
                result.lkt_name == "Metadata",
                "The environment metadata struct type must be called"
                f' "Metadata" (here: {result.lkt_name})',
                location=decl,
            )
            self.ctx.env_metadata = result
            self.ctx.has_env_metadata = True

        # Lower fields
        self.defer_type_members(
            result,
            None,
            decl.f_decls,
            allowed_field_kinds=(
                FieldKinds(metadata_fields=True)
                if annotations.metadata
                else FieldKinds(user_fields=True)
            ),
            scope=scope,
        )

        # Register the generic interfaces that this type implements
        self.ctx.deferred.implemented_interfaces.add(
            result, lambda: generic_interfaces
        )

        return result

    def check_env_metadata(self) -> None:
        """
        Perform legality checks on the env metadata struct.
        """
        for field in self.ctx.env_metadata.get_fields():
            check_source_language(
                field.type.is_bool_type or field.type.is_ast_node,
                "Environment metadata fields can be only booleans or nodes",
                location=field.location,
            )

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

        self.register_generic_interface(
            decl, annotations.generic_interface, self.root_scope
        )

    def process_user_trait(self, decl: L.TraitDecl, scope: Scope) -> None:
        """
        Process a trait declared in user code.

        :param decl: Trait declaration to process.
        :param scope: Scope used to resolve references in this trait
            declaration.
        """
        full_decl = decl.parent
        assert isinstance(full_decl, L.FullDecl)

        # The only traits that are supported there are generic interfaces
        annotations = parse_annotations(
            self.ctx, TraitAnnotations, full_decl, scope
        )
        if annotations.generic_interface is None:
            error(
                "only generic interface traits are allowed",
                location=decl,
            )

        self.register_generic_interface(
            decl, annotations.generic_interface, scope
        )

    def register_generic_interface(
        self,
        decl: L.TraitDecl,
        annotations: GenericInterfaceAnnotationSpec.Value,
        scope: Scope,
    ) -> None:
        """
        Create a generic interface and schedule the lowering of their members
        later.

        :param decl: Trait declaration for this generic interface.
        :param annotations: Annotations for this generic interface.
        :param scope: Scope used to resolve references in this trait
            declaration.
        """
        # Create the GenericInterface instance itself
        name = name_from_camel("generic_interface", decl.f_syn_name).camel
        gen_iface = GenericInterface(
            name=name,
            ctx=self.ctx,
            is_always_node=annotations.node_only,
            doc=lkt_doc(decl),
        )

        # Register it in the root scope
        entity = Scope.GenericInterface(
            name=name,
            diagnostic_node=decl,
            generic_interface=gen_iface,
        )
        scope.add(entity)

        # Reject generic interfaces, even if they are declared in different
        # scopes.
        self.resolver.global_scope.add(entity)

        # Schedule the lowering of its member in the
        # GENERIC_INTERFACE_MEMBERS_LOWERING pass, when all compiled types will
        # be known.
        self.gen_iface_decls.append((gen_iface, decl))

    def lower_generic_interface_members(
        self,
        gen_iface: GenericInterface,
        decl: L.TraitDecl,
        scope: Scope,
    ) -> None:
        """
        Lower all the members of the given generic interface.

        :param gen_iface: Generic interface whose members must be lowered.
        :param decl: Lkt parse node for the generic interface itself. The
            members to lower are searched from there.
        :param scope: Scope used to resolve references in this member
            declaration.
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
                "function", member_decl.f_syn_name
            ).lower
            method_doc = lkt_doc(member_decl)
            return_type = self.resolver.resolve_type_or_gen_iface(
                member_decl.f_return_type, scope
            )
            args: list[GenericArgument] = []
            for a in member_decl.f_params:
                check_no_annotations(a.f_decl_annotations)
                if a.f_default_val is not None:
                    error(
                        "default argument values not allowed in generic"
                        " interfaces",
                        location=a.f_default_val,
                    )
                args.append(
                    GenericArgument(
                        name=name_from_lower("argument", a.f_syn_name).lower,
                        type=self.resolver.resolve_type_or_gen_iface(
                            a.f_decl_type, scope
                        ),
                    )
                )

            # Finally register the method in its owning generic interfac
            gen_iface.add_method(method_name, args, return_type, method_doc)

    def set_implemented_method(
        self,
        member: AbstractNodeData,
        method_name: L.DotExpr,
        scope: Scope,
    ) -> None:
        """
        Mark the generic interface designated by ``method_name`` as implemented
        by ``member``.

        :param scope: Scope used to resolve the reference to the generic
            interface method.
        """
        self.ctx.deferred.implemented_methods.add(
            member,
            lambda: self.resolver.resolve_generic_interface_method(
                method_name, scope
            ),
        )


def create_types(resolver: Resolver) -> None:
    """
    Create types from Lktlang units.

    :param resolver: Resolver for Lkt entities.
    """
    loader = LktTypesLoader(resolver)
    resolver.context.lkt_types_loader = loader
