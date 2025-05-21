from __future__ import annotations

import dataclasses
import os.path
from typing import overload

from langkit.compile_context import CompileCtx
from langkit.compiled_types import ASTNodeType, CompiledType, T
from langkit.diagnostics import Location, error
from langkit.envs import RefKind
import langkit.expressions as E
from langkit.frontend.scopes import Scope
from langkit.generic_interface import (
    BaseGenericInterface,
    GenericInterface,
    InterfaceMethodProfile,
)

import liblktlang as L


@dataclasses.dataclass(frozen=True)
class Builtins:
    """
    Holder for Lkt builtin entities. These are stored in scopes, but having
    them in dataclasses is convenient for direct access in Lkt lowering code.
    """

    @dataclasses.dataclass(frozen=True)
    class Generics:
        ast_list: Scope.Generic
        array: Scope.Generic
        entity: Scope.Generic
        iterator: Scope.Generic
        node: Scope.Generic
        node_builder: Scope.Generic
        set: Scope.Generic

    @dataclasses.dataclass(frozen=True)
    class Functions:
        dynamic_lexical_env: Scope.BuiltinFunction

    @dataclasses.dataclass(frozen=True)
    class DynVars:
        error_location: Scope.BuiltinDynVar
        logic_context: Scope.BuiltinDynVar

    @dataclasses.dataclass(frozen=True)
    class Exceptions:
        precondition_failure: Scope.Exception
        property_error: Scope.Exception

    generics: Generics
    functions: Functions
    dyn_vars: DynVars
    exceptions: Exceptions

    @classmethod
    def create(cls, context: CompileCtx, root_scope: Scope) -> Builtins:
        result = Builtins(
            cls.Generics(
                Scope.Generic("ASTList"),
                Scope.Generic("Array"),
                Scope.Generic("Entity"),
                Scope.Generic("Iterator"),
                Scope.Generic("Node"),
                Scope.Generic("NodeBuilder"),
                Scope.Generic("Set"),
            ),
            cls.Functions(Scope.BuiltinFunction("dynamic_lexical_env")),
            cls.DynVars(
                Scope.BuiltinDynVar(
                    "error_location",
                    E.DynamicVariable(Location.builtin, "error_location"),
                ),
                Scope.BuiltinDynVar(
                    "logic_context",
                    E.DynamicVariable(Location.builtin, "logic_context"),
                ),
            ),
            cls.Exceptions(
                Scope.Exception("PreconditionFailure"),
                Scope.Exception("PropertyError"),
            ),
        )

        context.deferred.dynamic_variable_types.add(
            result.dyn_vars.error_location.variable,
            lambda: context.root_node_type,
        )
        context.deferred.dynamic_variable_types.add(
            result.dyn_vars.logic_context.variable, lambda: T.LogicContext
        )

        def builtin_type(name: str) -> Scope.BuiltinType:
            """
            Create a builtin type for scopes.

            :param name: Name for this type in scopes.
            """
            return Scope.BuiltinType(name, getattr(T, name))

        # Register builtins in the root scope
        for builtin in [
            builtin_type("Address"),
            builtin_type("AnalysisUnit"),
            builtin_type("AnalysisUnitKind"),
            builtin_type("BigInt"),
            builtin_type("Bool"),
            builtin_type("Char"),
            builtin_type("CompletionItemKind"),
            builtin_type("DesignatedEnv"),
            builtin_type("DesignatedEnvKind"),
            builtin_type("EntityInfo"),
            builtin_type("EnvAssoc"),
            builtin_type("EnvRebindings"),
            builtin_type("Equation"),
            builtin_type("InnerEnvAssoc"),
            builtin_type("Int"),
            builtin_type("LexicalEnv"),
            builtin_type("LogicContext"),
            builtin_type("LogicVar"),
            builtin_type("LookupKind"),
            builtin_type("RefCategories"),
            builtin_type("SolverDiagnostic"),
            builtin_type("SolverResult"),
            builtin_type("SourceLocation"),
            builtin_type("SourceLocationRange"),
            builtin_type("String"),
            builtin_type("Symbol"),
            builtin_type("Token"),
            Scope.BuiltinValue("false", E.BooleanLiteralExpr(None, False)),
            Scope.BuiltinValue("true", E.BooleanLiteralExpr(None, True)),
            result.dyn_vars.error_location,
            result.dyn_vars.logic_context,
            result.exceptions.precondition_failure,
            result.exceptions.property_error,
            result.generics.ast_list,
            result.generics.array,
            result.generics.entity,
            result.generics.iterator,
            result.generics.node,
            result.generics.node_builder,
            result.generics.set,
            Scope.Trait("ErrorNode"),
            Scope.Trait("TokenNode"),
            result.functions.dynamic_lexical_env,
        ]:
            root_scope.mapping[builtin.name] = builtin

        return result


class Resolver:
    """
    Collection of helpers to resolve references to entities in Lkt sources.
    """

    def __init__(self, context: CompileCtx, lkt_units: list[L.AnalysisUnit]):
        """
        :param context: Context for which Lkt sources are lowered.
        :param lkt_units: Non-empty list of analysis units where to look for
            declarations.
        """
        self.context = context
        self.lkt_units = lkt_units

        #
        # ROOT_SCOPE_CREATION
        #

        self.root_scope = Scope("the root scope", context)
        self.builtins = Builtins.create(context, self.root_scope)

        # Create a special scope to resolve the "kind" argument for
        # "reference()" env actions.
        self.refd_env_scope = Scope("builtin scope", context)
        for ref_kind_value in RefKind:
            self.refd_env_scope.mapping[ref_kind_value.name] = (
                Scope.RefKindValue(ref_kind_value.name, ref_kind_value)
            )

    @property
    def root_lkt_source_loc(self) -> Location:
        """
        Location to use when reporting that something is missing in the Lkt
        source code.

        Such errors are reported on the entry point Lkt source file (no
        particular line).
        """
        return Location(file=self.lkt_units[0].filename)

    def find_toplevel_decl(
        self,
        node_type: type,
        label: str,
    ) -> L.FullDecl:
        """
        Look for a top-level declaration of type ``node_type`` in the Lkt
        units.

        If none or several are found, emit error diagnostics. Return the
        associated full declaration.

        :param node_type: Node type to look for.
        :param label: Human readable string for what to look for. Used to
            create diagnostic mesages.
        """
        result = None
        for unit in self.lkt_units:
            assert isinstance(unit.root, L.LangkitRoot)
            for decl in unit.root.f_decls:
                if not isinstance(decl.f_decl, node_type):
                    continue

                if result is not None:
                    error(
                        f"only one {label} allowed (previous found at"
                        f" {os.path.basename(result.unit.filename)}"
                        f":{result.sloc_range.start})",
                        location=decl,
                    )
                result = decl

        if result is None:
            error(f"missing {label}", location=self.root_lkt_source_loc)

        return result

    def resolve_entity(self, name: L.Expr, scope: Scope) -> Scope.Entity:
        """
        Resolve the entity designated by ``name`` in the given scope.
        """
        return scope.resolve(name)

    def resolve_generic(self, name: L.Expr, scope: Scope) -> Scope.Generic:
        """
        Like ``resolve_entity``, but for generics specifically.
        """
        result = self.resolve_entity(name, scope)
        if isinstance(result, Scope.Generic):
            return result
        else:
            error(
                f"generic expected, got {result.diagnostic_name}",
                location=name,
            )

    def resolve_generic_interface(
        self,
        name: L.Expr,
        scope: Scope,
    ) -> GenericInterface:
        """
        Like ``resolve_entity``, but for generic interfaces specifically.
        """
        result = self.resolve_entity(name, scope)
        if isinstance(result, Scope.GenericInterface):
            return result.generic_interface
        else:
            error(
                f"generic interface expected, got {result.diagnostic_name}",
                location=name,
            )

    def resolve_generic_interface_method(
        self,
        name: L.DotExpr,
        scope: Scope,
    ) -> InterfaceMethodProfile:
        """
        Resolve the generic interface method designated by ``name`` in the
        given scope.
        """
        generic_interface = self.resolve_generic_interface(
            name.f_prefix, scope
        )
        try:
            return generic_interface.methods[name.f_suffix.text]
        except KeyError:
            error(
                f"{generic_interface.name.camel} has no {name} method",
                location=name.f_suffix,
            )

    def resolve_type_or_gen_iface(
        self,
        name: L.TypeRef,
        scope: Scope,
    ) -> CompiledType | BaseGenericInterface:
        """
        Like ``resolve_entity``, but for types or generic interface types
        specifically.
        """
        if isinstance(name, L.GenericTypeRef):
            generic = self.resolve_generic(name.f_type_name, scope)
            type_args = list(name.f_args)
            if generic == self.builtins.generics.ast_list:
                if len(type_args) != 1:
                    error(
                        f"{generic.name} expects one type argument: the list"
                        " element type",
                        location=name,
                    )
                (element_type,) = type_args

                # Check that the element type is a node and that the designated
                # root node is indeed the root node.
                return self.resolve_node(element_type, scope).list

            elif generic == self.builtins.generics.array:
                if len(type_args) != 1:
                    error(
                        f"{generic.name} expects one type argument: the"
                        " element type",
                        location=name,
                    )
                (element_type,) = type_args
                return self.resolve_type_or_gen_iface(
                    element_type, scope
                ).array

            elif generic == self.builtins.generics.set:
                if len(type_args) != 1:
                    error(
                        f"{generic.name} expects one type argument: the"
                        " element type",
                        location=name,
                    )
                (element_type,) = type_args
                return self.resolve_type(element_type, scope).set

            elif generic == self.builtins.generics.entity:
                if len(type_args) != 1:
                    error(
                        f"{generic.name} expects one type argument: the node"
                        " type",
                        location=name,
                    )
                (node_type,) = type_args
                return self.resolve_node(node_type, scope).entity

            elif generic == self.builtins.generics.iterator:
                if len(type_args) != 1:
                    error(
                        f"{generic.name} expects one type argument: the"
                        " element type",
                        location=name,
                    )
                (element_type,) = type_args
                return self.resolve_type(element_type, scope).iterator

            elif generic == self.builtins.generics.node:
                error(
                    "this generic trait is supposed to be used only in the"
                    " 'implements' part of the root node type declaration",
                    location=name,
                )

            elif generic == self.builtins.generics.node_builder:
                if len(type_args) != 1:
                    error(
                        f"{generic.name} expects one type argument: the node"
                        " type",
                        location=name,
                    )
                (node_type,) = type_args
                return self.resolve_node(node_type, scope).builder_type

            else:
                # User code cannot define new generics, so there cannot
                # possibly be other generics.
                assert False

        elif isinstance(name, L.SimpleTypeRef):
            # The only generic interface type possible is by-name: just look
            # for a generic interface entity in the given scope. If not found,
            # it must be a compiled type.
            type_name = name.f_type_name
            if isinstance(type_name, L.RefId):
                entity = self.resolve_entity(type_name, scope)
                if isinstance(entity, Scope.GenericInterface):
                    return entity.generic_interface
            return self.resolve_type_expr(type_name, scope)

        else:
            error("invalid type reference", location=name)

    def resolve_type(self, name: L.TypeRef, scope: Scope) -> CompiledType:
        """
        Like ``resolve_entity``, but for compiled types specifically.
        """
        result = self.resolve_type_or_gen_iface(name, scope)
        if isinstance(result, BaseGenericInterface):
            error(
                "specific type expected, got a generic interface type",
                location=name,
            )
        else:
            return result

    def resolve_node(self, name: L.TypeRef, scope: Scope) -> ASTNodeType:
        """
        Like ``resolve_type``, but checks that the resolved type is a node.
        """
        result = self.resolve_type(name, scope)
        if isinstance(result, ASTNodeType):
            return result
        else:
            error("node expected", location=name)

    def resolve_type_expr(self, name: L.Expr, scope: Scope) -> CompiledType:
        """
        Like ``resolve_type``, but working on a type expression directly.
        """
        if isinstance(name, L.RefId):
            entity = self.resolve_entity(name, scope)
            if isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                return entity.t
            else:
                error(
                    f"type expected, got {entity.diagnostic_name}",
                    location=name,
                )

        elif isinstance(name, L.DotExpr):
            # This must be a reference to an enum node:
            # "EnumNode.Alternative".
            dot_expr = name
            prefix = self.resolve_type_expr(dot_expr.f_prefix, scope)
            suffix = dot_expr.f_suffix

            if (
                # Make sure that prefix is an enum node...
                not isinstance(prefix, ASTNodeType)
                or not prefix.is_enum_node
                # ... and not an enum node alternative
                or prefix.base is None
                or prefix.base.is_enum_node
            ):
                error("base enum node expected", location=dot_expr.f_prefix)

            try:
                return prefix._alternatives_map[suffix.text]
            except KeyError:
                error("no such alternative", location=suffix)

        else:
            error("invalid type reference", location=name)

    def resolve_node_type_expr(
        self,
        name: L.Expr,
        scope: Scope,
    ) -> ASTNodeType:
        """
        Like ``resolve_node``, but working on a type expression directly.
        """
        result = self.resolve_type_expr(name, scope)
        if isinstance(result, ASTNodeType):
            return result
        else:
            error("node expected", location=name)

    @overload
    def resolve_property(self, name: L.Expr) -> E.PropertyDef: ...

    @overload
    def resolve_property(self, name: None) -> None: ...

    def resolve_property(self, name: L.Expr | None) -> E.PropertyDef | None:
        """
        Like ``resolve_entity``, but for properties specifically.
        """
        if name is None:
            return None

        if not isinstance(name, L.DotExpr):
            error(
                "invalid reference to a property (should be:"
                " ``T.property_name``)",
                location=name,
            )

        prefix = self.resolve_node_type_expr(name.f_prefix, self.root_scope)
        suffix_node = name.f_suffix

        member = prefix.get_abstract_node_data_dict().get(
            suffix_node.text, None
        )
        if member is None:
            error(
                f"no such member for {prefix.lkt_name}",
                location=suffix_node,
            )
        elif not isinstance(member, E.PropertyDef):
            error(
                f"property expected, got a {member.kind_name}",
                location=suffix_node,
            )
        return member
