from __future__ import annotations

import abc
import dataclasses
from typing import ClassVar

from langkit.compile_context import CompileCtx
from langkit.compiled_types import CompiledType
from langkit.diagnostics import Location, WarningSet, error
from langkit.envs import RefKind
import langkit.expressions as E
from langkit.generic_interface import GenericInterface

import liblktlang as L


class Scope:
    """
    Scope data structure, use to resolve named references.
    """

    @dataclasses.dataclass
    class Entity(metaclass=abc.ABCMeta):
        """
        Object that is registered in a scope.
        """

        name: str

        @abc.abstractproperty
        def diagnostic_name(self) -> str:
            """
            Name for this entity to use when creating diagnostics.
            """
            ...

    class BuiltinEntity(Entity):
        """
        Any entity that is created automatically by Lkt.
        """

        pass

    class BuiltinFunction(BuiltinEntity):
        """
        Builtin function, used to expose a DSL operation.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin function {self.name}"

    @dataclasses.dataclass
    class BuiltinType(BuiltinEntity):
        """
        Type created automatically by Lkt.
        """

        t: CompiledType
        """
        Reference to the corresponding compiled type.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin type {self.name}"

    @dataclasses.dataclass
    class BuiltinValue(BuiltinEntity):
        """
        Named value created automatically by Lkt.
        """

        value: E.Expr
        """
        Value to use for this during expression lowering.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin value {self.value}"

    @dataclasses.dataclass
    class SelfVariable(BuiltinValue):
        """
        Binding for the automatic "self" variable.
        """

    @dataclasses.dataclass
    class BuiltinDynVar(BuiltinEntity):
        """
        Dynamic variable created automatically by Lkt.
        """

        variable: E.DynamicVariable

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin dynamic variable {self.variable.lkt_name}"

    class Generic(BuiltinEntity):
        """
        Generic declaration, always created automatically by Lkt.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the generic {self.name}"

    class Trait(BuiltinEntity):
        """
        Trait declaration, always created automatically by Lkt.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the trait {self.name}"

    @dataclasses.dataclass
    class Exception(BuiltinEntity):
        """
        Exception type, always created automatically by Lkt.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the exception {self.name}"

    @dataclasses.dataclass
    class RefKindValue(BuiltinEntity):
        """
        Reference kinds in "reference()" env actions.
        """

        value: RefKind

        @property
        def diagnostic_name(self) -> str:
            return f"the reference kind {self.name}"

    @dataclasses.dataclass
    class UserEntity(Entity):
        """
        Entity defined in user code.
        """

        diagnostic_node: L.LktNode
        """
        Lkt node to use as the reference for this entity when creating
        diagnostics: generally a declaration (VarDecl for a variable
        declaration, TypeDecl for a type declaration, ...), sometimes not
        (VarBind for a bound dynamic variable).
        """

        kind_name: ClassVar[str]
        """
        Name for the kind of this entity, to use when formatting diagnostics.
        """

        @property
        def diagnostic_name(self) -> str:
            loc = Location.from_lkt_node(self.diagnostic_node)
            return (
                f"the {self.kind_name} {self.name} at {loc.gnu_style_repr()}"
            )

    class Lexer(UserEntity):
        """
        Lexer declaration.
        """

        kind_name = "lexer"

    class Grammar(UserEntity):
        """
        Grammar declaration.
        """

        kind_name = "grammar"

    @dataclasses.dataclass
    class UserType(UserEntity):
        """
        Type declaration.
        """

        t: CompiledType
        """
        Reference to the corresponding compiled type.
        """

        kind_name = "type"

    @dataclasses.dataclass
    class UserValue(UserEntity):
        """
        Value declaration.
        """

        variable: E.Expr
        """
        Value to use for this during expression lowering.
        """

        kind_name = "named value"

    class LocalVariable(UserValue):
        """
        Local variable declaration.
        """

        kind_name = "local variable"

    @dataclasses.dataclass
    class BoundDynVar(LocalVariable):
        """
        Dynamic variable that has been bound.

        ``BoundDynVar`` instances are put in a scope as soon as a dynamic
        variable is bound in that scope: it allows keep track of the fact that
        it is bound (to be used as regular variables, hence the
        ``LocalVariable`` derivation).
        """

        dyn_var: E.DynamicVariable

        kind_name = "dynamic variable binding"

    class Argument(UserValue):
        """
        Function argument declaration.
        """

        kind_name = "argument"

    @dataclasses.dataclass
    class DynVar(UserEntity):
        """
        Dynamic variable declaration.

        Note that this is not a derivation of ``UserValue`` since dynamic
        variables cannot be used as-is: they first need to be bound either
        explicitly through a ``VarBind`` expression or with the
        ``@with_dynvars`` property annotation.
        """

        variable: E.DynamicVariable

        kind_name = "dynamic variable"

    @dataclasses.dataclass
    class GenericInterface(UserEntity):
        """
        Generic interface declaration.
        """

        generic_interface: GenericInterface
        kind_name = "generic interface"

    def __init__(
        self,
        label: str,
        context: CompileCtx,
        parent: Scope | None = None,
    ):
        """
        :param label: Label for this scope to use when formatting diagnostics.
        :param context: Current compilation context.
        :param parent: Optional parent scope. When looking for an entity by
            name, the search escalates to the parent if we cannot find the
            entity in this scope.
        """
        self.label = label
        self.context = context
        self.parent = parent
        self.mapping: dict[str, Scope.Entity] = {}

        self.ignored: set[str] = set()
        """
        Set of names for entities meant to be unused in this scope.
        """

        self.looked_up: set[str] = set()
        """
        Set of names for entities that have been looked up in this scope.
        """

    def add(self, entity: Scope.UserEntity, ignored: bool = False) -> None:
        """
        Add a declaration to the current scope.

        Stop with a user-level error if there is already a declaration with the
        same name in this scope.

        :param ignored: Whether this entity is declared as ignored: a warning
            will be emitted if it is returned by a lookup.
        """
        # Pseudo "_" entities are used to mean "do not bind this to an
        # identifier": just skip them.
        if entity.name == "_":
            return

        # Reject homonym entities in the same scope
        other_entity = self.mapping.get(entity.name)
        if other_entity is not None:
            other_label = (
                other_entity.diagnostic_name
                if isinstance(other_entity, Scope.UserEntity)
                else "a builtin"
            )
            error(
                f"this declaration conflicts with {other_label}",
                location=entity.diagnostic_node,
            )

        self.mapping[entity.name] = entity
        if ignored:
            self.ignored.add(entity.name)

    def lookup(
        self,
        name: str,
        location: Location | L.LktNode | None = None,
    ) -> Scope.Entity:
        """
        Look for the declaration for a given name in this scope or one of its
        parents. Raise a ``KeyError`` exception if there is no such
        declaration.

        :param location: If it is possible for this lookup to return an ignored
            entity, a Location is required to give context for the
            corresponding warning.
        """
        scope: Scope | None = self
        while scope is not None:
            try:
                result = scope.mapping[name]
            except KeyError:
                scope = scope.parent
            else:
                scope.looked_up.add(name)
                if (
                    isinstance(result, Scope.UserEntity)
                    and name in self.ignored
                ):
                    assert location is not None
                    WarningSet.unused_bindings.warn_if(
                        True,
                        f"ignored {result.kind_name} is actually used",
                        location=location,
                    )
                return result

        raise KeyError(f"no entity called '{name}' in {self.label}")

    def resolve(self, name: L.Expr) -> Scope.Entity:
        """
        Resolve the entity designated by ``name`` in this scope.

        Unlike ``lookup``, this create a diagnostic if the entity is not found.
        """
        if isinstance(name, L.RefId):
            try:
                return self.lookup(name.text, name)
            except KeyError as exc:
                error(exc.args[0], location=name)
        else:
            error("invalid entity reference", location=name)

    def create_child(self, label: str) -> Scope:
        """
        Return a new scope whose ``self`` is the parent.
        """
        return Scope(label, self.context, self)

    def report_unused(self) -> None:
        """
        Emit a warning for all user entities registered in this scope that were
        not looked up ("unused bindings").
        """
        unused = set(self.mapping) - self.ignored - self.looked_up
        for name in unused:
            entity = self.mapping[name]
            if isinstance(entity, Scope.UserEntity):
                WarningSet.unused_bindings.warn_if(
                    True,
                    f"unused {entity.kind_name}",
                    location=entity.diagnostic_node,
                )

    def dump(self) -> None:
        """
        Debug helper: dump this scope and its parent on the standard output.
        """
        s: Scope | None = self
        while s is not None:
            print(f"{s.label}:")
            for k, v in sorted(s.mapping.items()):
                print(f"  {k}: {v}")
            s = s.parent
