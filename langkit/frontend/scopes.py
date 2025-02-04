from __future__ import annotations

import abc
import dataclasses
from typing import Callable, ClassVar

from langkit.compile_context import CompileCtx
from langkit.compiled_types import CompiledType
from langkit.diagnostics import Location, error
from langkit.envs import RefKind
import langkit.expressions as E
from langkit.frontend.utils import lkt_context
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

        value: E.AbstractExpression
        """
        Value to use for this during expression lowering.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin value {self.value}"

    @dataclasses.dataclass
    class BuiltinDynVar(BuiltinEntity):
        """
        Dynamic variable created automatically by Lkt.
        """

        variable: E.DynamicVariable

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin dynamic variable {self.variable.dsl_name}"

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

        constructor: Callable[[CompiledType, str], E.BaseRaiseException]

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

        variable: E.AbstractVariable
        """
        Value to use for this during expression lowering.
        """

    class LocalVariable(UserValue):
        """
        Local variable declaration.
        """

        kind_name = "local variable"

    @dataclasses.dataclass
    class BoundDynVar(LocalVariable):
        """
        Dynamic variable that has been bound.

        ``BoudDynVar`` instances are put in a scope as soon as a dynamic
        variable is bound in that scope: it allows keep track of the fact that
        it is bound (to be used as regular variables, hence the
        ``LocalVariable`` derivation).
        """

        kind_name = "bound dynamic variable"

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

    def add(self, entity: Scope.UserEntity) -> None:
        """
        Add a declaration to the current scope.

        Stop with a user-level error if there is already a declaration with the
        same name in this scope.
        """
        # Pseudo "_" entities are used to mean "do not bind this to an
        # identifier": just skip them.
        if entity.name == "_":
            return

        other_entity = self.mapping.get(entity.name)
        if other_entity is None:
            self.mapping[entity.name] = entity
        else:
            with lkt_context(entity.diagnostic_node):
                other_label = (
                    other_entity.diagnostic_name
                    if isinstance(other_entity, Scope.UserEntity) else
                    "a builtin"
                )
                error(f"this declaration conflicts with {other_label}")

    def lookup(self, name: str) -> Scope.Entity:
        """
        Look for the declaration for a given name in this scope or one of its
        parents. Raise a ``KeyError`` exception if there is no such
        declaration.
        """
        scope: Scope | None = self
        while scope is not None:
            try:
                return scope.mapping[name]
            except KeyError:
                scope = scope.parent

        raise KeyError(f"no entity called '{name}' in {self.label}")

    def resolve(self, name: L.Expr) -> Scope.Entity:
        """
        Resolve the entity designated by ``name`` in this scope.

        Unlike ``lookup``, this create a diagnostic if the entity is not found.
        """
        if isinstance(name, L.RefId):
            try:
                return self.lookup(name.text)
            except KeyError as exc:
                with lkt_context(name):
                    error(exc.args[0])
        else:
            with lkt_context(name):
                error("invalid entity reference")

    def create_child(self, label: str) -> Scope:
        """
        Return a new scope whose ``self`` is the parent.
        """
        return Scope(label, self.context, self)

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


def create_root_scope(ctx: CompileCtx) -> Scope:
    """
    Create and return a root scope.

    TODO (eng/libadalang/langkit#704): once the DSL is no more, use the same
    root scope in:

    * the lexer lowering pass,
    * the grammar lowering pass,
    * the types lowering pass.

    See callers for this helper function.
    """
    return Scope("the root scope", ctx)
