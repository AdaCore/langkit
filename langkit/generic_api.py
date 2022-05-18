from __future__ import annotations

from typing import List, Optional, Union

from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, ArrayType, CompiledType, EntityType,
    EnumType, IteratorType, StructType, T
)
import langkit.names as names


class GenericAPI:
    """
    Common code to generate code for the Ada generic API.

    This class provides helper methods to be used in the code generation of the
    ``$.Generic_API`` and ``$.Generic_Introspection`` units.
    """

    def __init__(self, context: CompileCtx):
        self.context = context

    @property
    def all_types(self) -> List[CompiledType]:
        """
        Return the list of all public types for this context.
        """
        all_but_builtin_types: List[CompiledType] = []
        all_but_builtin_types += self.enum_types
        all_but_builtin_types += self.array_types
        all_but_builtin_types += self.iterator_types
        all_but_builtin_types += self.struct_types
        all_but_builtin_types += self.entity_types

        # Builtin types are all the types that are public (exposed) but not in
        # all_but_builtin_types.
        other_types = [
            t for t in T.all_types
            if (
                t.exposed
                and not t.is_ast_node
                and t not in all_but_builtin_types
            )
        ]

        return (
            sorted(other_types, key=lambda t: t.api_name)
            + all_but_builtin_types
        )

    @property
    def all_members(self) -> List[AbstractNodeData]:
        """
        Return the list of all base struct members: struct fields, node syntax
        fields and properties.
        """
        sf = self.context.sorted_struct_fields
        pf = self.context.sorted_parse_fields
        p = self.context.sorted_properties

        assert sf is not None
        assert pf is not None
        assert p is not None

        result: List[AbstractNodeData] = []
        result += sf
        result += pf
        result += p
        return result

    @property
    def enum_types(self) -> List[EnumType]:
        """
        Return the list of enum types for this context.
        """
        return self.context.enum_types

    @property
    def array_types(self) -> List[ArrayType]:
        """
        Return the list of public array types for this context.
        """
        return [t for t in self.context.array_types if t.exposed]

    @property
    def iterator_types(self) -> List[IteratorType]:
        """
        Return the list of public iterator types for this context.
        """
        return [t for t in self.context.iterator_types if t.exposed]

    @property
    def struct_types(self) -> List[StructType]:
        """
        Return the list of public struct types for this context.

        Note that this omit entity types, as they get a very different handling
        in code generation.
        """
        return [t for t in self.context.struct_types
                if t.exposed and not t.is_entity_type]

    @property
    def entity_types(self) -> List[EntityType]:
        """
        Return the list of entity types for this context.
        """
        return [t.entity for t in self.context.astnode_types]

    def type_name(self, t: CompiledType) -> str:
        """
        Return a short type name for ``t``, to be used in the generation of
        constants.
        """
        if isinstance(t, ASTNodeType):
            t = t.entity

        if isinstance(t, ArrayType):
            return f"{self.type_name(t.element_type)}_Array"
        elif isinstance(t, IteratorType):
            return f"{self.type_name(t.element_type)}_Iterator"
        elif isinstance(t, EntityType):
            return (
                names.Name.from_camel(t.element_type.type_repo_name)
                .camel_with_underscores
            )
        else:
            return (names.Name.from_camel(t.type_repo_name)
                    .camel_with_underscores)

    def type_index(self, t: Optional[CompiledType]) -> str:
        """
        Return the name of the constant for ``t``'s type index, or
        ``No_Type_Index`` if ``t`` is None.

        For convenience, also automatically handle bare nodes as entities (bare
        nodes are not exposed).
        """
        return (
            "No_Type_Index"
            if t is None
            else f"Type_Index_For_{self.type_name(t)}"
        )

    def root_member(self, m: AbstractNodeData) -> AbstractNodeData:
        """
        If ``m`` is an overriding node member, return the root member it
        overrides.

        In the case of overriding node members (abstract syntax fields,
        overriding properties), we emit a single member descriptor for the
        whole derivation tree, so always refer to the root member.
        """
        assert m.struct
        if m.struct.is_ast_node:
            while True:
                base = m.base
                if base is None:
                    break
                m = base
        return m

    def member_name(self, m: AbstractNodeData) -> str:
        """
        Return a unique name for the ``m`` struct member.
        """
        assert m.struct
        m = self.root_member(m)

        # Node members are already qualified by the node type name, so we need
        # to add the type name only for structs. Also, make sure we get the
        # original name for properties that have been turned into dispatchers.
        if m.is_property and m.is_dispatcher:
            return m.name_before_dispatcher
        elif m.struct.is_ast_node:
            return m.name
        else:
            return f"{self.type_name(m.struct)}_{m.name}"

    def member_index(self, m: AbstractNodeData) -> str:
        """
        Return the name of the constant for ``m``'s struct member index.
        """
        return f"Member_Index_For_{self.member_name(m)}"

    def internal_value_typename(self, t: CompiledType) -> str:
        """
        Return the name to use for internal value types for ``t``.
        """
        return (
            "Node"
            if t.is_ast_node or t.is_entity_type
            else self.type_name(t)
        )

    def internal_value_type(self, t: CompiledType) -> str:
        """
        Return the name of the
        ``Langkit_Support.Internal.Introspection.Internal_Value`` derivation
        used to represent ``t``.
        """
        return f"Internal_Rec_{self.internal_value_typename(t)}"

    def internal_value_access(self, t: CompiledType) -> str:
        """
        Like ``internal_value_type``, but return the access type instead.
        """
        return f"Internal_Acc_{self.internal_value_typename(t)}"

    def array_access_type(self, t: ArrayType) -> str:
        """
        Return the name of the access type used to store ``t`` values.
        """
        return f"Internal_Stored_{self.internal_value_typename(t)}"

    def to_specific_node(self,
                         lk_node_expr: str,
                         node: Union[EntityType, ASTNodeType]) -> str:
        """
        Return an expression that converts ``lk_node_expr`` (an expression that
        computes a generic ``Lk_Node`` value) into the given language-specific
        public ``node`` type.
        """
        if isinstance(node, EntityType):
            node = node.element_type
            assert isinstance(node, ASTNodeType)

        result = f"Get_Node ({lk_node_expr})"
        if not node.is_root_node:
            result += f".As_{node.entity.api_name}"
        return result
