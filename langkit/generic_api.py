from __future__ import annotations

from typing import List, Optional

from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, ArrayType, CompiledType, EnumType
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
    def enum_types(self) -> List[EnumType]:
        """
        Return the list of enum types for this context.
        """
        return self.context.enum_types

    def type_name(self, t: CompiledType) -> str:
        """
        Return a short type name for ``t``, to be used in the generation of
        constants.
        """
        if isinstance(t, ASTNodeType):
            t = t.entity

        return (
            f"{self.type_name(t.element_type)}_Array"
            if isinstance(t, ArrayType)
            else names.Name.from_camel(t.type_repo_name).camel_with_underscores
        )

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
        # to add the type name only for structs.
        return (
            m.name
            if m.struct.is_ast_node
            else f"{self.type_name(m.struct)}_{m.name}"
        )

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
