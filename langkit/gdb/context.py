from __future__ import annotations

import gdb

from langkit.debug_info import DebugInfo
from langkit.gdb.state import State
from langkit.names import Name


class Context:
    """
    Holder for generated library-specific information.
    """

    def __init__(
        self,
        lib_name: str,
        astnode_names: list[str],
        astnode_kinds: dict[int, str],
        prefix: str,
        standalone: bool,
    ):
        """
        :param str lib_name: Lower-case name for the generated library.

        :param astnode_names: List of camel-with-mixed-case names for all node
            types.

        :param astnode_kinds: Mapping of kinds ('Enum_Rep) to
            camel-with-mixed-case node names.

        :param prefix: Prefix to use for command names.

        :param standalone: Whether this is a standalone Langkit-generated
            library.
        """
        self.lib_name = lib_name
        self.astnode_names = [Name(name) for name in astnode_names]
        self.astnode_kinds = {
            kind: Name(name) for kind, name in astnode_kinds.items()
        }
        self.prefix = prefix
        self.standalone = standalone

        self.node_record = "{}.implementation.root_node_record".format(
            self.lib_name
        )
        """
        Name of the record type used to represent node data.
        """

        self.entity_struct_names = self._entity_struct_names()

        self.support_pkg = (
            f"{lib_name}_support" if self.standalone else "langkit_support"
        )

        self.reparse_debug_info()

    def in_support_pkg(self, entity: str) -> str:
        """
        Return the fully qualified name of an entity under the support package.
        """
        return f"{self.support_pkg}.{entity}"

    def _entity_struct_names(self) -> set[str]:
        """
        Turn the set of AST node names into a set of encoded type names for the
        corresponding entity records.
        """
        return {
            "{}.implementation.internal_entity_{}".format(
                self.lib_name, name.lower
            )
            for name in self.astnode_names
        } | {
            "{}.implementation.internal_entity".format(self.lib_name),
            "{}.implementation.ast_envs.entity".format(self.lib_name),
        }

    def decode_state(self, frame: gdb.Frame | None = None) -> State | None:
        """
        Shortcut for::

            State.decode(self, frame)

        If `frame` is None, use the selected frame.
        """
        if frame is None:
            frame = gdb.selected_frame()
        return State.decode(self, frame)

    @property
    def analysis_prefix(self) -> str:
        """
        Return the prefix for symbols defined in the $.Implementation unit. For
        instance: "libfoolang.implementation.".
        """
        return "{}.implementation.".format(self.lib_name)

    def reparse_debug_info(self) -> None:
        """
        Reload debug information from the analysis source file.
        """
        self.debug_info = DebugInfo.parse_from_gdb(self)

    def implname(self, suffix: str) -> str:
        """
        Return the C-style symbol name to use for an Ada entity in the
        $.Implementation package.

        :param suffix: Name suffix. For instance: "my_type_name".
        """
        return "{}.implementation.{}".format(self.lib_name, suffix)

    def comname(self, suffix: str) -> str:
        """
        Return the C-style symbol name to use for an Ada entity in the
        $.Common package.

        :param suffix: Name suffix. For instance: "my_type_name".
        """
        return "{}.common.{}".format(self.lib_name, suffix)
