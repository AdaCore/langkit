from typing import Dict, List, Optional, Set

import gdb

from langkit.debug_info import DebugInfo
from langkit.gdb.state import State
from langkit.names import Name


class Context:
    """
    Holder for generated library-specific information.
    """

    def __init__(self,
                 lib_name: str,
                 astnode_names: List[str],
                 astnode_kinds: Dict[int, str],
                 prefix: str):
        """
        :param str lib_name: Lower-case name for the generated library.

        :param list[str] astnode_names: List of camel-with-mixed-case names for
            all node types.

        :param dict[int, str] astnode_kinds: Mapping of kinds ('Enum_Rep) to
            camel-with-mixed-case node names.

        :param str prefix: Prefix to use for command names.
        """
        self.lib_name = lib_name
        self.astnode_names = [Name(name) for name in astnode_names]
        self.astnode_kinds = {kind: Name(name)
                              for kind, name in astnode_kinds.items()}
        self.prefix = prefix

        self.node_record = ('{}__implementation__root_node_record'
                            .format(self.lib_name))
        """
        Name of the record type used to represent node data.
        """

        self.entity_struct_names = self._entity_struct_names()

        self.reparse_debug_info()

    def _entity_struct_names(self) -> Set[str]:
        """
        Turn the set of AST node names into a set of encoded type names for the
        corresponding entity records.
        """
        return {
            '{}__implementation__internal_entity_{}'
            .format(self.lib_name, name.lower)
            for name in self.astnode_names
        } | {
            '{}__implementation__internal_entity'.format(self.lib_name),
            '{}__implementation__ast_envs__entity'.format(self.lib_name),
        }

    def decode_state(self,
                     frame: Optional[gdb.Frame] = None) -> Optional[State]:
        """
        Shortcut for::

            State.decode(self, frame)

        If `frame` is None, use the selected frame.

        :rtype: State
        """
        if frame is None:
            frame = gdb.selected_frame()
        return State.decode(self, frame)

    @property
    def analysis_prefix(self) -> str:
        """
        Return the prefix for symbols defined in the $.Implementation unit. For
        instance: "libfoolang__implementation__".

        :rtype: str
        """
        return '{}__implementation__'.format(self.lib_name)

    def reparse_debug_info(self) -> None:
        """
        Reload debug information from the analysis source file.
        """
        self.debug_info = DebugInfo.parse_from_gdb(self)

    def implname(self, suffix: str) -> str:
        """
        Return the C-style symbol name to use for an Ada entity in the
        $.Implementation package.

        :param str suffix: Name suffix. For instance: "my_type_name".
        :rtype: str
        """
        return '{}__implementation__{}'.format(self.lib_name, suffix)

    def comname(self, suffix: str) -> str:
        """
        Return the C-style symbol name to use for an Ada entity in the
        $.Common package.

        :param str suffix: Name suffix. For instance: "my_type_name".
        :rtype: str
        """
        return '{}__common__{}'.format(self.lib_name, suffix)
