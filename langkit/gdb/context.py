from __future__ import absolute_import, division, print_function

from langkit.gdb.debug_info import DebugInfo
from langkit.names import Name


class Context(object):
    """
    Holder for generated library-specific information.
    """

    def __init__(self, lib_name, astnode_names, prefix):
        """
        :param str lib_name: Lower-case name for the generated library.

        :param astnode_names: Set of lower-case names for all AST node types.
        :type ast_node_names: set[str]

        :param str prefix: Prefix to use for command names.
        """
        self.lib_name = lib_name
        self.astnode_names = astnode_names
        self.prefix = prefix

        self.astnode_struct_names = self._astnode_struct_names()

        self.debug_info = DebugInfo.parse(self)

    def _astnode_struct_names(self):
        """
        Turn the set of ASTNode subclass names into a mapping from ASTNode
        record names, as GDB will see them, to user-friendly ASTNode names.
        """
        return {
            '{}__analysis__{}_type'.format(self.lib_name, name.lower()):
                Name.from_camel_with_underscores(name)
            for name in self.astnode_names
        }
