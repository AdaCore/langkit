from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb

from langkit.gdb.line_map import LineMap
from langkit.gdb.utils import ptr_to_int, system_address
from langkit.names import Name


class Context(object):
    """
    Holder for generated library-specific information.
    """

    def __init__(self, lib_name, astnode_names):
        """
        :param str lib_name: Lower-case name for the generated library.

        :param astnode_names: Set of lower-case names for all AST node types.
        :type ast_node_names: set[str]
        """
        self.lib_name = lib_name
        self.astnode_names = astnode_names

        self.astnode_struct_names = self._astnode_struct_names()
        self.tags_mapping = self._tags_mapping()

        self.line_map = LineMap.parse(self)

    def _astnode_struct_names(self):
        """
        Turn the set of ASTNode subclass names into a set of ASTNode record
        names, as GDB will see them.
        """
        return {
            '{}__analysis__{}_type'.format(self.lib_name, name)
            for name in self.astnode_names
        }

    def _tags_mapping(self):
        """
        Build a mapping: address (int) -> AST node pretty name.

        Each address is the address of the AST node type tag.
        """
        # The symbols we are looking up here do not correspond exactly to the
        # _tag field we see in tagged records: we need to add an offset to
        # them. This offset is 4 times the size of a pointer. See GNAT's
        # a-tags.ads file for more details.
        tag_offset = 4 * system_address.sizeof

        return {
            ptr_to_int(gdb.parse_and_eval(
                '<{}__analysis__{}_typeT>'.format(self.lib_name, name)
            ).address) + tag_offset: Name.from_lower(name).camel
            for name in self.astnode_names
        }
