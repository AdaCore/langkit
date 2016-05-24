## vim: filetype=makoada

--  This package defines subprograms whose only purpose it to be used from a
--  debugger. This is supposed to make developpers' life easier.

with ${_self.ada_api_settings.lib_name}.AST;
use ${_self.ada_api_settings.lib_name}.AST;

package ${_self.ada_api_settings.lib_name}.Debug is

   procedure PN (Node : ${root_node_type_name});
   --  "Print Node".  Shortcut for Put_Line (Node.Short_Image). This is useful
   --  because Short_Image takes an implicit accessibility level parameter,
   --  which is not convenient in GDB.

   procedure PT (Node : ${root_node_type_name});
   --  "Print Tree". Shortcut for Node.Print. This is useful because Print is a
   --  dispatching primitive whereas these are difficult to call from GDB.
   --  Besides, it removes the Level parameter.

end ${_self.ada_api_settings.lib_name}.Debug;
