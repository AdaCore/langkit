## vim: filetype=makoada

--  This package defines subprograms whose only purpose it to be used from a
--  debugger. This is supposed to make developpers' life easier.

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;
with ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer.Token_Data_Handlers;

package ${_self.ada_api_settings.lib_name}.Debug is

   procedure PN (Node : ${root_node_type_name});
   --  "Print Node".  Shortcut for Put_Line (Node.Short_Image). This is useful
   --  because Short_Image takes an implicit accessibility level parameter,
   --  which is not convenient in GDB.

   procedure PT (Node : ${root_node_type_name});
   --  "Print Tree". Shortcut for Node.Print. This is useful because Print is a
   --  dispatching primitive whereas these are difficult to call from GDB.
   --  Besides, it removes the Level parameter.

   procedure PTok (Node : ${root_node_type_name}; T : Token_Index);
   --  "Print Token". Print the data associated to the T token. Node must be in
   --  the same analysis unit as the token that T represents.

end ${_self.ada_api_settings.lib_name}.Debug;
