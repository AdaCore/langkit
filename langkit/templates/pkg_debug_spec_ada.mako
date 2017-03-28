## vim: filetype=makoada

--  This package defines subprograms whose only purpose it to be used from a
--  debugger. This is supposed to make developpers' life easier.

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Lexer;    use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

package ${ada_lib_name}.Debug is

   procedure PN (Node : ${root_node_type_name});
   --  "Print Node".  Shortcut for Put_Line (Node.Short_Image). This is useful
   --  because Short_Image takes an implicit accessibility level parameter,
   --  which is not convenient in GDB.

   procedure PT (Node : ${root_node_type_name});
   --  "Print Tree". Shortcut for Node.Print. This is useful because Print is a
   --  dispatching primitive whereas these are difficult to call from GDB.
   --  Besides, it removes the Level parameter.

   procedure PTok (TDH : Token_Data_Handler_Access; T : Token_Index);
   --  "Print Token". Print the data associated to the T token in the given
   --  token data handler.

   procedure PEnv (Env : Lexical_Env);
   --  "Print lexical Environment". Print the content of Env and all its parent
   --  chain.

end ${ada_lib_name}.Debug;
