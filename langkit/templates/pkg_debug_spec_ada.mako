## vim: filetype=makoada

--  This package defines subprograms whose only purpose it to be used from a
--  debugger. This is supposed to make developpers' life easier.

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;
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

   procedure PEnv (Env : AST_Envs.Lexical_Env);
   --  "Print lexical Environment". Print the content of Env and all its parent
   --  chain.

   function Sym_Matches (S : Symbol_Type; Text : String) return Boolean;
   --  Return whether the text associated to S matches Text. There is a bug in
   --  GDB that makes comparison with "=" always return false.

   procedure PRel (Rel : Relation; Context_Node : ${root_node_type_name});
   --  "Print Relation". Print Rel as a tree of logic relations

end ${ada_lib_name}.Debug;
