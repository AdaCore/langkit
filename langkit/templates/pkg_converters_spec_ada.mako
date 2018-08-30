## vim: filetype=makoada

with Langkit_Support.Text; use Langkit_Support.Text;

with ${ada_lib_name}.Analysis;       use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;         use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package ${ada_lib_name}.Converters is

   type Context_Wrapper is access function
     (Context : Internal_Context) return Analysis_Context;
   Wrap_Context : Context_Wrapper;

   type Context_Unwrapper is access function
     (Context : Analysis_Context'Class) return Internal_Context;
   Unwrap_Context : Context_Unwrapper;

   type Unit_Wrapper is access function
     (Unit : Internal_Unit) return Analysis_Unit;
   Wrap_Unit : Unit_Wrapper;

   type Unit_Unwrapper is access function
     (Unit : Analysis_Unit'Class) return Internal_Unit;
   Unwrap_Unit : Unit_Unwrapper;

   type Node_Wrapper is access function
     (Node : access ${root_node_value_type}'Class;
      Info : AST_Envs.Entity_Info := No_Entity_Info)
      return ${root_entity.api_name};
   Wrap_Node : Node_Wrapper;

   type Node_Unwrapper is access function
     (Node : ${root_entity.api_name}'Class) return ${root_node_type_name};
   Unwrap_Node : Node_Unwrapper;

   type Token_Reference_Wrapper is access function
     (TDH   : Token_Data_Handler_Access;
      Index : Token_Or_Trivia_Index) return Token_Reference;
   Wrap_Token_Reference : Token_Reference_Wrapper;

   type Token_TDH_Getter is access function
     (Token : Token_Reference) return Token_Data_Handler_Access;
   Get_Token_TDH : Token_TDH_Getter;

   type Token_Index_Getter is access function
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   Get_Token_Index : Token_Index_Getter;

   type Token_Text_Extractor is access procedure
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   Extract_Token_Text : Token_Text_Extractor;

end ${ada_lib_name}.Converters;
