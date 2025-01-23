
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Text; use Liblktlang_Support.Text;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

with Liblktlang.Common;         use Liblktlang.Common;
with Liblktlang.Implementation; use Liblktlang.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package Liblktlang.Private_Converters is

   function From_Generic (Token : Lk_Token) return Common.Token_Reference
     with Import, External_Name => "Liblktlang__from_generic_token";
   function To_Generic (Token : Common.Token_Reference) return Lk_Token
     with Import, External_Name => "Liblktlang__to_generic_token";
   --  See the corresponding exports in $.Common's body

   function From_Generic_Node (Node : Lk_Node) return Internal_Entity;
   function To_Generic_Node (Entity : Internal_Entity) return Lk_Node;

   type Token_Reference_Wrapper is access function
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   Wrap_Token_Reference : Token_Reference_Wrapper;

   type Token_Context_Getter is access function
     (Token : Token_Reference) return Internal_Context;
   Get_Token_Context : Token_Context_Getter;

   type Token_Unit_Getter is access function
     (Token : Token_Reference) return Internal_Unit;
   Get_Token_Unit : Token_Unit_Getter;

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

end Liblktlang.Private_Converters;
