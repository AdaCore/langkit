## vim: filetype=makoada

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;
with ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer.Token_Data_Handlers;
with ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
use ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;

--  This package provides types and primitives to parse buffers and files and
--  get AST out of them.
--
--  TODO??? For now, consider that this package is not part of the public API.
--  Please use the Analysis package to parse source files.

package ${_self.ada_api_settings.lib_name}.AST.Types.Parsers is

   type Fail_Info is record
      Pos               : Token_Index := No_Token_Index;
      Expected_Token_Id : Token_Kind;
      Found_Token_Id    : Token_Kind;
   end record;

   type Parser_Type is record
      Current_Pos : Token_Index := First_Token_Index;
      Last_Fail   : Fail_Info;
      Diagnostics : Diagnostics_Vectors.Vector;
      Unit        : Analysis_Unit_Interface;
      TDH         : Token_Data_Handler_Access;
      Mem_Pool    : Bump_Ptr_Pool;
   end record;

   function Create_From_File
     (Filename, Charset : String;
      Unit              : Analysis_Unit_Interface;
      With_Trivia       : Boolean := False) return Parser_type;
   --  Create a parser to parse the source in Filename, decoding it using
   --  Charset. The resulting tokens (and trivia if With_Trivia) are stored
   --  into TDH.
   --
   --  This can raise Lexer.Unknown_Charset or Lexer.Invalid_Input exceptions
   --  if the lexer has trouble decoding the input.

   function Create_From_Buffer
     (Buffer, Charset : String;
      Unit            : Analysis_Unit_Interface;
      With_Trivia     : Boolean := False) return Parser_type;
   --  Create a parser to parse the source in Buffer, decoding it using
   --  Charset. The resulting tokens (and trivia if With_Trivia) are stored
   --  into TDH.
   --
   --  This can raise Lexer.Unknown_Charset or Lexer.Invalid_Input exceptions
   --  if the lexer has trouble decoding the input.

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule)
      return ${root_node_type_name};
   --  Do the actual parsing using the Rule parsing rule.  If Check_Complete,
   --  consider the case when the parser could not consume all the input tokens
   --  as an error.

   procedure Clean_All_Memos;
   --  TODO??? We want to allow multiple parsers to run at the same time so
   --  memos should be stored in Parser_Type. In the end, this should be turned
   --  into a Parser_Type finalizer.

end ${_self.ada_api_settings.lib_name}.AST.Types.Parsers;
