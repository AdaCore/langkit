## vim: filetype=makoada

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

--  This package provides types and primitives to parse buffers and files and
--  get AST out of them.

private package ${ada_lib_name}.Analysis.Parsers is

   type Cst_String is access constant String;

   type Fail_Info_Kind is (Token_Fail, Custom_Fail);

   type Fail_Info (Kind : Fail_Info_Kind := Token_Fail) is record
      Pos               : Token_Index := No_Token_Index;
      case Kind is
      when Token_Fail =>
         Expected_Token_Id : Token_Kind;
         Found_Token_Id    : Token_Kind;
      when Custom_Fail =>
         Custom_Message    : Cst_String;
      end case;
   end record;

   type Parser_Private_Part is private;

   type Parser_Type is record
      Current_Pos     : Token_Index := First_Token_Index;
      Last_Fail       : Fail_Info;
      Diagnostics     : Diagnostics_Vectors.Vector;
      Unit            : Analysis_Unit;
      TDH             : Token_Data_Handler_Access;
      Mem_Pool        : Bump_Ptr_Pool;
      % if ctx.symbol_literals:
      Symbol_Literals : Symbol_Literal_Array_Access;
      % endif

      Private_Part    : Parser_Private_Part;
   end record;

   function Create_From_File
     (Filename, Charset : String;
      Read_BOM          : Boolean;
      Unit              : Analysis_Unit;
      With_Trivia       : Boolean := False) return Parser_type;
   --  Create a parser to parse the source in Filename, decoding it using
   --  Charset. The resulting tokens (and trivia if With_Trivia) are stored
   --  into TDH.
   --
   --  This can raise Lexer.Unknown_Charset or Lexer.Invalid_Input exceptions
   --  if the lexer has trouble decoding the input.

   function Create_From_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Unit            : Analysis_Unit;
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

private

   type Parser_Private_Part_Type;
   type Parser_Private_Part is access all Parser_Private_Part_Type;

end ${ada_lib_name}.Analysis.Parsers;
