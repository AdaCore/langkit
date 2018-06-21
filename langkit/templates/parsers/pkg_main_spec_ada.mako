## vim: filetype=makoada

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with ${ada_lib_name}.Common;    use ${ada_lib_name}.Common;
limited with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Lexer;    use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

--  This package provides types and primitives to parse buffers and files and
--  get AST out of them.

package ${ada_lib_name}.Parsers is

   type Cst_String is access constant String;

   Generic_Parsing_Error_Message : aliased constant String := "Syntax error";
   Generic_Parsing_Error_Message_Access : constant Cst_String :=
      Generic_Parsing_Error_Message'Access;

   type Fail_Info_Kind is (Token_Fail, Custom_Fail);

   type Fail_Info (Kind : Fail_Info_Kind := Token_Fail) is record
      Pos : Token_Index := No_Token_Index;
      --  Index for the first token on which parsing failed

      case Kind is
         when Token_Fail =>
            Expected_Token_Id : Token_Kind;
            Found_Token_Id    : Token_Kind;
            --  In case of token mismatch, indicate what was expected and what
            --  we found instead.

         when Custom_Fail =>
            Custom_Message : Cst_String;
            --  Custom error message for parsing failure
      end case;
   end record;

   type Parsed_Node is access all Implementation.${root_node_value_type}'Class;
   type Symbol_Literal_Array_Access is
      access all Implementation.Symbol_Literal_Array;

   type Parser_Private_Part is private;

   type Parser_Type is record
      Current_Pos     : Token_Index := First_Token_Index;
      Last_Fail       : Fail_Info;
      Diagnostics     : Diagnostics_Vectors.Vector;
      Unit            : access Implementation.Analysis_Unit_Type;
      TDH             : Token_Data_Handler_Access;
      Mem_Pool        : Bump_Ptr_Pool;
      Symbol_Literals : Symbol_Literal_Array_Access;
      Private_Part    : Parser_Private_Part;
   end record;

   procedure Init_Parser
     (Input           : Lexer_Input;
      With_Trivia     : Boolean;
      Unit            : access Implementation.Analysis_Unit_Type;
      TDH             : Token_Data_Handler_Access;
      Symbol_Literals : Symbol_Literal_Array_Access;
      Parser          : in out Parser_Type);
   --  Init a parser to parse the given source. The resulting tokens (and
   --  trivia if With_Trivia) are stored into TDH.
   --
   --  This can raise:
   --
   --    * Lexer.Unknown_Charset or Lexer.Invalid_Input exceptions if the lexer
   --      has trouble decoding the input.
   --
   --    * Name_Error exceptions if this involves reading a file that we cannot
   --      open.

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule)
      return Parsed_Node;
   --  Do the actual parsing using the Rule parsing rule.  If Check_Complete,
   --  consider the case when the parser could not consume all the input tokens
   --  as an error.

   procedure Reset (Parser : in out Parser_Type);
   --  Reset the parser so that it is ready to parse again

   procedure Initialize (Parser : in out Parser_Type);
   --  Destroy resources associated with the parser

   procedure Destroy (Parser : in out Parser_Type);
   --  Destroy resources associated with the parser

private

   type Parser_Private_Part_Type;
   type Parser_Private_Part is access all Parser_Private_Part_Type;

end ${ada_lib_name}.Parsers;
