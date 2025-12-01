## vim: filetype=makoada

with System;

with Langkit_Support.Bump_Ptr;          use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics;       use Langkit_Support.Diagnostics;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Packrat;           use Langkit_Support.Packrat;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;
limited with ${ada_lib_name}.Implementation;

--  Internal package to provide types and primitives to parse buffers and files
--  and get AST out of them.

private package ${ada_lib_name}.Parsers is

   --  In principle, ``Fail_Info`` itself should be the discriminated record,
   --  but having a separate ``Fail_Info_Record`` type is useful so that
   --  ``Fail_Info`` has a compile-time known size, and thus copying it is
   --  faster (enough to have noticeable impact on parser performance).

   type Fail_Info_Record (Kind : Fail_Info_Kind := Token_Fail) is record
      case Kind is
         when Token_Fail =>
            Expected_Token_Id : Token_Kind;
            Found_Token_Id    : Token_Kind;
            --  In case of token mismatch, indicate what was expected and what
            --  we found instead.

         when Predicate_Fail =>
            null;
      end case;
   end record;

   type Fail_Info is record
      Pos : Token_Index := No_Token_Index;
      --  Index for the first token on which parsing failed

      Data : Fail_Info_Record;
   end record;

   type Parsed_Node is
      access all Implementation.${T.root_node.value_type_name};

   type Parser_Type is record
      Current_Pos  : Token_Index := First_Token_Index;
      Last_Fail    : Fail_Info;
      Pool         : Diagnostic_Pool;
      Last_Diag    : Diagnostic_Mark;
      Diagnostics  : Diagnostics_Vectors.Vector;
      Unit         : access Implementation.Analysis_Unit_Type;
      TDH          : Token_Data_Handler_Access;
      Mem_Pool     : Bump_Ptr_Pool;
      Private_Part : System.Address;
   end record;

   procedure Init_Parser
     (Input         : Lexer_Input;
      With_Trivia   : Boolean;
      Unit          : access Implementation.Analysis_Unit_Type;
      TDH           : Token_Data_Handler_Access;
      Parser        : in out Parser_Type;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean);
   --  Initialise parsing for the given source. The resulting tokens (and
   --  trivia if With_Trivia) are stored into TDH.
   --
   --  Set ``Same_Contents`` to whether if ``Old_TDH`` is not null and its
   --  contents is identical to the content of ``Input``.
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

end ${ada_lib_name}.Parsers;
