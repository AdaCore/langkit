
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

with Liblktlang.Common; use Liblktlang.Common;
limited with Liblktlang.Implementation;
with Liblktlang.Lexer_Implementation;
use Liblktlang.Lexer_Implementation;
with Liblktlang.Parsers; use Liblktlang.Parsers;

--  The rationale for this unit is explained in the $.Parsers package body

private package Liblktlang.Parsers_Impl is

   procedure Init_Parser
     (Input         : Internal_Lexer_Input;
      With_Trivia   : Boolean;
      Unit          : access Implementation.Analysis_Unit_Type;
      TDH           : Token_Data_Handler_Access;
      Parser        : in out Parser_Type;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean)
   with Export, External_Name => "Liblktlang__init_parser";

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule)
      return Parsed_Node
   with Export, External_Name => "Liblktlang__parse";

   procedure Reset (Parser : in out Parser_Type)
   with Export, External_Name => "Liblktlang__reset_parser";

   procedure Initialize (Parser : in out Parser_Type)
   with Export, External_Name => "Liblktlang__initialize_parser";

   procedure Destroy (Parser : in out Parser_Type)
   with Export, External_Name => "Liblktlang__destroy_parser";

end Liblktlang.Parsers_Impl;
