## vim: filetype=makoada

with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;
limited with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Parsers; use ${ada_lib_name}.Parsers;

--  The rationale for this unit is explained in the $.Parsers package body

private package ${ada_lib_name}.Parsers_Impl is

   procedure Init_Parser
     (Input         : Lexer_Input;
      With_Trivia   : Boolean;
      Unit          : access Implementation.Analysis_Unit_Type;
      TDH           : Token_Data_Handler_Access;
      Parser        : in out Parser_Type;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean)
   with Export, External_Name => "${ada_lib_name}__init_parser";

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule)
      return Parsed_Node
   with Export, External_Name => "${ada_lib_name}__parse";

   procedure Reset (Parser : in out Parser_Type)
   with Export, External_Name => "${ada_lib_name}__reset_parser";

   procedure Initialize (Parser : in out Parser_Type)
   with Export, External_Name => "${ada_lib_name}__initialize_parser";

   procedure Destroy (Parser : in out Parser_Type)
   with Export, External_Name => "${ada_lib_name}__destroy_parser";

end ${ada_lib_name}.Parsers_Impl;
