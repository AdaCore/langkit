
with Liblktlang.Parsers_Impl;

--  TODO??? (eng/libadalang/libadalang#1265) The implementation has been moved
--  to a separate unit (Parsers_Impl) to reduce the number of exported symbols
--  in shared libraries.

package body Liblktlang.Parsers is

   -----------------
   -- Init_Parser --
   -----------------

   procedure Init_Parser
     (Input         : Lexer_Input;
      With_Trivia   : Boolean;
      Unit          : access Implementation.Analysis_Unit_Type;
      TDH           : Token_Data_Handler_Access;
      Parser        : in out Parser_Type;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean) is
   begin
      Parsers_Impl.Init_Parser
        (Input, With_Trivia, Unit, TDH, Parser, Old_TDH, Same_Contents);
   end Init_Parser;

   -----------
   -- Parse --
   -----------

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule)
      return Parsed_Node is
   begin
      return Parsers_Impl.Parse (Parser, Check_Complete, Rule);
   end Parse;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Parser_Type) is
   begin
      Parsers_Impl.Reset (Parser);
   end Reset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parser : in out Parser_Type) is
   begin
      Parsers_Impl.Initialize (Parser);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Parser : in out Parser_Type) is
   begin
      Parsers_Impl.Destroy (Parser);
   end Destroy;

end Liblktlang.Parsers;
