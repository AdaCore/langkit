
with Liblktlang_Support.Diagnostics; use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Symbols;     use Liblktlang_Support.Symbols;

with Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

limited with Liblktlang.Implementation;

private package Liblktlang.Lexer_Implementation is

   procedure Extract_Tokens
     (Input         : Liblktlang_Support.Internal.Analysis.Lexer_Input;
      With_Trivia   : Boolean;
      File_Reader   : access Implementation.Internal_File_Reader'Class;
      TDH           : in out Token_Data_Handler;
      Diagnostics   : in out Diagnostics_Vectors.Vector;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean);
   --  Implementation for Liblktlang.Lexer.Extract_Tokens.
   --
   --  In addition, set ``Same_Contents`` to whether if ``Old_TDH`` is not null
   --  and its contents is identical to the content of ``Input``.

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

end Liblktlang.Lexer_Implementation;
