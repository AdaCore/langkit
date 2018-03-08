with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

with Libfoolang.Lexer; use Libfoolang.Lexer;
use Libfoolang.Lexer.Token_Data_Handlers;

procedure Main is

   Buffer : constant String :=
     ("# comment" & ASCII.LF
      & "example" & ASCII.LF);

   TDH         : Token_Data_Handler;
   Diagnostics : Diagnostics_Vectors.Vector;
   Tok         : Token_Or_Trivia_Index;

begin
   Lex_From_Buffer
     (Buffer      => Buffer,
      Charset     => "ascii",
      Read_BOM    => True,
      TDH         => TDH,
      Diagnostics => Diagnostics,
      With_Trivia => True);

   Tok := First_Token_Or_Trivia (TDH);
   while Tok /= No_Token_Or_Trivia_Index loop
      declare
         Token_Data : constant Token_Data_Type := Data (Tok, TDH);
      begin
         Put_Line (Image (Token_Data.Sloc_Range)
                   & " " & Token_Kind'Image (Token_Data.Kind)
                   & ": " & Image (TDH, Token_Data));
      end;
      Tok := Next (Tok, TDH);
   end loop;

   Free (TDH);
   Put_Line ("main.adb: Done.");
end Main;
