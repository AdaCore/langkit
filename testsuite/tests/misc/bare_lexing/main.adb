with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

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
   Extract_Tokens
     (Input => (Kind => Bytes_Buffer,
                Charset => To_Unbounded_String ("ascii"),
                Read_BOM => True,
                Bytes => Buffer'Unrestricted_Access),
      With_Trivia => True,
      TDH         => TDH,
      Diagnostics => Diagnostics);

   Tok := First_Token_Or_Trivia (TDH);
   while Tok /= No_Token_Or_Trivia_Index loop
      declare
         Token_Data : constant Stored_Token_Data := Data (Tok, TDH);
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
