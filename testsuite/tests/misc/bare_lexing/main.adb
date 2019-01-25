with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

with Libfoolang.Common; use Libfoolang.Common;
use Libfoolang.Common.Token_Data_Handlers;
with Libfoolang.Lexer;  use Libfoolang.Lexer;

procedure Main is

   Buffer : constant String :=
     ("# comment" & ASCII.LF
      & "example" & ASCII.LF);

   TDH         : Token_Data_Handler;
   Diagnostics : Diagnostics_Vectors.Vector;
   Tok         : Token_Or_Trivia_Index;

begin
   Extract_Tokens
     (Input => (Kind        => Bytes_Buffer,
                Charset     => To_Unbounded_String ("ascii"),
                Read_BOM    => True,
                Bytes       => To_Unbounded_String (Buffer)),
      Tab_Stop    => 8,
      With_Trivia => True,
      TDH         => TDH,
      Diagnostics => Diagnostics);

   Tok := First_Token_Or_Trivia (TDH);
   while Tok /= No_Token_Or_Trivia_Index loop
      declare
         Token_Data : constant Stored_Token_Data := Data (Tok, TDH);
      begin
         Put_Line (Image (Token_Data.Sloc_Range)
                   & " " & Token_Kind'Image (To_Token_Kind (Token_Data.Kind))
                   & ": " & Image (TDH, Token_Data));
      end;
      Tok := Next (Tok, TDH);
   end loop;

   Free (TDH);
   Put_Line ("main.adb: Done.");
end Main;
