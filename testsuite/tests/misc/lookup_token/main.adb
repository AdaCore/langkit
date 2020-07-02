with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;

   procedure Test (Filename : String);

   procedure Test (Filename : String) is
      U            : constant Analysis_Unit := Get_From_File (Ctx, Filename);
      Last_Tok     : constant Token_Data_Type := Data (Last_Token (U));
      Previous_Tok : Token_Reference := No_Token;
   begin
      Put_Line ("= " & Filename & " =");
      for Line in 1 .. Sloc_Range (Last_Tok).End_Line loop
         declare
            Sloc  : constant Source_Location := (Line, 1);
            Token : constant Token_Reference := Lookup_Token (U, Sloc);
         begin
            Put_Line ("  " & Image (Sloc)
                      & " -> [" & Image (Sloc_Range (Data (Token))) & "] "
                      & Image (Text (Token)));
            if Line > 1 then
               if Previous_Tok = Token then
                  Put_Line ("    Same as previous token");
               else
                  if Next (Previous_Tok) /= Token then
                     Put_Line ("    Next (Previous_Tok) does not match");
                  end if;
                  if Previous (Token) /= Previous_Tok then
                     Put_Line ("    Previous (Token) does not match");
                  end if;
               end if;
            end if;
            Previous_Tok := Token;
         end;
      end loop;
      New_Line;
   end Test;
begin
   Test ("leading_trivia.txt");
   Test ("inside_trivia.txt");
   Test ("trailing_trivia.txt");
   Test ("no_trivia.txt");
   Test ("no_token.txt");
   Test ("r403_028.txt");

   Put_Line ("main.adb: Done.");
end Main;
