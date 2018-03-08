with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create (With_Trivia => True);

   procedure Test (Filename : String);

   procedure Test (Filename : String) is
      U        : constant Analysis_Unit := Get_From_File (Ctx, Filename);
      Last_Tok : constant Token_Data_Type := Data (Last_Token (U));
   begin
      Put_Line ("= " & Filename & " =");
      for Line in 1 .. Sloc_Range (Last_Tok).End_Line loop
         declare
            Sloc  : constant Source_Location := (Line, 1);
            Token : constant Token_Type := Lookup_Token (U, Sloc);
         begin
            Put_Line ("  " & Image (Sloc)
                      & " -> [" & Image (Sloc_Range (Data (Token))) & "] "
                      & Text (Token));
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

   Put_Line ("main.adb: Done.");
end Main;
