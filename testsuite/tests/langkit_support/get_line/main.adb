with Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics.Output;
use Langkit_Support.Diagnostics.Output;
with Langkit_Support.Text; use Langkit_Support.Text;

procedure Main is
   procedure Check (Label : String; Buffer : Text_Type);

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; Buffer : Text_Type) is
      B           : Text_Access := new Text_Type'(Buffer);
      TB          : Text_Buffer := Create (Text_Cst_Access (B));
      Lines_Count : constant Natural :=
         Ada.Strings.Wide_Wide_Fixed.Count (Buffer, (1 => Chars.LF));
   begin
      Put_Line ("== " & Label & " ==");
      for I in 1 .. Lines_Count + 2 loop
         declare
            L : constant Text_Type := Get_Line (TB, I);
         begin
            Put_Line
              ("  Line" & I'Image & ": " & Image (L, With_Quotes => True));
         end;
      end loop;
      Free (B);
      New_Line;
   end Check;

begin
   Check ("Buffer 1",
          "Line 1" & Chars.LF & "Line 2" & Chars.LF & "Line 3" & Chars.LF);
   Check ("Buffer 2",
          "" & Chars.LF & "Line 2" & Chars.LF & "Line 3");
   Check ("Buffer 2",
          "" & Chars.LF & Chars.LF & "Line 3" & Chars.LF & Chars.LF);
end Main;
