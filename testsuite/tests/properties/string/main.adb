with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libfoolang.Analysis;  use Libfoolang.Analysis;

procedure Main is
   U : Analysis_Unit;
begin
   Put_Line ("main.adb: Running...");

   U := Create_Context.Get_From_File ("main.txt");
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   declare
      N   : constant Example := U.Root.As_Example;
      Arg : constant Text_Type := "h" & Character_Type'Val (16#e9#) & "llo";
   begin
      Put_Line
        (".identity(" & Image (Arg, With_Quotes => True) & ") = "
         & Image (N.P_Identity (Arg), With_Quotes => True));

      Put_Line
        (".extend(" & Image (Arg, With_Quotes => True) & ") = "
         & Image (N.P_Extend (Arg), With_Quotes => True));

      Put_Line (".newline() = " & Image (N.P_Newline, With_Quotes => True));
   end;

   Put_Line ("main.adb: Done.");
   New_Line;
end Main;
