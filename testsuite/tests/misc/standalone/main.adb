with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;     use Libfoolang.Analysis;
with Libfoolang_Support.Text; use Libfoolang_Support.Text;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt", Buffer => "example");
begin
   if U.Has_Diagnostics then
      Put_Line ("Errors:");
      for D of U.Diagnostics loop
         Put_Line ("  " & U.Format_GNU_Diagnostic (D));
      end loop;
   else
      Put_Line ("Success: " & Image (U.Text, With_Quotes => True));
   end if;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
