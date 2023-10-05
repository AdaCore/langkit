with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_File ("main.txt");
begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line ("  " & U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;
   U.Root.Print;
   Put_Line ("main.adb: Done.");
end Main;
