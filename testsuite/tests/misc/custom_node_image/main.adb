with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit :=
     Create_Context.Get_From_Buffer ("main.txt", Buffer => "def foo");
begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line ("  " & U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   Put_Line (U.Root.Image);
   Put_Line (U.Root.Child (1).Image);
   Put_Line ("main.adb: Done.");
end Main;
