with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example");
begin
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;
   Put_Line
     ("Result of P_Get_Bool: " & U.Root.Child (1).P_Get_Bool'Image);
   Put_Line ("main.adb: Done.");
end Main;
