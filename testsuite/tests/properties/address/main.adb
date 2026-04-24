with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx   : constant Analysis_Context := Create_Context;
   U     : constant Analysis_Unit :=
     Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => "example");
   Dummy : Boolean;
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   Put_Line ("Calling FooNode.P_P");
   Dummy := U.Root.Child (1).P_P;

   Put_Line ("main.adb: Done.");
end Main;
