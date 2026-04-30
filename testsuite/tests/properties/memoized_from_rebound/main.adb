with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
     Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => "example");
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   for From_Rebound in Boolean loop
      Put_Line
        ("About to call FooNode.P_Trigger (From_Rebound => "
         & From_Rebound'Image & "):");
      Put_Line ("  -> " & U.Root.P_Trigger (From_Rebound)'Image);
      New_Line;
   end loop;

   Put_Line ("main.adb: Done.");
end Main;
