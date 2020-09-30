with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example");
begin
   GNATCOLL.Traces.Parse_Config_File;
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;
   for I in 1 .. 2 loop
      Put ("Attempt" & I'Image & "... ");
      Put_Line (U.Root.As_Example.F_My_Field'Image);
   end loop;
   Put_Line ("main.adb: Done.");
end Main;
