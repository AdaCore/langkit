with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   C : constant Analysis_Context := Create_Context;
   U : constant Analysis_Unit := C.Get_From_Buffer
     ("main.txt", Buffer => "example");
begin
   GNATCOLL.Traces.Parse_Config ("LANGKIT.*=yes");
   Put_Line ("Calling P_Prop:");
   for N of U.Root.P_Prop loop
      Put_Line ("  " & N.Image);
   end loop;
   Put_Line ("Done.");
end Main;
