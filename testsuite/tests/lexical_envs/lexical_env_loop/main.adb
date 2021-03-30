with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;
with Libfoolang.Iterators;

procedure Main is
   C : constant Analysis_Context := Create_Context;
   U : constant Analysis_Unit := C.Get_From_File ("bar.txt");

   package I renames Libfoolang.Iterators;

begin
   GNATCOLL.Traces.Parse_Config_File;
   for El of I.Find (U.Root, I.Kind_Is (Foo_Ref)).Consume loop
      Put_Line ("Ref " & El.Image & " references " & El.As_Ref.P_Entity.Image);
   end loop;
   Put_Line ("Done.");
end Main;
