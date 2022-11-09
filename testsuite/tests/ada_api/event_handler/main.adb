with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;

with Support; use Support;

procedure Main is
   Ctx : Analysis_Context;

   Dummy_Unit : Analysis_Unit;

begin
   Put_Line ("main.adb: Starting...");

   --  Create a context with our file reader

   declare
      My_Event_Handler : constant Event_Handler_Reference :=
         Create_Event_Handler_Reference
           (Event_Handler'(null record));
   begin
      Ctx := Create_Context (Event_Handler => My_Event_Handler);
   end;

   --  Check the file reader is used appropriately when reading files, and that
   --  common errors are properly reported.

   Dummy_Unit := Ctx.Get_From_File ("direct-ok.txt", "");
   Dummy_Unit := Dummy_Unit.Root.P_Get_Unit
     (To_Unbounded_Text ("pouet"));

   Put_Line ("main.adb: Done.");
end Main;
