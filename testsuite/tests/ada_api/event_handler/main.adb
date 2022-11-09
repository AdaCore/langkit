with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;

with Support; use Support;

procedure Main is
   Buffer : constant String := "example" & ASCII.LF;
   Ctx    : Analysis_Context;
   U      : Analysis_Unit;
   N      : Foo_Node;
   Dummy  : Boolean;
begin
   Put_Line ("main.adb: Starting...");
   New_Line;

   --  Create an analysis context with our event handler

   Ctx := Create_Context (Event_Handler => Create_Event_Handler ("MyEH"));

   --  Trigger the "unit parsed" event twice: once for the initial parsing, and
   --  once for a reparsing.

   Put_Line ("== unit parsed ==");
   New_Line;

   U := Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => Buffer);
   U := Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => Buffer);

   --  Trigger the "unit requested" event with various parameters

   Put_Line ("== unit requested ==");
   New_Line;

   N := U.Root;
   Dummy := N.P_Trigger_Unit_Requested
     (Name => To_Unbounded_Text ("foo_1"), Found => True, Error => False);
   Dummy := N.P_Trigger_Unit_Requested
     (Name => To_Unbounded_Text ("foo_2"), Found => False, Error => False);
   Dummy := N.P_Trigger_Unit_Requested
     (Name => To_Unbounded_Text ("foo_3"), Found => False, Error => True);

   Put_Line ("main.adb: Done.");
end Main;
