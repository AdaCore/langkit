with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Unparsing; use Libfoolang.Unparsing;

procedure Main is
   Buffer : constant String :=
      "def {1};" & ASCII.LF
      & "def (a);" & ASCII.LF;

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "main.txt", Buffer => Buffer);
begin
   Put_Line ("main.adb: starting...");

   if Has_Diagnostics (U) then
      Put_Line ("Errors:");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   declare
      R : constant Foo_Node'Class := Root (U);
   begin
      Put_Line (Unparse (R));
   end;
   New_Line;
   Put_Line ("main.adb: done.");
end Main;
