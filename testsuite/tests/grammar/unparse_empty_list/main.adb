with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

procedure Main is
   Buffer : constant String := "(a, )";

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "main.txt", Buffer => Buffer);
begin
   Put_Line ("main.adb: starting...");

   if Has_Diagnostics (U) then
      raise Program_Error;
   end if;

   declare
      RH        : Rewriting_Handle := Start_Rewriting (Ctx);
      Dummy_URH : constant Unit_Rewriting_Handle := Handle (U);
      R         : constant Apply_Result := Apply (RH);
   begin
      if not R.Success then
         Abort_Rewriting (RH);
         raise Program_Error;
      end if;
   end;
   Put_Line (Image (U.Text));
   New_Line;

   Put_Line ("main.adb: done.");
end Main;
