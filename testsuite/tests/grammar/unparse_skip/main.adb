with Ada.Text_IO;       use Ada.Text_IO;
with System.Assertions; use System.Assertions;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;
with Libfoolang.Unparsing; use Libfoolang.Unparsing;

with Libfoolang.Common;

procedure Main is
   Buffer : constant String :=
      "def a = 1;"
      & ASCII.LF & "b;"
      & ASCII.LF & "def c = 2;";

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
   end if;
   New_Line;

   declare
      R : constant Foo_Node'Class := Root (U);
   begin
      Put_Line ("Unparsing: " & Unparse (R));
         raise Program_Error;
   exception
      when Assert_Failure =>
         Put_Line ("Assertion failure on unparsing badly parsed unit");
   end;
   New_Line;

   declare
      RH  : Rewriting_Handle := Start_Rewriting (Ctx);
      URH : Unit_Rewriting_Handle with Unreferenced;
      NRH : Node_Rewriting_Handle with Unreferenced;
   begin
      begin
         URH := Handle (U);
         raise Program_Error;
      exception
         when Libfoolang.Common.Precondition_Failure =>
            Put_Line ("Assertion failure on getting badly parsed unit"
                      & " rewriting handle");
      end;
      begin
         NRH := Handle (U.Root);
         raise Program_Error;
      exception
         when Libfoolang.Common.Precondition_Failure =>
            Put_Line ("Assertion failure on getting rewriting handle for root"
                      & " of badly parsed unit");
      end;

      Abort_Rewriting (RH);
   end;
   New_Line;

   Put_Line ("main.adb: done.");
end Main;
