with Ada.Text_IO; use Ada.Text_IO;

with System.Assertions;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

procedure Main is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = (2 + a) + 3" & ASCII.LF
      & "def c = a + b" & ASCII.LF);

   procedure Try (Label : String; Proc : access procedure);

   ---------
   -- Try --
   ---------

   procedure Try (Label : String; Proc : access procedure) is
   begin
      Put_Line (Label & "...");
      Proc.all;
      Put_Line ("   Done with no assert failure");
   exception
      when System.Assertions.Assert_Failure =>
         Put_Line ("   Got an assert failure");
   end Try;

   Ctx : constant Analysis_Context := Create;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "main.txt", Buffer => Buffer);
   RH  : Rewriting_Handle;
   UH  : Unit_Rewriting_Handle;
begin
   if Has_Diagnostics (U) then
      Put_Line ("Errors:");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   if Handle (Ctx) /= No_Rewriting_Handle then
      raise Program_Error;
   end if;

   declare
      procedure Proc;

      procedure Proc is
         Dummy : constant Unit_Rewriting_Handle := Handle (U);
      begin
         null;
      end Proc;
   begin
      Try ("Try to get a unit rewriting handle out of a rewriting session",
           Proc'Access);
   end;

   declare
      procedure Proc;

      procedure Proc is
      begin
         RH := Start_Rewriting (Ctx);
      end Proc;
   begin
      Put_Line ("Create a rewriting handle");
      Proc;
      Try ("Try to create a second rewriting handle", Proc'Access);
   end;

   if Handle (Ctx) /= RH then
      raise Program_Error;
   elsif Context (RH) /= Ctx then
      raise Program_Error;
   end if;

   Put_Line ("Get a rewriting handle for the analysis unit");
   UH := Handle (U);
   pragma Unreferenced (UH);

   Put_Line ("Apply the rewriting");
   if not Apply (RH) then
      --  We don't expect any failure as we did not do any modification to
      --  trees!
      raise Program_Error;

   elsif Handle (Ctx) /= No_Rewriting_Handle then
      raise Program_Error;
   end if;

   --  Test that we can do a new rewriting session once the previous one is
   --  done.

   Put_Line ("Create a second rewriting handler");
   RH := Start_Rewriting (Ctx);
   Put_Line ("Apply the rewriting");
   if not Apply (RH) then
      --  We don't expect any failure as we did not do any modification to
      --  trees!
      raise Program_Error;
   end if;

   Put_Line ("main.adb: Done.");
end Main;
