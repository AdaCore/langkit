with Ada.Text_IO; use Ada.Text_IO;

with System.Assertions;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

procedure Rewrite is
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
begin
   if Has_Diagnostics (U) then
      Put_Line ("Errors:");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   RH := Start_Rewriting (Ctx);

   declare
      N : constant Node_Rewriting_Handle := Handle (Root (U));

      procedure Proc;

      ----------
      -- Proc --
      ----------

      procedure Proc is
      begin
         Set_Child (N, 2, Child (N, 3));
      end Proc;
   begin
      Try ("Try assigning a child that is already tied to a tree",
           Proc'Access);

      New_Line;
      Put_Line ("Replace the middle definition (b) with a clone of the last"
                & " definition (c)");
      Set_Child (N, 2, Clone (Child (N, 3)));
   end;

   if not Apply (RH) then
      raise Program_Error;
   end if;

   Root (U).Print (Show_Slocs => False);

   Put_Line ("main2.adb: Done.");
end Rewrite;
