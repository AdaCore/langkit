with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;
with Support; use Support;

procedure General_API is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = (2 + a) + 3" & ASCII.LF
      & "def c = a + b" & ASCII.LF);

   procedure Print_Exc (Exc : Exception_Occurrence);
   --  Helper to print exception information on the standard output

   ---------------
   -- Print_Exc --
   ---------------

   procedure Print_Exc (Exc : Exception_Occurrence) is
   begin
      Put_Line ("  " & Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end Print_Exc;

   Ctx : Analysis_Context := Create_Context;
   U   : Analysis_Unit := Ctx.Get_From_Buffer ("main.txt", Buffer => Buffer);

   Dummy_U      : Analysis_Unit;
   RH, Dummy_RH : Rewriting_Handle;
   Dummy_UH     : Unit_Rewriting_Handle;
begin
   if Has_Diagnostics (U) then
      Put_Line ("Errors:");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   ---------------------------------------------------------
   -- Getting rewriting handles out of rewriting sessions --
   ---------------------------------------------------------

   if Handle (Ctx) /= No_Rewriting_Handle then
      raise Program_Error;
   end if;

   Put_Line ("Get a unit rewriting handle out of a rewriting session");
   begin
      Dummy_UH := Handle (U);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   ------------------------------------------------
   -- Creating two concurrent rewriting sessions --
   ------------------------------------------------

   Put_Line ("Create a rewriting handle");
   RH := Start_Rewriting (Ctx);
   begin
      Put_Line ("Create a second rewriting handle");
      Dummy_RH := Start_Rewriting (Ctx);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   if Handle (Ctx) /= RH then
      raise Program_Error;
   elsif Context (RH) /= Ctx then
      raise Program_Error;
   end if;

   -----------------------------
   -- Rewriting and reparsing --
   -----------------------------

   --  Test that analysis context getters that return units are properly
   --  protected against invalid uses when there is an active rewriting
   --  context.

   begin
      Put_Line ("Call Get_From_File (Reparse => False)");
      begin
         Dummy_U := Ctx.Get_From_File ("helper.txt");
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_From_File (Reparse => True)");
      begin
         Dummy_U := Ctx.Get_From_File ("helper.txt", Reparse => True);
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_From_Buffer_Str");
      begin
         Dummy_U := Ctx.Get_From_Buffer ("helper.txt", Buffer => "");
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_From_Buffer_Unb_Str");
      begin
         Dummy_U := Ctx.Get_From_Buffer
           ("helper.txt",
            Buffer => Ada.Strings.Unbounded.Null_Unbounded_String);
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_With_Error");
      begin
         Dummy_U := Ctx.Get_With_Error ("helper.txt", "error message");
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;
   end;

   -------------------------------
   -- Example rewriting session --
   -------------------------------

   Put_Line ("Get a rewriting handle for the analysis unit");
   Dummy_UH := Handle (U);

   Put_Line ("Apply the rewriting");
   Process_Apply (RH);

   if Handle (Ctx) /= No_Rewriting_Handle then
      raise Program_Error;
   end if;

   --  Test that we can do a new rewriting session once the previous one is
   --  done.

   Put_Line ("Create a second rewriting handler");
   RH := Start_Rewriting (Ctx);
   Put_Line ("Apply the rewriting");
   Process_Apply (RH);

   New_Line;
   Put_Line ("# Rewriting with a file reader");
   New_Line;
   Ctx := Create_Context (File_Reader => Create_File_Reader);
   U := Ctx.Get_From_File ("to_preprocess.txt");

   Put_Line ("Original source:");
   Put_Line (Image (U.Text, With_Quotes => True));
   New_Line;

   RH := Start_Rewriting (Ctx);
   Remove_Child (Handle (U.Root.Child (1)));
   Process_Apply (RH);
   Put_Line ("Rewritten source:");
   Put_Line (Image (U.Text, With_Quotes => True));

   Put_Line ("main.adb: Done.");
end General_API;
