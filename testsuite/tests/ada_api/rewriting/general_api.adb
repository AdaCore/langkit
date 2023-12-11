with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;
with Support; use Support;

procedure General_API is
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
      Put_Line ("   Done with no precondition failure");
   exception
      when Libfoolang.Common.Precondition_Failure =>
         Put_Line ("   Got a precondition failure");
   end Try;

   Ctx : Analysis_Context := Create_Context;
   U   : Analysis_Unit := Ctx.Get_From_Buffer ("main.txt", Buffer => Buffer);
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

   --  Test that analysis context getters that return units are properly
   --  protected against invalid uses when there is an active rewriting
   --  context.
   declare
      procedure Do_Get_From_File;
      procedure Do_Get_From_File_Reparse;
      procedure Do_Get_From_Buffer_Str;
      procedure Do_Get_From_Buffer_Unb_Str;
      procedure Do_Get_With_Error;

      Dummy : Analysis_Unit;

      procedure Do_Get_From_File is
      begin
         Dummy := Ctx.Get_From_File ("helper.txt");
      end Do_Get_From_File;

      procedure Do_Get_From_File_Reparse is
      begin
         Dummy := Ctx.Get_From_File ("helper.txt", Reparse => True);
      end Do_Get_From_File_Reparse;

      procedure Do_Get_From_Buffer_Str is
      begin
         Dummy := Ctx.Get_From_Buffer ("helper.txt", Buffer => "");
      end Do_Get_From_Buffer_Str;

      procedure Do_Get_From_Buffer_Unb_Str is
      begin
         Dummy := Ctx.Get_From_Buffer
           ("helper.txt",
            Buffer => Ada.Strings.Unbounded.Null_Unbounded_String);
      end Do_Get_From_Buffer_Unb_Str;

      procedure Do_Get_With_Error is
      begin
         Dummy := Ctx.Get_With_Error ("helper.txt", "error message");
      end Do_Get_With_Error;
   begin
      Try ("Call Get_From_File (Reparse => False)", Do_Get_From_File'Access);
      Try ("Call Get_From_File (Reparse => True)",
           Do_Get_From_File_Reparse'Access);
      Try ("Call Get_From_Buffer_Str", Do_Get_From_Buffer_Str'Access);
      Try ("Call Get_From_Buffer_Unb_Str", Do_Get_From_Buffer_Unb_Str'Access);
      Try ("Call Get_With_Error", Do_Get_With_Error'Access);
   end;

   Put_Line ("Get a rewriting handle for the analysis unit");
   UH := Handle (U);
   pragma Unreferenced (UH);

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
