with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   Ctx : Analysis_Context := Create_Context;
   N   : Foo_Node;

   function Load (Filename, Buffer : String) return Analysis_Unit;
   --  Load the Filename unit using the given buffer, check that it has no
   --  parsing error and return the unit.

   procedure Try_Print_N (Label : String);
   --  Print N, or print an error message if we get a Stale_Reference_Error in
   --  the process.

   ----------
   -- Load --
   ----------

   function Load (Filename, Buffer : String) return Analysis_Unit is
   begin
      return Unit : constant Analysis_Unit := Ctx.Get_From_Buffer
        (Filename => Filename, Buffer => Buffer)
      do
         if Unit.Has_Diagnostics then
            raise Program_Error;
         end if;
         Unit.Populate_Lexical_Env;
      end return;
   end Load;

   -----------------
   -- Try_Print_N --
   -----------------

   procedure Try_Print_N (Label : String) is
   begin
      Put_Line (Label & ":");
      Put ("   ");
      begin
         Put_Line (N.Image);
      exception
         when Exc : Stale_Reference_Error =>
            Put_Line
              ("got a Stale_Reference_Error: " & Exception_Message (Exc));
      end;
   end Try_Print_N;

   U1, U2, U3 : Analysis_Unit;
begin
   Put_Line ("main.adb: Starting...");
   U1 := Load ("foo1.txt", "example # u1 version 1");
   U2 := Load ("foo2.txt", "example example # u2 version 1");
   U3 := Load ("foo3.txt", "example # u3 version 1");

   N := U1.Root.Child (1).P_Rebind
     (From_Node => U2.Root.Child (1),
      To_Node   => U2.Root.Child (2));
   Try_Print_N ("Original");

   U3 := Load ("foo3.txt", "example # u3 version 2");
   Try_Print_N ("After U3 reload");

   U2 := Load ("foo2.txt", "example example # u2 version 2");
   Try_Print_N ("After U2 reload");

   U1 := Load ("foo1.txt", "example # u1 version 2");
   Try_Print_N ("After U1 reload");

   Ctx := No_Analysis_Context;
   U1 := No_Analysis_Unit;
   U2 := No_Analysis_Unit;
   U3 := No_Analysis_Unit;
   Try_Print_N ("After context release");

   Put_Line ("main.adb: Done.");

   pragma Unreferenced (U3);
end Main;
