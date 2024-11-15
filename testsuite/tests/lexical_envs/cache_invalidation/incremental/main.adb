--  Check that env caches are correctly invalidated when we reparse a unit that
--  had a null node previously.

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libfoolang.Analysis;  use Libfoolang.Analysis;

procedure Main is
   A_Buffer : constant String := "+b def a: -b";
   B_Buffer : constant String := "def b";
   C_Buffer : constant String := "def c: -c";

   Ctx     : Analysis_Context;
   A, B, C : Analysis_Unit;

   procedure Check (U : Analysis_Unit);

   -----------
   -- Check --
   -----------

   procedure Check (U : Analysis_Unit) is
   begin
      Put_Line ("From " & U.Root.Image & ":");
      for R of U.Root.As_Comp_Unit.F_Refs loop
         Put_Line
           ("  " & Image (R.Text) & " refers to: "
            & R.P_Designated_Unit.Image);
      end loop;
      New_Line;
   end Check;

begin
   --  This is not needed to trigger the bug: sanity check env lookups in a
   --  non-incremental scenario.

   Put_Line ("# In one go");
   Ctx := Create_Context;
   A := Ctx.Get_From_Buffer ("a.txt", Buffer => A_Buffer);
   C := Ctx.Get_From_Buffer ("c.txt", Buffer => C_Buffer);
   B := Ctx.Get_From_Buffer ("b.txt", Buffer => B_Buffer);
   Check (C);
   Check (A);

   --  Run the following step, which used to make env lookup use stale caches:
   --
   --  * Perform a first lookup on a.txt when a dependency (b.txt) was not
   --    loaded (this populates env caches).
   --
   --  * Load both b.txt and c.txt (this should invalidate the caches, in
   --    particular in the root env).
   --
   --  * Run lookups on c.txt to create valid cache on the root lexical
   --    environment.
   --
   --  * Run lookups on a.txt: this used to use stale cache from an env in
   --    a.txt. We now expect the reloading of b.txt to invalidate all caches.

   Put_Line ("# Incremental");
   Ctx := Create_Context;
   A := Ctx.Get_From_Buffer ("a.txt", Buffer => A_Buffer);
   Check (A);
   C := Ctx.Get_From_Buffer ("c.txt", Buffer => C_Buffer);
   B := Ctx.Get_From_Buffer ("b.txt", Buffer => B_Buffer);
   Check (C);
   Check (A);

   Put_Line ("main.adb: Done.");

   pragma Unreferenced (B);
end Main;
