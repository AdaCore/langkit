with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text;  use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is

   Helper_Buffer_Complete : constant String :=
     "a { def{b c} }"
     & ASCII.LF & "a { def{b} }"
     & ASCII.LF & "a { def{c d} }";
   Helper_Buffer_Single   : constant String := "a { def{b} }";
   Main_Buffer            : constant String := "z { +{a} a b c d }";

   Ctx    : constant Analysis_Context := Create_Context;
   Helper : constant Analysis_Unit :=
     Ctx.Get_From_Buffer ("helper.txt", Buffer => "");
   Main   : constant Analysis_Unit :=
     Ctx.Get_From_Buffer ("main.txt", Buffer => Main_Buffer);

   procedure Process (Label, Helper_Buffer : String);
   --  Parse ``Helper_Buffer`` as the ``helper.txt`` unit, print diagnostics if
   --  there are parsing errors and try to resolve all references in the main
   --  unit.

   function Visit (Node : Foo_Node'Class) return Visit_Status;
   --  If ``Node`` is a reference, try to resolve it. Return ``Into``.

   -------------
   -- Process --
   -------------

   procedure Process (Label, Helper_Buffer : String) is
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;

      --  Reparse the buffer and check for parsing errors

      Helper.Reparse (Buffer => Helper_Buffer);
      for D of Helper.Diagnostics loop
         Put_Line (Helper.Format_GNU_Diagnostic (D));
      end loop;

      --  Resolve all references

      Main.Root.Traverse (Visit'Access);

      New_Line;
   end Process;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Foo_Node'Class) return Visit_Status is
   begin
      if Node.Kind = Foo_Ref then
         declare
            R    : constant Ref := Node.As_Ref;
            Decl : constant Foo_Node := R.P_Resolve;
         begin
            Put_Line ("  " & Image (R.Text) & " -> " & Decl.Image);
         end;
      end if;
      return Into;
   end Visit;

begin
   --  Note that the fact that we work on the same Helper unit helps checking
   --  that "update after reparse" processings work as expected.

   --  No definitions: all refs should resolve to None without error

   Process ("empty", "");

   --  No PLE root (single scope): refs should resolve to it

   Process ("single", Helper_Buffer_Single);

   --  Multiple PLE roots, automatic PLE on the 2nd one: refs should resolve
   --  only to defs in that second PLE root.

   Process ("complete", Helper_Buffer_Complete);

   --  Multiple PLE roots, manual PLE on all roots: refs will resolve to
   --  whatever comes first. This checks that we indeed have something
   --  different from when only one PLE root has envs populated.

   Helper.Populate_Lexical_Env (1);
   Helper.Populate_Lexical_Env (2);
   Helper.Populate_Lexical_Env (3);
   Process ("complete - PLE all", Helper_Buffer_Complete);
end Main;
