with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "a-b.txt");

   procedure Resolve;
   function Visit (Node : Foo_Node'Class) return Visit_Status;
   function Node_Image (Node : Foo_Node'Class) return String;

   -------------
   -- Resolve --
   -------------

   procedure Resolve is
      Dummy : Visit_Status;
   begin
      Put_Line ("Starting resolution...");
      Dummy := Root (Unit).Traverse (Visit'Access);
      New_Line;
   end Resolve;

   ----------------
   -- Node_Image --
   ----------------

   function Node_Image (Node : Foo_Node'Class) return String is
   begin
      if Node.Is_Null then
         return "None";
      end if;

      declare
         Fullname : constant String := Get_Filename (Node.Unit);
         Basename : constant String := +Create (+Fullname).Base_Name;
      begin
         return ("<" & Node.Kind_Name & " " & Basename & ":"
                 & Image (Node.Sloc_Range) & ">");
      end;
   end Node_Image;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Foo_Node'Class) return Visit_Status is
   begin
      if not Node.Is_Null and then Node.Kind = Foo_Var then
         declare
            V    : constant Var := Node.As_Var;
            Decl : constant Foo_Node := V.F_Value.P_Resolve;
         begin
            Put_Line ("   " & Node_Image (V) & " -> " & Node_Image (Decl));
         end;
      end if;
      return Into;
   end Visit;

begin
   GNATCOLL.Traces.Parse_Config ("LIBFOOLANG.MAIN_TRACE=yes >&1");

   Put_Line ("Performing resolution of a-b.txt without parsing a.txt...");
   --  The parent env of `a.b` in the source above will be empty since `a.txt`
   --  was not parsed yet. Thus, all references in that file will be None.
   --  Note that we don't invoke `Resolve` just to show that, we also want
   --  the lexical envs to cache the lookups to `a`, to make sure caches
   --  will be properly invalidated before running the second resolution.
   Resolve;

   Put_Line ("Performing resolution after parsing a.txt...");
   Ctx.Get_From_File ("a.txt").Populate_Lexical_Env;
   --  A named env has now been created for `a`, so the parent env of
   --  `a.b` is updated. At this stage, references used to still resolve to
   --  None, because caches populated during the first resolution were not
   --  invalidated, which should now be fixed.
   Resolve;

   Put_Line ("main.adb: Done.");
end Main;
