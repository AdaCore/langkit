with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Introspection; use Libfoolang.Introspection;
with Libfoolang.Rewriting;     use Libfoolang.Rewriting;

procedure Revert is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = (2 + a) + 3" & ASCII.LF
      & "def c = a + b" & ASCII.LF
      & "def d = 4" & ASCII.LF
      & "def e = 5" & ASCII.LF);

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "main.txt", Buffer => Buffer);

   RH       : Rewriting_Handle := Start_Rewriting (Ctx);
   Def_List : constant Node_Rewriting_Handle := Handle (Root (U));
   Def_A    : constant Node_Rewriting_Handle := Child (Def_List, 1);
begin
   --  Alter the tree so that rewriting will fail and so that the resulting
   --  list of tokens it much smaller.
   Set_Child
     (Def_A, Index (Kind (Def_A), Def_F_Name), No_Node_Rewriting_Handle);
   Remove_Child (Def_List, 5);
   Remove_Child (Def_List, 4);

   --  Try to apply the rewriting
   if Apply (RH).Success then
      raise Program_Error;
   end if;

   --  Make sure we can still print the tree, i.e. that pre-rewriting data is
   --  still available.
   Root (U).Print;

   Abort_Rewriting (RH);
   Put_Line ("revert.adb: Done.");
end Revert;
