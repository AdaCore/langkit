with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Apply_Error is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "main.txt", Buffer => "");
   RH  : Rewriting_Handle := Start_Rewriting (Ctx);
   N   : constant Node_Rewriting_Handle := Handle (Root (U));
begin
   Append_Child
     (N, Create_Def
           (RH, Create_Token_Node (RH, Foo_Literal, "1"),
                No_Node_Rewriting_Handle,
                Create_Token_Node (RH, Foo_Name, "a")));
   Process_Apply (RH, Abort_On_Error => False);

   Abort_Rewriting (RH);
   Put_Line ("apply_error.adb: Done.");
end Apply_Error;
