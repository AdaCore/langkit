with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Preserve_Formatting_Wrap is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
      Get_From_File (Ctx, "preserve_formatting_wrap.txt");
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

   Put_Line ("Wrap a's expression in a parenthesized expression...");
   declare
      Def_List : constant Node_Rewriting_Handle := Handle (Root (U));
      Def_A    : constant Node_Rewriting_Handle := Child (Def_List, 1);
      Expr     : constant Node_Rewriting_Handle :=
        Child (Def_A, Member_Refs.Decl_F_Expr);
      Paren    : constant Node_Rewriting_Handle :=
        Create_Regular_Node (RH, Foo_Paren_Expr,
                             (1 => No_Node_Rewriting_Handle));
   begin
      Replace (Expr, Paren);
      Set_Child (Paren, 1, Expr);
   end;
   New_Line;

   Put_Line ("Applying the diff...");
   Process_Apply (RH);

   New_Line;
   Put_Line ("Quoting source buffer for rewritten unit...");
   Put_Line (Encode (Text (U), "ASCII"));

   Put_Line ("preserve_formatting.adb: Done.");
end Preserve_Formatting_Wrap;
