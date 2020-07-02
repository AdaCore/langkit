with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "foo.txt", Buffer => "error");
   RH  : Rewriting_Handle := Start_Rewriting (Ctx);

   Node : constant Foo_Node := U.Root.Lookup ((1, 1));
   NH   : constant Node_Rewriting_Handle := Handle (Node);
begin
   Put_Line ("Unparsing " & Node.Image & ":");
   Put_Line (Image (Unparse (NH)));
   Abort_Rewriting (RH);
   Put_Line ("main.adb: done");
end Main;
