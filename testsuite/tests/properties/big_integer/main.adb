with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   =>
         "def a0 = 2"
         & ASCII.LF & "def a = 10000000000000000000000000000000"
         & ASCII.LF & "def b = a + 1"
         & ASCII.LF & "def c = b - 1"
         & ASCII.LF
         & ASCII.LF & "def d = a = b"
         & ASCII.LF & "def e = a = c"
         & ASCII.LF & "def f = a < b"
         & ASCII.LF & "def g = a < c");
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   Put_Line ("P_Check_Big_Literal: " & U.Root.P_Check_Big_Literal.Image);

   for I in 1 .. U.Root.Children_Count loop
      declare
         D  : constant Decl := U.Root.Child (I).As_Decl;
         E  : constant Expr := D.F_Expr_Tree;
         BI : constant Big_Integer := E.P_Evaluate;
      begin
         Put_Line (Image (D.F_Name.Text) & " evaluates to " & BI.Image);
      end;
   end loop;
   Put_Line ("main.adb: Done.");
end Main;
