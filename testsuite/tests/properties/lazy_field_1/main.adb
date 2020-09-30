with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "1 + 2");

   function Process (Node : Foo_Node'Class) return Visit_Status;
   --  If Node is an expression, print its lazy field

   -------------
   -- Process --
   -------------

   function Process (Node : Foo_Node'Class) return Visit_Status is
   begin
      if Node.Kind in Foo_Expr then
         Put_Line (Node.Image & " -> " & Node.As_Expr.F_My_Field'Image);
      end if;
      return Into;
   end Process;

begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;
   U.Root.Traverse (Process'Access);
   Put_Line ("main.adb: Done.");
end Main;
