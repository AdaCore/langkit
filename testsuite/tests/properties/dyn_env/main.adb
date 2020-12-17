with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   =>
         "cons1 = 100" & ASCII.LF
         & "cons2 = 200" & ASCII.LF
         & "def foo(a=10 b=20 c=cons1)" & ASCII.LF
         & "def bar(x=99 y=cons2)" & ASCII.LF
         & "foo(1 2 3)" & ASCII.LF
         & "bar(x y)" & ASCII.LF);

   C : Call_Expr;
   E : Expr;

begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   for N of U.Root.Children loop
      if N.Kind = Foo_Call_Expr then
         C := N.As_Call_Expr;
         Put_Line ("== " & C.Image & " ==");

         for Id_Char of String'("abcdexyz") loop
            declare
               Id : constant String := (1 => Id_Char);
            begin
               Put_Line (Id & ":");

               E := C.P_Get_Arg (To_Unbounded_Text (To_Text (Id)));
               Put_Line ("  (arg):  "
                         & (if E.Is_Null then "None" else Image (E.Text)));

               E := C.P_Get_Arg_Expr (To_Unbounded_Text (To_Text (Id)));
               Put_Line ("  (expr): "
                         & (if E.Is_Null then "None" else Image (E.Text)));
            end;
         end loop;

         New_Line;
      end if;
   end loop;
end Main;
