with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   =>
         "def foo(a b c)" & ASCII.LF
         & "def bar(x y)" & ASCII.LF
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
               E := C.P_Get (To_Unbounded_Text (To_Text (Id)));
               Put_Line ("" & Id & ": "
                         & (if E.Is_Null then "None" else Image (E.Text)));
            end;
         end loop;

         New_Line;
      end if;
   end loop;
end Main;
