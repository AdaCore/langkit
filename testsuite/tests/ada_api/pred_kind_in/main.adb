with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Iterators; use Libfoolang.Iterators;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     ("foo.txt", Buffer => "var a = 1; var b = a; def f;");
begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      return;
   end if;

   declare
      It : constant Traverse_Iterator'Class :=
         Find (U.Root, Kind_In (Foo_Decl'First, Foo_Decl'Last));
   begin
      for N of It.Consume loop
         Put_Line (N.Image);
      end loop;
   end;

   Put_Line ("Done.");
end Main;
