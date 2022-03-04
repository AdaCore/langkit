with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "buffer",
      Buffer   => "var foo()");
begin
   for D of U.Diagnostics loop
      Put_Line (U.Format_GNU_Diagnostic (D));
   end loop;
   U.Root.Print;
end Main;
