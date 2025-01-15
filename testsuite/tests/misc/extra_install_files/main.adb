with Ada.Text_IO;

with Libbarlang.Analysis;
with Libfoolang.Analysis;

procedure Main is
   Foo_Ctx : Libfoolang.Analysis.Analysis_Context :=
      Libfoolang.Analysis.Create_Context;
   Bar_Ctx : Libbarlang.Analysis.Analysis_Context :=
      Libbarlang.Analysis.Create_Context;
begin
   pragma Unreferenced (Foo_Ctx, Bar_Ctx);
   Ada.Text_IO.Put_Line ("main.adb: Done.");
end Main;
