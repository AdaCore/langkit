with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "foo example");

   I : Identifier;
   E : Example;
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   I := U.Root.Child (1).As_Identifier;
   Put_Line (I.Image & ".F_My_Field =" & I.F_My_Field'Image);

   E := U.Root.Child (2).As_Example;
   Put_Line (E.Image & ".F_My_Field =" & E.F_My_Field'Image);

   Put_Line ("main.adb: Done.");
end Main;
