with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;

   Buffer : constant String := "example";

   U : Analysis_Unit :=
     Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => Buffer);

   E : Example := U.Root.Child (1).As_Example;
   R : Example;
begin
   U.Populate_Lexical_Env;
   R := E.P_Rebind (E, E);
   Put_Line (R.Image);

   U := Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => Buffer);
   E := U.Root.Child (1).As_Example;

   U.Populate_Lexical_Env;
   R := E.P_Rebind (E, E);

   --  A stale reference exception used to be raised on the second call to
   --  `P_Foo` below: the first call would return an entity with an
   --  incorrect safety net, which would then be checked as part of the
   --  ``Check_Safety_Net`` routine triggered during the second call.
   Put_Line (R.P_Foo.P_Foo.Image);
   Put_Line ("main.adb: Done");
end Main;
