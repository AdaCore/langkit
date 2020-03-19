with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example");
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   Put_Line ("Iterating though array...");
   for Elem of U.Root.As_Example.P_Entities_Array loop
      Put_Line ("  " & Image (Elem));
   end loop;

   Put_Line ("Iterating though iterator...");
   declare
      Iter : constant Example_Iterator :=
         U.Root.As_Example.P_Entities_Iterator;
      Elem : Example;
   begin
      while Next (Iter, Elem) loop
         Put_Line ("  " & Image (Elem));
      end loop;
   end;

   Put_Line ("main.adb: Done.");
end Main;
