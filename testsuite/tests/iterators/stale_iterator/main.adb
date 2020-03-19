with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common; use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example");
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   declare
      Iter : constant Example_Iterator :=
         U.Root.As_Example.P_Entities_Iterator;
      Elem : Example;
   begin
      Put ("main.adb: Iterating once: ");
      if Next (Iter, Elem) then
         Put_Line (Image (Elem));
      else
         New_Line;
      end if;

      Put_Line ("main.adb: Parsing new unit");
      U := Ctx.Get_From_Buffer
        (Filename => "main2.txt",
         Buffer   => "example");

      Put ("main.adb: Iterating once more: ");
      if Next (Iter, Elem) then
         Put_Line (Image (Elem));
      else
         New_Line;
      end if;
   exception
      when Stale_Reference_Error =>
         Put_Line ("<Stale_Reference_Error>");
   end;

   Put_Line ("main.adb: Done.");
end Main;
