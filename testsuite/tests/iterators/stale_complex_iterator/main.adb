with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx          : Analysis_Context;
   U            : Analysis_Unit;
   Iter_Of_Iter : Test_Struct_Iterator_Iterator;
   Dummy        : Test_Struct_Iterator;
begin
   Put_Line ("main.adb: Running...");

   Ctx := Create_Context;
   U := Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => "example");

   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   Iter_Of_Iter := U.Root.As_Example.P_Test_Struct_Iterator;
   Put_Line ("Parsing new unit");
   U := Ctx.Get_From_Buffer
     (Filename => "main2.txt",
      Buffer   => "example");

   Put_Line ("Trying to iterate...");
   begin
      if Next (Iter_Of_Iter, Dummy) then
         Put_Line ("   got an element");
      else
         Put_Line ("   got no element");
      end if;
   exception
      when Stale_Reference_Error =>
         Put_Line ("   got Stale_Reference_Error");
   end;

   Put_Line ("main.adb: Done.");
end Main;
