with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : Analysis_Unit;
begin
   Put_Line ("main.adb: Running...");

   U := Create_Context.Get_From_Buffer
     (Filename => "main.txt", Buffer => "example");
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      return;
   end if;

   for I in 1 .. 3 loop
      declare
         Result : constant Integer_Array := U.Root.As_Example.P_Mmz_Prop;
      begin
         Put ("root.p_mmz_prop = [");
         for Value of Result loop
            Put (Value'Image);
         end loop;
         Put_Line ("]");
      end;
   end loop;

   Put_Line ("main.adb: Done.");
end Main;
