with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;
with Libfoolang.Iterators;

procedure Main is
   C : Analysis_Context;
   U : Analysis_Unit;

   package I renames Libfoolang.Iterators;

begin
   Put_Line ("main.adb: Starting...");

   C := Create_Context;
   U := C.Get_From_File ("foo.txt");
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   for El of I.Find (U.Root, I.Kind_Is (Foo_Ref)).Consume loop
      Put_Line (El.Image & " resolves to:");
      begin
         for N of El.As_Ref.P_Resolve loop
            Put_Line ("  " & N.Image);
         end loop;
      exception
         when Exc : Property_Error =>
            Put_Line
              ("   ... got a Property_Error: " & Exception_Message (Exc));
      end;
   end loop;

   Put_Line ("main.adb: Done.");
end Main;
