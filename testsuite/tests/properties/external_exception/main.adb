with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

with Libfoolang_Support; use Libfoolang_Support;

procedure Main is
   U : constant Analysis_Unit :=
     Create_Context.Get_From_Buffer
       (Filename => "main.txt", Buffer => "example");
begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   Put_Line ("Calling P_Do_Raise...");
   declare
      Dummy : Boolean;
   begin
      Dummy := U.Root.P_Do_Raise;
   exception
      when Exc : My_Exception =>
         Put_Line
           ("Got " & Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end;

   Put_Line ("main.adb: Done");
end Main;
