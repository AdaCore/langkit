with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "foo.txt", Buffer => "example");
begin
   if Has_Diagnostics (Unit) then
      for D of Diagnostics (Unit) loop
         Put_Line (To_Pretty_String (D));
      end loop;
      raise Program_Error;
   end if;

   declare
      Dummy : Boolean;
   begin
      Dummy := Unit.Root.P_Prop;
      Put_Line ("No exception...");
   exception
      when Exc : Property_Error =>
         Put_Line ("Got an exception!");
         Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end;

   Put_Line ("main.adb: Done");
end Main;
