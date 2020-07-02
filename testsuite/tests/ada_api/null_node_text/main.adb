with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Text;   use Langkit_Support.Text;
with Libfoolang.Analysis;    use Libfoolang.Analysis;

procedure Main is
begin
   begin
      Put_Line ("Text: " & Image (No_Foo_Node.Text));
   exception
      when Property_Error =>
         Put_Line ("Got the property error");
   end;
   Put_Line ("main.adb: Done.");
end Main;
