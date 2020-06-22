with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Libfoolang.Analysis;    use Libfoolang.Analysis;

procedure Main is
begin
   begin
      Put_Line ("Text: " & No_Foo_Node.Debug_Text);
   exception
      when Property_Error =>
         Put_Line ("Got the property error");
   end;
   Put_Line ("main.adb: Done.");
end Main;
