with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Buffer : constant Unbounded_String := To_Unbounded_String ("example");
   U      : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt", Buffer => Buffer);
begin
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;
   U.Root.Print;
   Put_Line ("Done.");
end Main;
