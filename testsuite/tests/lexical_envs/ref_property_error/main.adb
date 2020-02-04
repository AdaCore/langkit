with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "foo.txt",
      Buffer   => "example");
begin
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   U.Populate_Lexical_Env;

   Put_Line ("main.adb: Done.");
end Main;
