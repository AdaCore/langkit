with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example # Foo " & ASCII.CR & " bar" & ASCII.CR & ASCII.LF
                  & "example # Baz" & ASCII.LF);
begin
   U.PP_Trivia;
   Put_Line ("main.adb: Done.");
end Main;
