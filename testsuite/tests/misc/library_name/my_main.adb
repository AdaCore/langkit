with Ada.Text_IO; use Ada.Text_IO;

with Bar;

procedure My_Main is
begin
   Put_Line ("Foo/Bar version: " & Bar.Version);
end My_Main;
