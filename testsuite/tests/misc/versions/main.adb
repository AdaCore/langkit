with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang;

procedure Main is
begin
   Put_Line ("main.adb: Starting...");
   Put_Line ("version: " & Libfoolang.Version);
   Put_Line ("build date: " & Libfoolang.Build_Date);
   Put_Line ("main.adb: Done.");
end Main;
