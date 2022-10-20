with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure C_Main with Import, Convention => C, External_Name => "c_main";
begin
   C_Main;

   Put_Line ("main.adb: Done.");
end Main;
