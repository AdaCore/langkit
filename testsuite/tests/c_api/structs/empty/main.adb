procedure Main is
   procedure C_Main with Import, Convention => C, External_Name => "c_main";
begin
   C_Main;
end Main;
