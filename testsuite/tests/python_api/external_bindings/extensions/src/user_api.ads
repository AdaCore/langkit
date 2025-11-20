with Ada.Text_IO; use Ada.Text_IO;

with System;

with Libfoolang.Analysis; use Libfoolang.Analysis;

package User_API is

   procedure Check_Context
     (Context : System.Address; Copy_Context : out System.Address)
   with Export, Convention => C, External_Name => "foo_check_context";

   procedure Check_Unit
     (Unit : System.Address; Copy_Unit : out System.Address)
   with Export, Convention => C, External_Name => "foo_check_unit";

end User_API;
