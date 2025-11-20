with Ada.Text_IO; use Ada.Text_IO;

with System;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.C;        use Libfoolang.C;

package User_API is

   procedure Check_Context
     (Context : System.Address; Copy_Context : out System.Address)
   with Export, Convention => C, External_Name => "foo_check_context";

   procedure Check_Unit
     (Unit : System.Address; Copy_Unit : out System.Address)
   with Export, Convention => C, External_Name => "foo_check_unit";

   procedure Check_Node
     (Node : C_Node_Type; Copy_Node : out C_Node_Type)
   with Export, Convention => C, External_Name => "foo_check_node";

end User_API;
