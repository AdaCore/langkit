with Ada.Text_IO; use Ada.Text_IO;

package body Libfoolang.Implementation.Extensions is

   ---------------------------
   -- Foo_Node_P_Print_Addr --
   ---------------------------

   function Foo_Node_P_Print_Addr
     (Node : Bare_Foo_Node; A : System.Address) return Boolean
   is
      pragma Unreferenced (A);
   begin
      Put_Line ("FooNode.P_Print_Addr called");
      return False;
   end Foo_Node_P_Print_Addr;

end Libfoolang.Implementation.Extensions;
