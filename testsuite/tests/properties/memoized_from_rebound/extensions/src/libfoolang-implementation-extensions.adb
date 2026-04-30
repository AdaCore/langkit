with Ada.Text_IO; use Ada.Text_IO;

package body Libfoolang.Implementation.Extensions is

   ----------------------
   -- Foo_Node_P_Trace --
   ----------------------

   function Foo_Node_P_Trace (Node : Bare_Foo_Node) return Boolean is
   begin
      Put_Line ("FooNode.trace was called");
      return False;
   end Foo_Node_P_Trace;

end Libfoolang.Implementation.Extensions;
