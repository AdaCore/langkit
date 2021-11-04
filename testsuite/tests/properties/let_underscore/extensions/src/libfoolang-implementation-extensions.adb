with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

package body Libfoolang.Implementation.Extensions is

   --------------------
   -- Bar_Node_P_Foo --
   --------------------

   function Bar_Node_P_Foo
     (Node : Bare_Bar_Node;
      I    : Integer) return Big_Integer_Type
   is
      pragma Unreferenced (Node);
   begin
      Put_Line ("BarNode.p_foo:" & I'Image);
      return Create_Big_Integer (I);
   end Bar_Node_P_Foo;

end Libfoolang.Implementation.Extensions;
