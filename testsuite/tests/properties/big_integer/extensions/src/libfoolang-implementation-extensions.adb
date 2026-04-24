with Ada.Text_IO; use Ada.Text_IO;

package body Libfoolang.Implementation.Extensions is

   --------------------------
   -- Foo_Node_P_Print_Int --
   --------------------------

   function Foo_Node_P_Print_Int
     (Node : Bare_Foo_Node; I : Big_Integer_Type) return Boolean is
   begin
      Put_Line ("FooNode.P_Print_Int called with " & I.Value.Image);
      return False;
   end Foo_Node_P_Print_Int;

   ------------------------
   -- Literal_P_Evaluate --
   ------------------------

   function Literal_P_Evaluate (Node : Bare_Literal) return Big_Integer_Type is
   begin
      return Create_Big_Integer (Image (Text (Node)));
   end Literal_P_Evaluate;

end Libfoolang.Implementation.Extensions;
