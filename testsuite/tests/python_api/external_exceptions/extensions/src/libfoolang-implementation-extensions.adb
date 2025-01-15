with Helpers;

package body Libfoolang.Implementation.Extensions is

   ---------------------
   -- Foo_Node_P_Prop --
   ---------------------

   function Foo_Node_P_Prop (Self : Bare_Foo_Node) return Integer is
   begin
      return (raise Helpers.My_External_Exception with "my exception message");
   end Foo_Node_P_Prop;

end Libfoolang.Implementation.Extensions;
