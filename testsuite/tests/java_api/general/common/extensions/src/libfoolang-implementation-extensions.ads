package Libfoolang.Implementation.Extensions is

   function Foo_Node_P_Trigger_Unit_Requested
     (Node  : Bare_Foo_Node;
      Name  : Symbol_Type;
      Found : Boolean;
      Error : Boolean) return Boolean;

   function Foo_Node_P_New_Struct_With_Inner
     (Node : Bare_Foo_Node) return Internal_With_Inner;

end Libfoolang.Implementation.Extensions;
