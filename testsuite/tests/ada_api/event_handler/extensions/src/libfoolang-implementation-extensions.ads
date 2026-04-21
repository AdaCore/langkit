package Libfoolang.Implementation.Extensions is

   function Foo_Node_P_Trigger_Unit_Requested
     (Node  : Bare_Foo_Node;
      Name  : Symbol_Type;
      Found : Boolean;
      Error : Boolean) return Boolean;

   function Foo_Node_P_Trigger_Unit_Diagnostic
     (Node    : Bare_Foo_Node;
      Message : Symbol_Type) return Boolean;

end Libfoolang.Implementation.Extensions;
