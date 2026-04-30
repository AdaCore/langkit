package Libfoolang.Implementation.Extensions is

   function Foo_Node_P_Print_Int
     (Node : Bare_Foo_Node; I : Big_Integer_Type) return Boolean;

   function Literal_P_Evaluate (Node : Bare_Literal) return Big_Integer_Type;

end Libfoolang.Implementation.Extensions;
