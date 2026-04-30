package Libfoolang.Implementation.Extensions is

   function Literal_Sequence_P_Get_Foreign_Node
     (Node : Bare_Literal_Sequence) return Bare_Foo_Node;

   function Literal_Sequence_P_Print_Msg
     (Node : Bare_Literal_Sequence; N : Node_Builder_Type) return Boolean;

end Libfoolang.Implementation.Extensions;
