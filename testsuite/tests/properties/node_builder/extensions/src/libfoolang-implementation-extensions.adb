with Ada.Text_IO; use Ada.Text_IO;

package body Libfoolang.Implementation.Extensions is

   -----------------------------------------
   -- Literal_Sequence_P_Get_Foreign_Node --
   -----------------------------------------

   function Literal_Sequence_P_Get_Foreign_Node
     (Node : Bare_Literal_Sequence) return Bare_Foo_Node
   is
      Foreign_Unit : constant Internal_Unit :=
        Get_From_Buffer
          (Context  => Node.Unit.Context,
           Filename => "foreign.txt",
           Charset  => "",
           Buffer   => "(foreign 10, 11, 12)",
           Rule     => Default_Grammar_Rule);
   begin
      return Foreign_Unit.Ast_Root;
   end Literal_Sequence_P_Get_Foreign_Node;

   ----------------------------------
   -- Literal_Sequence_P_Print_Msg --
   ----------------------------------

   function Literal_Sequence_P_Print_Msg
     (Node : Bare_Literal_Sequence; N : Node_Builder_Type) return Boolean
   is
   begin
      Put_Line ("... Literal_Sequence.P_Print_Msg called with:");
      Put ("    ");
      Put_Line (Trace_Image (N));
      return False;
   end Literal_Sequence_P_Print_Msg;

end Libfoolang.Implementation.Extensions;
