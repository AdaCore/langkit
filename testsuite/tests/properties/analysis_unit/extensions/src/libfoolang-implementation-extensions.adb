package body Libfoolang.Implementation.Extensions is

   ----------------------
   -- Literal_P_Result --
   ----------------------

   function Literal_P_Result (Node : Bare_Literal) return Integer is
       N : constant Bare_Foo_Node := Node;
   begin
       return Integer'Value (Image (Text (N)));
   end Literal_P_Result;

   ----------------------------
   -- Name_P_Designated_Unit --
   ----------------------------

   function Name_P_Designated_Unit (Node : Bare_Name) return Internal_Unit is
       Filename : constant String := Image (Text (Node)) & ".txt";
       Context  : constant Internal_Context := Node.Unit.Context;
   begin
       return Get_From_File (Context, Filename, Default_Charset,
                             Reparse => False,
                             Rule    => Default_Grammar_Rule);
   end Name_P_Designated_Unit;

end Libfoolang.Implementation.Extensions;
