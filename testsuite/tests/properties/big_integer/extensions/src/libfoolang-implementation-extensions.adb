package body Libfoolang.Implementation.Extensions is

   ------------------------
   -- Literal_P_Evaluate --
   ------------------------

   function Literal_P_Evaluate (Node : Bare_Literal) return Big_Integer_Type is
   begin
      return Create_Big_Integer (Image (Text (Node)));
   end Literal_P_Evaluate;

end Libfoolang.Implementation.Extensions;
