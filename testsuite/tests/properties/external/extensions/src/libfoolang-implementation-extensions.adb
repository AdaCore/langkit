package body Libfoolang.Implementation.Extensions is

   ----------------------
   -- Literal_P_Result --
   ----------------------

   function Literal_P_Result (Node : Bare_Literal) return Integer is
   begin
       return Integer'Value (Image (Text (Node)));
   end Literal_P_Result;

end Libfoolang.Implementation.Extensions;
