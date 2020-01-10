package body Libfoolang.Implementation.Extensions is

   -------------------
   -- Number_P_Eval --
   -------------------

   function Number_P_Eval (Node : Bare_Number) return Integer is
   begin
       return Integer'Value (Image (Text (Node)));
   end Number_P_Eval;

end Libfoolang.Implementation.Extensions;
