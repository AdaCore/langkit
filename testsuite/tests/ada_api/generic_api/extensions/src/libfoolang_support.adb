package body Libfoolang_Support is
   function Canonicalize (T : Text_Type) return Symbolization_Result is
   begin
      if T = "invalid_symbol" then
         return Create_Error ("this is an invalid symbol");
      elsif
         T'Length >= 7
         and then To_Lower (T (T'First .. T'First + 6)) = "casing_"
      then
         return Create_Symbol (To_Lower (T));
      else
         return Create_Symbol (T);
      end if;
   end Canonicalize;
end Libfoolang_Support;
