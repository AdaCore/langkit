package body Libfoolang.Helpers is

   -------------------------
   -- Canonicalize_Symbol --
   -------------------------

   function Canonicalize_Symbol
     (Name : Text_Type) return Symbolization_Result
   is
      Result : Text_Type := Name;
   begin
      for C of Result loop
         case C is
         when 'a' .. 'z' => null;
         when 'A' .. 'Z' => C := To_Lower (C);
         when others     =>
            return Create_Error ("invalid symbol character: " & C);
         end case;
      end loop;
      return Create_Symbol (Result);
   end Canonicalize_Symbol;

end Libfoolang.Helpers;
