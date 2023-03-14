package body Pkg is

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize (Name : Text_Type) return Symbolization_Result is
      Result : Text_Type (Name'Range);
      Next   : Positive := Name'First;
   begin
      for C of Name loop
         if C in 'a' .. 'z' | '_' then
            Result (Next) := C;
         elsif C in 'A' .. 'Z' then
            Result (Next) := Character_Type'Val
              (Character_Type'Pos (C)
               - Character_Type'Pos ('A')
               + Character_Type'Pos ('a'));
         else
            return Create_Error ("Invalid symbol character: '" & C & "'");
         end if;

         Next := Next + 1;
      end loop;

      return Create_Symbol (Result (Result'First .. Next - 1));
   end Canonicalize;

end Pkg;
