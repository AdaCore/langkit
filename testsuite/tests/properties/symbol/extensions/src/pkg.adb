package body Pkg is

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize (Name : Text_Type) return Symbolization_Result is
      pragma Unreferenced (Name);
   begin
      return Create_Error ("no symbol allowed");
   end Canonicalize;

end Pkg;
