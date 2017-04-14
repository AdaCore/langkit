package body Langkit_Support.Adalog.Eq_Same is

   function Convert
     (C_Data : Dummy_Convert_Data; From : LR_Type) return LR_Type
   is
      pragma Unreferenced (C_Data);
   begin
      Inc_Ref (From);
      return From;
   end Convert;

end Langkit_Support.Adalog.Eq_Same;
