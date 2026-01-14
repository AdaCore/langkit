package body Libfoolang.Implementation.Extensions is

   -------------------------
   -- Id_P_Block_Location --
   -------------------------

   function Id_P_Block_Location
     (Node : Bare_Id) return Internal_Block_Location
   is
      Result : Internal_Block_Location;
   begin
      Get_Unit_And_PLE_Root
        (Node.Unit.Context,
         Text (Node),
         Unit_Specification,
         Result.Unit,
         Result.PLE_Root_Index);
      Populate_Lexical_Env (Result.Unit, Result.PLE_Root_Index);

      --  Internals deal with 1-based indexes, but in the Lkt world these
      --  indexes are 0-based.

      Result.PLE_Root_Index := Result.PLE_Root_Index - 1;

      return Result;
   end Id_P_Block_Location;

end Libfoolang.Implementation.Extensions;
