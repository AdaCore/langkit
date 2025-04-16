package body Libfoolang.Implementation.Extensions is

   -------------------------
   -- Unit_Import_P_Fetch --
   -------------------------

   function Unit_Import_P_Fetch (Node : Bare_Unit_Import) return Bare_Comp_Unit
   is
      Unit_Name : constant String := Image (Text (Node.Unit_Import_F_N));
      Filename  : constant String := Unit_Name & ".txt";
      U         : constant Internal_Unit :=
        Get_From_File
          (Context  => Node.Unit.Context,
           Filename => Filename,
           Charset  => "",
           Reparse  => False,
           Rule     => Main_Rule_Rule);
   begin
      Populate_Lexical_Env (U, 1);
      return U.Ast_Root;
   end Unit_Import_P_Fetch;

end Libfoolang.Implementation.Extensions;
