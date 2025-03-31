package body Libfoolang.Implementation.Extensions is

   -------------------------------------
   -- Name_P_Referenced_Unit_Or_Error --
   -------------------------------------

   function Name_P_Referenced_Unit_Or_Error
     (Node : Bare_Name; Or_Error : Boolean) return Internal_Unit
   is
      Ctx       : constant Internal_Context := Node.Unit.Context;
      Unit_Name : String := Image (Text (Node));
   begin
      for C of Unit_Name loop
         if C = '.' then
            C := '-';
         end if;
      end loop;

      declare
         Filename : constant String := Unit_Name & ".txt";
         Unit     : constant Internal_Unit :=
           (if Or_Error and then not Has_Unit (Ctx, Filename)
            then No_Analysis_Unit
            else Get_From_File (Ctx, Filename, "", False,
                                Default_Grammar_Rule));
      begin
         if Or_Error and then not Has_Unit (Ctx, Filename) then
            return No_Analysis_Unit;
         else
            declare
               Unit : constant Internal_Unit := Get_From_File
                 (Ctx, Filename, "", False, Default_Grammar_Rule);
            begin
               Populate_Lexical_Env (Unit);
               return Unit;
            end;
         end if;
      end;
   end Name_P_Referenced_Unit_Or_Error;

end Libfoolang.Implementation.Extensions;
