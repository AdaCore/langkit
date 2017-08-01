package body Langkit_Support.Adalog.Main_Support is

   ------------------------
   -- Free_Relation_Tree --
   ------------------------

   procedure Free_Relation_Tree (R : in out Relation) is
   begin
      for C of R.Children loop
         declare
            C_Var : Relation := C;
         begin
            Free_Relation_Tree (C_Var);
            while C_Var.Ref_Count > 1 loop
               Dec_Ref (C_Var);
            end loop;
         end;
      end loop;
      Dec_Ref (R);
   end Free_Relation_Tree;

end Langkit_Support.Adalog.Main_Support;
