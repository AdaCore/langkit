package body Langkit_Support.Adalog.Main_Support is

   ------------------------
   -- Free_Relation_Tree --
   ------------------------

   procedure Free_Relation_Tree (R : in out Relation) is
      Var : Relation;
   begin
      for C of R.Children loop
         Var := C;
         Free_Relation_Tree (Var);
         while Var /= null and then Var.Ref_Count > 1 loop
            Dec_Ref (Var);
         end loop;
      end loop;
      Var := R;
      Dec_Ref (Var);
      R := null;
   end Free_Relation_Tree;

end Langkit_Support.Adalog.Main_Support;
