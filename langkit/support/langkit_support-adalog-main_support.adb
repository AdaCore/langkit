package body Langkit_Support.Adalog.Main_Support is

   ---------
   -- "+" --
   ---------

   function "+" (R : Relation) return Relation is
   begin
      Relations.Append (R);
      return R;
   end "+";

   -----------------------
   -- Release_Relations --
   -----------------------

   procedure Release_Relations is
   begin
      for R of Relations loop
         Dec_Ref (R);
      end loop;
      Relations := Relation_Vectors.Empty_Vector;
   end Release_Relations;

end Langkit_Support.Adalog.Main_Support;
