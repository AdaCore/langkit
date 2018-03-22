package body Langkit_Support.Adalog.Pure_Relations is

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out False_Relation_Rec) return Solving_State is
      pragma Unreferenced (Self);
   begin
      return Unsatisfied;
   end Apply;

   ------------------
   -- Custom_Image --
   ------------------

   function Custom_Image (Self : False_Relation_Rec) return String is
      pragma Unreferenced (Self);
   begin
      return "<False>";
   end Custom_Image;

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out True_Relation_Rec) return Solving_State is
      pragma Unreferenced (Self);
   begin
      return Satisfied;
   end Apply;

   ------------------
   -- Custom_Image --
   ------------------

   function Custom_Image (Self : True_Relation_Rec) return String is
      pragma Unreferenced (Self);
   begin
      return "<True>";
   end Custom_Image;

end Langkit_Support.Adalog.Pure_Relations;
