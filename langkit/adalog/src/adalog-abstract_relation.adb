package body Adalog.Abstract_Relation is

   -----------
   -- Reset --
   -----------

   pragma Warnings (Off, "parameter");
   procedure Reset (Self : in out Relation)
   is
   begin
      Reset (Self.all);
   end Reset;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Relation) is
   begin
      Free (Self.all);
   end Free;

end Adalog.Abstract_Relation;
