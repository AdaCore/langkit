with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Relations;
use Langkit_Support.Adalog.Relations;

package Langkit_Support.Adalog.Pure_Relations is

   --------------------
   -- False_Relation --
   --------------------

   type False_Relation_Rec is null record;
   function Apply (Inst : in out False_Relation_Rec) return Boolean is (False);
   procedure Free (Inst : in out False_Relation_Rec) is null;
   package False_Relation is new Pure_Relation (Ty => False_Relation_Rec);

   function False_Rel return Relation
   is (new False_Relation.Rel'(others => <>));

   -------------------
   -- True_Relation --
   -------------------

   type True_Relation_Rec is null record;
   function Apply (Inst : in out True_Relation_Rec) return Boolean is (True);
   procedure Free (Inst : in out True_Relation_Rec) is null;
   package True_Relation is new Pure_Relation (Ty => True_Relation_Rec);

   function True_Rel return Relation is (new True_Relation.Rel'(others => <>));

   ----------------------
   -- Boolean_Relation --
   ----------------------

   type Boolean_Relation_Rec is record
      Result : Boolean;
   end record;

   function Apply (Inst : in out Boolean_Relation_Rec) return Boolean
   is (Inst.Result);

   procedure Free (Inst : in out Boolean_Relation_Rec) is null;

   package Boolean_Relation is
     new Pure_Relation (Ty => Boolean_Relation_Rec);

end Langkit_Support.Adalog.Pure_Relations;
