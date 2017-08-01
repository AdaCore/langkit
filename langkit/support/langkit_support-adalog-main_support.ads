--  Provide common support material for unit tests

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Langkit_Support.Adalog.Main_Support is

   function Element_Image (I : Integer) return String is (I'Image);
   package Eq_Int is new Eq_Same (Integer);
   package Pred_Int is new Dyn_Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var);

   procedure Free_Relation_Tree (R : in out Relation);
   --  Most of the time in testcases, we build relation trees in a single
   --  expression::
   --
   --    Create (Create (...), Create (...), ...).
   --
   --  As creation yields an ownership share for the result and create one
   --  ownership share per argument, all relation nodes except the root one get
   --  a ref-count of 2 at least. And so calling Dec_Ref (...) on the root
   --  relation is not enough to get everything free'd.
   --
   --  This procedure decrements the ref-count of all relations but the root
   --  one once and then calls Dec_Ref on the root one.

end Langkit_Support.Adalog.Main_Support;
