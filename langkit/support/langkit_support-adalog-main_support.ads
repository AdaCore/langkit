--  Provide common support material for unit tests

with Ada.Containers.Vectors;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Langkit_Support.Adalog.Main_Support is

   function Element_Image (I : Integer) return String is (I'Image);
   package Eq_Int is new Eq_Same (Integer);
   package Pred_Int is new Dyn_Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var);

   function "+" (R : Relation) return Relation;
   --  Register R and return it. This is used to keep track of allocated
   --  relations in testcases.

   procedure Release_Relations;
   --  Decrement the ref-count of all relations registered with "+"

private

   package Relation_Vectors is new Ada.Containers.Vectors
     (Positive, Relation);
   Relations : Relation_Vectors.Vector;

end Langkit_Support.Adalog.Main_Support;
