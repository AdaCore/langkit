with Langkit_Support.Adalog.Eq_Same;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Langkit_Support.Adalog.Main_Support is

   function Element_Image (I : Integer) return String is (I'Image);
   package Eq_Int is new Eq_Same (Integer);

   package Pred_Int is
     new Dyn_Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var);
end Langkit_Support.Adalog.Main_Support;
