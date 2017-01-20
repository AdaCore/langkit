with Adalog.Eq_Same;
with Adalog.Predicates;        use Adalog.Predicates;

package Adalog.Main_Support is

   function Element_Image (I : Integer) return String is (I'Image);
   package Eq_Int is new Eq_Same (Integer);

   package Pred_Int is
     new Dyn_Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var);
end Adalog.Main_Support;
