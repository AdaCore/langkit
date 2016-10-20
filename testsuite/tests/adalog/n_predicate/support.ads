with Adalog.Main_Support; use Adalog.Main_Support;
with Adalog.Predicates;   use Adalog.Predicates;

package Support is
   type Dummy_Data is null record;
   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   package Bind is new Eq_Int.Raw_Custom_Bind (Dummy_Data, Transform);

   type Pred is null record;
   function Call (Self : Pred; L, R : Integer) return Boolean is (L = R * 2);

   function Image (Self : Pred) return String is ("");

   package Pred_2 is new Predicate_2
     (Integer, Eq_Int.Refs.Raw_Logic_Var, Pred, Call);

end Support;
