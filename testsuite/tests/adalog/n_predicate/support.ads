with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;

package Support is
   function Eq (A, B : Integer) return Boolean is (A = B);
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);
   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   package Bind is new Eq_Int.Raw_Custom_Bind
     (Dummy_Data, No_Data, Transform, Eq);

   type Pred is null record;
   function Call (Self : Pred; L, R : Integer) return Boolean is (L = R * 2);

   function Image (Self : Pred) return String is ("");

   package Pred_2 is new Predicate_2
     (Integer, Eq_Int.Refs.Raw_Logic_Var, Pred, Call);

end Support;
