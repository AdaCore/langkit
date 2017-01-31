with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Support is
   function Eq (A, B : Integer) return Boolean is (A = B);
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);
   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   package Bind is new Eq_Int.Raw_Custom_Bind
     (Dummy_Data, No_Data, Transform, Eq);


   type Pred is null record;
   function Call (P : Pred; X : Integer) return Boolean is ((X mod 2) = 0);

   function Image (Self : Pred) return String is ("");

   package Pred_Int is
     new Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var, Pred);

end Support;
