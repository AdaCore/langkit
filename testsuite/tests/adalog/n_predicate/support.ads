with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;

package Support is
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);

   function Transform (Dummy : Dummy_Data; I : Integer) return Integer is
      (I * 3);
   function Eq (Dummy : Dummy_Data; A, B : Integer) return Boolean is (A = B);

   package Bind is new Eq_Int.Raw_Custom_Bind
     (Converter   => Dummy_Data, No_Data        => No_Data,
      Equals_Data => Dummy_Data, No_Equals_Data => No_Data,
      Convert     => Transform,
      Equals      => Eq);

   type Pred is null record;
   function Call (Dummy : Pred; L, R : Integer) return Boolean is (L = R * 2);

   function Image (Dummy : Pred) return String is ("");

   package Pred_2 is new Predicate_2
     (Integer, Eq_Int.Refs.Raw_Logic_Var, Pred, Call);

end Support;
