with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Support is
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);

   function Transform (Dummy_D : Dummy_Data; I : Integer) return Integer
   is (I * 3);
   function Eq (Dummy_D : Dummy_Data; A, B : Integer) return Boolean
   is (A = B);

   package Bind is new Eq_Int.Raw_Custom_Bind
     (Converter   => Dummy_Data, No_Data        => No_Data,
      Equals_Data => Dummy_Data, No_Equals_Data => No_Data,
      Convert     => Transform,
      Equals      => Eq);

   package Unify renames Eq_Int.Raw_Impl;

   type Pred is null record;
   function Call (Dummy_P : Pred; X : Integer) return Boolean
   is ((X mod 2) = 0);

   function Image (Dummy_Self : Pred) return String is ("");

   package Pred_Int is
     new Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var, Pred);

end Support;
