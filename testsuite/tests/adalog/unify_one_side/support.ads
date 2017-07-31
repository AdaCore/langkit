with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Support is

   ------------
   -- Square --
   ------------

   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);

   function Transform (D : Dummy_Data; I : Integer) return Integer is (I ** 2);
   function Eq (D : Dummy_Data; A, B : Integer) return Boolean is (A = B);

   package Bind is new Eq_Int.Raw_Custom_Bind
     (Converter   => Dummy_Data, No_Data        => No_Data,
      Equals_Data => Dummy_Data, No_Equals_Data => No_Data,
      Convert     => Transform,
      Equals      => Eq);

   function Square (X, Y : Eq_Int.Refs.Raw_Var) return Relation
   is (Bind.Create (X, Y, No_Data, No_Data));

   function Square (X : Eq_Int.Refs.Raw_Var; Y : Integer) return Relation
   is (Bind.Create (X, Y, No_Data, No_Data));

   function Square (X : Integer; Y : Eq_Int.Refs.Raw_Var) return Relation
   is (Bind.Create (X, Y, No_Data, No_Data));

   -------------
   -- Is_Even --
   -------------

   type Is_Even_Pred_Type is null record;
   Is_Even_Pred : constant Is_Even_Pred_Type := (null record);

   function Call (Self : Is_Even_Pred_Type; L : Integer) return Boolean is
     (L mod 2 = 0);
   function Image (Self : Is_Even_Pred_Type) return String is ("is-even?");

   package Is_Even_Predicate is new Predicate
     (El_Type        => Integer,
      Var            => Eq_Int.Refs.Raw_Logic_Var,
      Predicate_Type => Is_Even_Pred_Type,
      Call           => Call,
      Image          => Image);

   function Is_Even
     (Var : Eq_Int.Refs.Raw_Var) return access Base_Relation'Class
   is (Is_Even_Predicate.Create (Var, Is_Even_Pred));

end Support;
