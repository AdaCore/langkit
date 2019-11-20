with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Support is

   type Is_Even_Pred_Type is null record;
   Is_Even_Pred : constant Is_Even_Pred_Type := (null record);

   function Call (Dummy_Self : Is_Even_Pred_Type; L : Integer) return Boolean
   is (L mod 2 = 0);
   function Image (Dummy_Self : Is_Even_Pred_Type) return String
   is ("is-even?");

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
