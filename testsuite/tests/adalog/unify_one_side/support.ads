with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is
   use T_Solver;

   ------------
   -- Square --
   ------------

   type Squarer is new Converter_Type with null record;

   function Convert (Dummy : Squarer; I : Integer) return Integer
   is (I ** 2);

   function Image (Dummy : Squarer) return String is ("Square");

   function Square (X, Y : Refs.Raw_Var) return Relation
   is (Propagate (X, Y, Conv => Squarer'(null record)));

   function Square (X : Refs.Raw_Var; Y : Integer) return Relation
   is (Assign (X, Y, Conv => Squarer'(null record)));

   function Square (X : Integer; Y : Refs.Raw_Var) return Relation
   is (Square (Y, X));

   function Is_Even (V : Integer) return Boolean is (V mod 2 = 0);

   function Is_Even
     (Var : Refs.Raw_Var) return Relation
   is (Predicate (Var, Predicate (Is_Even'Access, "Is_Even")));

end Support;
