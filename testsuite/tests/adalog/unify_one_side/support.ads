with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is
   use Int_Solver;

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

   function Is_Even
     (Var : Refs.Raw_Var) return Relation
   is (Predicate (Var, Langkit_Support.Adalog.Main_Support.Is_Even));

end Support;
