with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is
   use T_Solver, Solver_Ifc;

   ------------
   -- Square --
   ------------

   type Squarer is new Converter_Type with null record;

   overriding function Convert (Dummy : Squarer; I : Integer) return Integer
   is (I ** 2);

   overriding function Image (Dummy : Squarer) return String is ("Square");

   Squarer_Singleton : constant Squarer :=
     (Cache_Set => False, Ref_Count => 1, others => <>);

   function Square (X, Y : Refs.Logic_Var) return Relation
   is (Propagate (X, Y, Conv => Squarer_Singleton));

   function Square (X : Refs.Logic_Var; Y : Integer) return Relation
   is (Assign (X, Y, Conv => Squarer_Singleton));

   function Square (X : Integer; Y : Refs.Logic_Var) return Relation
   is (Square (Y, X));

   function Is_Even (V : Integer) return Boolean is (V mod 2 = 0);

   function Is_Even
     (Var : Refs.Logic_Var) return Relation
   is (Predicate (Var, Predicate (Is_Even'Access, "Is_Even")));

end Support;
