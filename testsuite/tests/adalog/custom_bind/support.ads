with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Main_Support;      use Adalog.Main_Support;

package Support is
   type Dummy_Data is null record;
   D : Dummy_Data;

   function Transform (D : Dummy_Data; I : Integer) return Integer is (I ** 2);
   package Bind is new Eq_Int.Raw_Custom_Bind (Dummy_Data, Transform);
   function Square (X, Y : Eq_Int.Refs.Raw_Var) return Relation
   is (Bind.Create (X, Y, D));
end Support;
