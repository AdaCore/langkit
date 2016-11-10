with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Main_Support;      use Adalog.Main_Support;

package Support is
   function Eq (A, B : Integer) return Boolean is (A = B);
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);

   function Transform (D : Dummy_Data; I : Integer) return Integer is (I ** 2);
   package Bind is new Eq_Int.Raw_Custom_Bind
     (Dummy_Data, No_Data, Transform, Eq);
   function Square (X, Y : Eq_Int.Refs.Raw_Var) return Relation
   is (Bind.Create (X, Y, No_Data));
end Support;
