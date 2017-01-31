with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is
   function Eq (A, B : Integer) return Boolean is (A = B);
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);
   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   package Bind is new Eq_Int.Raw_Custom_Bind
     (Dummy_Data, No_Data, Transform, Eq);
end Support;
