with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is
   type Dummy_Data is null record;
   No_Data : constant Dummy_Data := (null record);

   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   function Eq (D : Dummy_Data; A, B : Integer) return Boolean is (A = B);

   package Bind is new Eq_Int.Raw_Custom_Bind
     (Converter   => Dummy_Data, No_Data        => No_Data,
      Equals_Data => Dummy_Data, No_Equals_Data => No_Data,
      Convert     => Transform,
      Equals      => Eq);
end Support;
