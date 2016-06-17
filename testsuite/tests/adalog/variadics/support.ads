with Adalog.Main_Support;      use Adalog.Main_Support;

package Support is
   type Dummy_Data is null record;
   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   package Bind is new Eq_Int.Raw_Custom_Bind (Dummy_Data, Transform);
end Support;
