with Langkit_Support.Adalog.Main_Support;

package Support is
   use Langkit_Support.Adalog.Main_Support.Solver_Ifc;

   type Transformer is new Converter_Type with null record;

   overriding function Convert
     (Dummy : Transformer; I : Integer) return Integer is (I * 3);
   overriding function Image (Dummy : Transformer) return String is ("*3");

   Transformer_Singleton : constant Transformer :=
     (Cache_Set => False, Ref_Count => 1, others => <>);

end Support;
