with Langkit_Support.Adalog.Main_Support;

package Support is

   use Langkit_Support.Adalog.Main_Support.Solver_Ifc;
   use Langkit_Support.Adalog.Main_Support.Refs;

   type Pred is new N_Predicate_Type with null record;

   overriding function Call (Dummy : Pred; Vals : Value_Array) return Boolean
   is (Vals (1) = Vals (2) * 2);

   overriding function Image (Dummy : Pred) return String is ("Is_Double_Of");

   Pred_Singleton : constant Pred :=
     (N => 2, Cache_Set => False, Ref_Count => 1, others => <>);

end Support;
