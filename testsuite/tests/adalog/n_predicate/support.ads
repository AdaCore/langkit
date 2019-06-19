with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is

   use Int_Solver;

   type Transformer is new Int_Solver.Converter_Type with null record;
   function Convert (Self : Transformer; Val : Integer) return Integer
   is (Val * 3);
   function Image (Self : Transformer) return String is ("*3");

   type Pred is new Int_Solver.N_Predicate_Type with null record;
   function Call (Dummy : Pred; Vals : Int_Solver.Value_Array) return Boolean
   is (Vals (1) = Vals (2) * 2);

   function Image (Dummy : Pred) return String is ("Is_Double_Of");

end Support;

