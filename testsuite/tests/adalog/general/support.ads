with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

package Support is
   use T_Solver;

   type Transformer is new Converter_Type with null record;
   function Convert (Dummy : Transformer; I : Integer) return Integer
   is (I * 3);
   function Image (Dummy : Transformer) return String is ("*3");

end Support;

