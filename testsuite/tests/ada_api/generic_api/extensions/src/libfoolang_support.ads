with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

package Libfoolang_Support is
   My_Exception_1 : exception;
   My_Exception_2 : exception;

   function Canonicalize (T : Text_Type) return Symbolization_Result;
end Libfoolang_Support;
