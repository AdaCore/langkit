with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

package Libfoolang.Helpers is
   function Canonicalize_Symbol
     (Name : Text_Type) return Symbolization_Result;
end Libfoolang.Helpers;
