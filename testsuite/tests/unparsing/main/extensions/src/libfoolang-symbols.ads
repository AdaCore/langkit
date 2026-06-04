with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

private package Libfoolang.Symbols is
   function Canonicalize (T : Text_Type) return Symbolization_Result;
end Libfoolang.Symbols;
