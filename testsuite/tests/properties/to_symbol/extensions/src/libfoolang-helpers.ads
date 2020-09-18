with Langkit_Support.Text; use Langkit_Support.Text;
with Libfoolang.Common; use Libfoolang.Common;

package Libfoolang.Helpers is
   function Canonicalize_Symbol
     (Name : Text_Type) return Symbolization_Result;
end Libfoolang.Helpers;
