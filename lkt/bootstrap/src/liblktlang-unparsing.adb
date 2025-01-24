
with Liblktlang.Implementation;
use Liblktlang.Implementation;

with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;
with Liblktlang.Unparsing_Implementation;
use Liblktlang.Unparsing_Implementation;

package body Liblktlang.Unparsing is

   -------------
   -- Unparse --
   -------------

   function Unparse (Node : Lkt_Node'Class) return String is
      N : constant Bare_Lkt_Node := Unwrap_Node (Node);
   begin
      return Unparse
        (Create_Abstract_Node (N),
         Unwrap_Node (Node).Unit,
         Preserve_Formatting => False,
         As_Unit             => False);
   end Unparse;

end Liblktlang.Unparsing;
