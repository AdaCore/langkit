
with Liblktlang.Analysis; use Liblktlang.Analysis;

package Liblktlang.Unparsing is

   function Unparse (Node : Lkt_Node'Class) return String
      with Pre => not Node.Unit.Has_Diagnostics;
   --  Turn the Node tree into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.
   --
   --  Note that this requires that Node's unit has no parsing error.

end Liblktlang.Unparsing;
