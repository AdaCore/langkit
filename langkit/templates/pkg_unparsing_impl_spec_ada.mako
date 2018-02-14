## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

package ${ada_lib_name}.Unparsing.Implementation is

   function Unparse
     (Node : access Abstract_Node_Type'Class;
      Unit : Analysis_Unit)
      return String;
   --  Turn the Node tree into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.

   function Unparse
     (Node : access Abstract_Node_Type'Class;
      Unit : Analysis_Unit)
      return String_Access;
   --  Likewise, but return a string access. Callers must deallocate the result
   --  when done with it.

   function Unparse (Node : access Abstract_Node_Type'Class) return Text_Type;
   --  Likewise, but return a text access. Callers must deallocate the result
   --  when done with it.

end ${ada_lib_name}.Unparsing.Implementation;
