## vim: filetype=makoada

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

package ${ada_lib_name}.Unparsing.Implementation is

   function Unparse (Node : ${root_node_type_name}) return String;
   --  Turn the Node tree into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.

end ${ada_lib_name}.Unparsing.Implementation;
