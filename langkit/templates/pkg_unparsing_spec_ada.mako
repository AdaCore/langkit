## vim: filetype=makoada

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;

package ${ada_lib_name}.Unparsing is

   function Unparse (Node : ${root_entity.api_name}'Class) return String;
   --  Turn the Node AST into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.

end ${ada_lib_name}.Unparsing;
