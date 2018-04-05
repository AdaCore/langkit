## vim: filetype=makoada

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

with ${ada_lib_name}.Unparsing.Implementation;
use ${ada_lib_name}.Unparsing.Implementation;

% if ctx.separate_properties:
   with ${ada_lib_name}.Analysis.Properties;
   use ${ada_lib_name}.Analysis.Properties;
% endif

package body ${ada_lib_name}.Unparsing is

   -------------
   -- Unparse --
   -------------

   function Unparse (Node : ${root_entity.api_name}'Class) return String is
      N : constant ${root_node_type_name} := Bare_Node (Node);
   begin
      return Unparse
        (Analysis.Implementation.Abstract_Node (N),
         Unit (Node),
         Preserve_Formatting => False);
   end Unparse;

end ${ada_lib_name}.Unparsing;
