## vim: filetype=makoada

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

with ${ada_lib_name}.Unparsing.Implementation;
use ${ada_lib_name}.Unparsing.Implementation;

package body ${ada_lib_name}.Unparsing is

   -------------
   -- Unparse --
   -------------

   function Unparse (Node : ${root_entity.api_name}'Class) return String is
   begin
      return Unparse (Bare_Node (Node));
   end Unparse;

end ${ada_lib_name}.Unparsing;
