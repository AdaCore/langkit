## vim: filetype=makoada

<%namespace name="entities" file="entities_ada.mako" />

% if ctx.separate_properties:
   with Ada.Containers.Vectors;
   with Ada.Unchecked_Conversion;

   with Langkit_Support.Array_Utils;

   with ${ada_lib_name}.Analysis.Implementation;
   use ${ada_lib_name}.Analysis.Implementation;
% endif

package body ${ada_lib_name}.Analysis.Properties is

   procedure Foo is null;

   % if ctx.separate_properties:
      use AST_Envs;

      ${entities.bodies()}
   % endif

end ${ada_lib_name}.Analysis.Properties;
