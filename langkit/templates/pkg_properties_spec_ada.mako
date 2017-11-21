## vim: filetype=makoada

<%namespace name="entities" file="entities_ada.mako" />

package ${ada_lib_name}.Analysis.Properties is

   procedure Foo;
   --  Dummy procedure so that this package requires a body no matter what is
   --  generated otherwise.

   % if ctx.separate_properties:
      ${entities.decls1()}
      ${entities.decls2()}
      ${entities.decls3()}
   % endif

end ${ada_lib_name}.Analysis.Properties;
