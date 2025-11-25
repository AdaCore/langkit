## vim: filetype=makoada

<%namespace name="env_specs"        file="env_specs_ada.mako" />
<%namespace name="exts"             file="extensions.mako" />
<%namespace name="logic_functors"   file="logic_functors_ada.mako" />
<%namespace name="logic_predicates" file="logic_predicates_ada.mako" />

--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

pragma Warnings (Off, "referenced");
pragma Warnings (Off, "use clause for package * has no effect");
${exts.with_clauses(with_clauses)}
pragma Warnings (On, "referenced");
pragma Warnings (On, "use clause for package * has no effect");

private package ${ada_lib_name}.${impl_pkg.name} is

   % for prop in impl_pkg.properties:
      ${prop.prop_decl}

      % if prop.requires_untyped_wrapper:
         ${prop.untyped_wrapper_decl}
      % endif

      % for pred in prop.logic_predicates:
         ${logic_predicates.decl(pred)}
      % endfor

      % for functor in prop.logic_functors:
         ${logic_functors.decl(functor)}
      % endfor
   % endfor

   % for env_spec in impl_pkg.env_specs:
      ${env_specs.decl(env_spec)}
   % endfor

end ${ada_lib_name}.${impl_pkg.name};
