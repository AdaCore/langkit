## vim: filetype=makoada

<%namespace name="env_specs"        file="env_specs_ada.mako" />
<%namespace name="exts"             file="extensions.mako" />
<%namespace name="logic_functors"   file="logic_functors_ada.mako" />
<%namespace name="logic_predicates" file="logic_predicates_ada.mako" />

--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

pragma Warnings (Off, "no entities of package * are referenced");
pragma Warnings (Off, "use clause for package * has no effect");
${exts.with_clauses(with_clauses)}
pragma Warnings (On, "no entities of package * are referenced");
pragma Warnings (On, "use clause for package * has no effect");

package body ${ada_lib_name}.${impl_pkg.name} is

   pragma Warnings (Off, "no entities of package * are referenced");
   pragma Warnings (Off, "use clause for package * has no effect");
   use ${ada_lib_name}.Implementation.Precomputed_Symbols;
   use ${ada_lib_name}.Implementation.Solver;
   pragma Warnings (On, "no entities of package * are referenced");
   pragma Warnings (On, "use clause for package * has no effect");

   % for prop in impl_pkg.properties:
      ${prop.prop_def}

      % if prop.requires_untyped_wrapper:
         ${prop.untyped_wrapper_def}
      % endif

      % for pred in prop.logic_predicates:
         ${logic_predicates.body(pred)}
      % endfor

      % for functor in prop.logic_functors:
         ${logic_functors.body(functor)}
      % endfor
   % endfor

   % for env_spec in impl_pkg.env_specs:
      ${env_specs.body(env_spec)}
   % endfor

end ${ada_lib_name}.${impl_pkg.name};
