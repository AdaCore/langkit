## vim: filetype=makoada

<%namespace name="exts"    file="extensions.mako" />
<%namespace name="helpers" file="properties/helpers.mako" />

pragma Warnings (Off, "referenced");
pragma Warnings (Off, "use clause for package * has no effect");
${exts.with_clauses(with_clauses)}
pragma Warnings (On, "referenced");
pragma Warnings (On, "use clause for package * has no effect");

private package ${ada_lib_name}.All_Properties is

   % for n in ctx.node_types:
      % for prop in n.get_properties(include_inherited=False):
         function ${prop.names.codegen}
           ${helpers.argument_list(prop, prop.dispatching)}
            return ${prop.type.name}
         renames ${prop.impl_package.qual_name}.${prop.names.codegen};
      % endfor
   % endfor

end ${ada_lib_name}.All_Properties;
