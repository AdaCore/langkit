## vim: filetype=makoada

% if emitter.builtin_unparsing_overridings:
with GNATCOLL.VFS; use GNATCOLL.VFS;
% endif

--  This package provides contants to refer to the default unparsing
--  configuration and builtin configuration overridings.

package ${ada_lib_name}.Generic_API.Unparsing is

   Default_Configuration_Filename : constant String :=
     ${ascii_repr(emitter.default_unparsing_config_filename)};
   --  Filename to use to refer to the default unparsing configuration

   package Builtin_Overridings is
      % for name, filename in emitter.builtin_unparsing_overridings.items():
      function ${name} return Virtual_File
      is (Create (${ascii_repr(filename)}));
      ${format_ada_doc(
         None, ctx.config.library.builtin_unparsing_overridings[name].doc, 6
      )}
      % endfor
   end Builtin_Overridings;
   --  Filenames to use to refer to builtin configuration overridings

end ${ada_lib_name}.Generic_API.Unparsing;
