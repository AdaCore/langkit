## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%def name="format_str_set(strings)">
   ${'({})'.format(', '.join('"{}"'.format(s)
                             for s in sorted(strings)))}
</%def>

with "gnatcoll";
with "gnatcoll_gmp";
with "gnatcoll_iconv";
with "langkit_support";

library project ${lib_name} is

   type Build_Mode_Type is ("dev", "prod");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   type Library_Kind_Type is ("static", "relocatable", "static-pic");
   Library_Kind_Param : Library_Kind_Type := external
     ("LIBRARY_TYPE", external ("LIBADALANG_LIBRARY_TYPE", "static"));

   type Boolean is ("false", "true");
   Enable_Warnings : Boolean :=
     external ("${lib_name.upper()}_WARNINGS", "false");

   <% extra_source_files = sorted(
         os_path.basename(p) for p in ctx.additional_source_files) %>

   for Languages use ${format_str_set(emitter.project_languages)};
   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;
   for Interfaces use ${format_str_set(emitter.library_interfaces)};

   <% source_dirs = ['../../include/{}'.format(lib_name.lower()),
                     emitter.extensions_src_dir] %>
   for Source_Dirs use
     (${', '.join(string_repr(d) for d in source_dirs if d)});

   for Library_Dir use
      "../${lib_name.lower()}/" & Library_Kind_Param & "/" & Build_Mode;
   for Object_Dir use
      "../../obj/${lib_name.lower()}/" & Library_Kind_Param & "/" & Build_Mode;

   Target := ${lib_name}'Target;

   package Compiler is

      ----------------------
      -- Common_Ada_Cargs --
      ----------------------

      --  Compilation switches to use for Ada that do not depend on the build
      --  mode.
      --
      --  For generated sources, enable all warnings and treat them as errors,
      --  except:
      --    * conditional expressions used in tests that are known to be True
      --      or False at compile time (C), as this is very common in generated
      --      properties code;
      --    * variables that could be turned into constants (K), as this is
      --      very common in generated parsers code;
      --    * variables that are assigned and never read (M), as this is also
      --      common in generated parsers code;
      --    * redundant constructs (R), as we do have redundant conversions for
      --      AST nodes (A'Class and B'Class are not compatible even though B
      --      derives from A).
      --
      --  As for style diagnostics:
      --    * check the casing of attributes (a);
      --    * check double spaces before comments (c);
      --    * check the casing of keywords (k);
      --    * check the casing of entities in Standard (n);
      --    * check the casing of pragmas (p);
      --    * check the casing of references is the same as the casing of the
      --      corresponding defining identifiers (r);
      --    * check the token spacing (t).

      Manual_Ada_Cargs := ();
      Generated_Ada_Cargs := ();
      case Enable_Warnings is
         when "true" =>
            Manual_Ada_Cargs := ("-gnatwa", "-gnatyg");
            Generated_Ada_Cargs := ("-gnatwaCKMR", "-gnatyacknprt");
         when others =>
            null;
      end case;

      ---------------
      -- Mode_Args --
      ---------------

      --  Compilation switches for all languages that depend on the build mode

      Mode_Args := ();
      case Build_Mode is
         when "dev" =>
            Mode_Args := ("-g", "-O0");

         when "prod" =>
            --  Debug information is useful even with optimization for
            --  profiling, for instance.
            Mode_Args := ("-g", "-Ofast");
      end case;

      -------------------
      -- Ada_Mode_Args --
      -------------------

      --  Compilation switches for Ada that depend on the build mode

      Ada_Mode_Args := ();
      case Build_Mode is
         when "dev" =>
            Ada_Mode_Args := ("-gnatwe", "-gnata");

         when "prod" =>
            --  Enable aggressive inlining
            Ada_Mode_Args := ("-gnatn2");
      end case;

      -----------------
      -- C_Mode_Args --
      -----------------

      --  Compilation switches for C that depend on the build mode

      C_Mode_Args := ();
      case Build_Mode is
         when "dev" =>
            C_Mode_Args := ("-DDEBUG=1");

         when "prod" =>
            null;
      end case;

      Common_Ada_Cargs := Mode_Args & Ada_Mode_Args;
      Common_C_Cargs := Mode_Args & C_Mode_Args;

      for Default_Switches ("Ada") use Common_Ada_Cargs & Generated_Ada_Cargs;
      for Default_Switches ("C") use Common_C_Cargs;

      % for f in extra_source_files:
      for Switches ("${f}") use Common_Ada_Cargs & Manual_Ada_Cargs;
      % endfor

      case Build_Mode is
         when "dev" =>
            null;

         when "prod" =>
            ## TODO: This extension point is added to change the flags of
            ## Libadalang specific extension files. It is a temporary
            ## workaround, waiting for QC05-038 to be fixed.
            ${exts.include_extension(ctx.ext("prod_additional_flags"))}
      end case;
   end Compiler;

end ${lib_name};
