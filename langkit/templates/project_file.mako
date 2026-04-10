## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%def name="format_str_set(strings, indent)">${ada_block_with_parens(
   [
      ## GPR tools expect UTF-8 project files (i.e. the default encoding for
      ## rendered template in Langkit), and we cannot use bytes_repr to format
      ## the corresponding string literals, as bytes_repr assumes Ada
      ## (``Character'Val (1)`` is invalid in the GPR world).
      '"{}"'.format(s.replace('"', '""')) for s in sorted(strings)
   ],
   indent
)}</%def>

with "gnatcoll_core";
with "gnatcoll_gmp";
with "gnatcoll_iconv";
with "prettier_ada";

% if not cfg.library.standalone:
   with "langkit_support";
% endif

${exts.include_extension(ctx.ext('withed_projects'))}

<%
   extra_source_files = sorted(os_path.basename(p)
                               for p in ctx.additional_source_files)

   source_dirs = ["src", "src/builtin_files", emitter.extensions_src_dir]

   upper_lib_name = lib_name.upper()
%>

library project ${lib_name} is

   type Build_Mode_Type is ("dev", "prod", "prof");
   Build_Mode : Build_Mode_Type :=
     external ("${upper_lib_name}_BUILD_MODE", external ("BUILD_MODE", "dev"));

   type Library_Kind_Type is ("static", "relocatable", "static-pic");
   Library_Kind_Param : Library_Kind_Type := external
     ("${upper_lib_name}_LIBRARY_TYPE", external ("LIBRARY_TYPE", "static"));

   type Library_Standalone_Type is ("standard", "no", "encapsulated");
   Library_Standalone : Library_Standalone_Type := external
     ("${upper_lib_name}_STANDALONE", external ("STANDALONE", "standard"));

   type Boolean is ("false", "true");

   Externally_Built : Boolean :=
     external ("${upper_lib_name}_EXTERNALLY_BUILT", "false");
   for Externally_Built use Externally_Built;

   Enable_Warnings : Boolean :=
     external ("${lib_name.upper()}_WARNINGS", "false");

   for Source_Dirs use
   ${format_str_set(
      [emitter.path_to(d, project_path) for d in source_dirs if d], 3
   )};

   for Languages use
   ${format_str_set(emitter.project_languages, 3)};

   Interfaces :=
   ${format_str_set(emitter.library_interfaces, 3)};
   case Library_Standalone is
      when "no" =>
         null;
      when "standard" | "encapsulated" =>
         for Library_Standalone use Library_Standalone;
         for Interfaces use Interfaces;
   end case;

   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;
   for Library_Dir use "lib/" & Library_Kind_Param & "/" & Build_Mode;
   for Object_Dir use "obj/" & Build_Mode;

   Target := ${lib_name}'Target;

   package Compiler is

      for Local_Configuration_Pragmas use "gnat.adc";

      For_All_Cargs := ("-fPIC");

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

         when "prof" =>
            --  Ensure that we have a relatively fast build but with all
            --  possible stack info & debug info, for profiling.
            Mode_Args := ("-Og", "-ggdb", "-g3", "-fno-omit-frame-pointer");

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

         when "prof" =>
            null;

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

         when "prof" =>
            null;

         when "prod" =>
            null;
      end case;

      Common_Ada_Cargs :=
        ("-gnatX") & For_All_Cargs & Mode_Args & Ada_Mode_Args;
      Common_C_Cargs := For_All_Cargs & Mode_Args & C_Mode_Args;

      for Default_Switches ("Ada") use Common_Ada_Cargs & Generated_Ada_Cargs;
      for Default_Switches ("C") use Common_C_Cargs;

      % for f in extra_source_files:
      for Switches ("${f}") use Common_Ada_Cargs & Manual_Ada_Cargs;
      % endfor

      case Build_Mode is
         when "dev" =>
            null;

         when "prof" =>
            null;

         when "prod" =>
            ## TODO: This extension point is added to change the flags of
            ## Libadalang specific extension files. It is a temporary
            ## workaround, waiting for QC05-038 to be fixed.
            ${exts.include_extension(ctx.ext("prod_additional_flags"))}
      end case;
   end Compiler;

   package Binder is
      case Build_Mode is
         when "dev" =>
            for Switches ("Ada") use ("-Es");
         when "prod" | "prof" =>
            for Switches ("Ada") use ("-E");
      end case;
   end Binder;

   package Coverage is
      for Units use
      ${format_str_set(emitter.coverage_units, 6)};
   end Coverage;

end ${lib_name};
