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
${exts.include_extension(ctx.ext('withed_projects'))}

% if emitter.coverage:
   with "gnatcov_rts_full";
% endif

<%
   extra_source_files = sorted(os_path.basename(p)
                               for p in ctx.additional_source_files)

   source_dirs = ["src", emitter.extensions_src_dir]
%>

library project ${lib_name} is

   type Build_Mode_Type is ("dev", "prod");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   type Library_Kind_Type is ("static", "relocatable", "static-pic");
   Library_Kind_Param : Library_Kind_Type := external
     ("LIBRARY_TYPE", external ("LIBADALANG_LIBRARY_TYPE", "static"));

   type Boolean is ("false", "true");

   ## Disable style checks on instrumented code
   % if emitter.coverage:
      Enable_Warnings : Boolean := "false";
   % else:
      Enable_Warnings : Boolean :=
        external ("${lib_name.upper()}_WARNINGS", "false");
   % endif

   ## We rely a lot on project installation (gprinstall), and gnatcov does not
   ## handle that well, so we need kludges to compute code coverage using
   ## gnatcov without completely reworking how Langkit-generated libraries are
   ## built and tested.
   ##
   ## Generating a library for which we want to compute code coverage happens
   ## in four steps:
   ##
   ## 1. Langkit generates library sources in the regular location
   ##    (../../include/$/).
   ##
   ## 2. We run "gnatcov instrument" with the $_COVINSTR scenario
   ##    variable set to true. "gnatcov instrument" generates instrumented
   ##    sources in the "$-gnatcov-instr" subdirectory in the object directory.
   ##
   ## 3. Langkit moves these instrumented sources to the
   ##    ../../obj/$/$-gnatcov-instr directory so that the path of instrumented
   ##    sources does not depend on scenario variables.
   ##
   ## 4. gprinstall and all other uses of the not-yet-installed project leave
   ##    the $_COVINSTR scenario variable to "false" (its default value) so
   ##    that the source directory is ../../obj/$/$-gnatcov-instr (i.e. points
   ##    to instrumented sources).
   ##
   ## This organization is indeed involved, but given gnatcov's requirements,
   ## no simple setup is possible, and this reduces the number of places that
   ## have to worry about these things to this Mako template and to the
   ## langkit.coverage module.
   Primary_Source_Dirs :=
     (${', '.join(ascii_repr(emitter.path_to(d, project_path))
                  for d in source_dirs if d)});

   % if emitter.coverage:
      Secondary_Source_Dirs :=
        ("obj/${lib_name.lower()}-gnatcov-instr");
      For_Coverage_Instrumentation : Boolean := external
        ("${lib_name.upper()}_COVINSTR", "false");
      case For_Coverage_Instrumentation is
         when "false" => for Source_Dirs use Secondary_Source_Dirs;
         when "true" =>  for Source_Dirs use Primary_Source_Dirs;
      end case;
   % else:
      for Source_Dirs use Primary_Source_Dirs;
   % endif

   for Languages use ${format_str_set(emitter.project_languages)};
   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;

   % if emitter.coverage:
      --  Before "gnatcov instrument" produced instrumented sources, not all
      --  interfaces units are present, so do not define interfaces at this
      --  stage. "gnatcov instrument" does not need to know interfaces in order
      --  to work, anyway.
      case For_Coverage_Instrumentation is
         when "false" =>
            for Interfaces use ${format_str_set(emitter.library_interfaces)};
         when "true" =>
      end case;
   % else:
      for Interfaces use ${format_str_set(emitter.library_interfaces)};
   % endif

   for Library_Dir use "lib/" & Library_Kind_Param & "/" & Build_Mode;
   for Object_Dir use "obj/" & Build_Mode;

   Target := ${lib_name}'Target;

   package Compiler is

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

      Common_Ada_Cargs := For_All_Cargs & Mode_Args & Ada_Mode_Args;
      Common_C_Cargs := For_All_Cargs & Mode_Args & C_Mode_Args;

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
