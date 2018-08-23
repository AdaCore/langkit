## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

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

   for Languages use ("Ada", "C");
   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;
   for Interfaces use
     ("${lib_name.lower()}.ads",
      "${lib_name.lower()}-analysis.adb",
      "${lib_name.lower()}-analysis.ads",
      "${lib_name.lower()}-c.ads",
      "${lib_name.lower()}-c.adb",
      "${lib_name.lower()}-common.ads",
      "${lib_name.lower()}-common.adb",
      "${lib_name.lower()}-converters.ads",
      "${lib_name.lower()}-debug.adb",
      "${lib_name.lower()}-debug.ads",
      "${lib_name.lower()}-implementation.adb",
      "${lib_name.lower()}-implementation.ads",
      "${lib_name.lower()}-implementation-c.adb",
      "${lib_name.lower()}-implementation-c.ads",
      "${lib_name.lower()}-init.adb",
      "${lib_name.lower()}-init.ads",
      "${lib_name.lower()}-introspection.ads",
      "${lib_name.lower()}-iterators.adb",
      "${lib_name.lower()}-iterators.ads",
      "${lib_name.lower()}-lexer.adb",
      "${lib_name.lower()}-lexer.ads",
      "${lib_name.lower()}-parsers.adb",
      "${lib_name.lower()}-parsers.ads",
      "${lib_name.lower()}-rewriting.adb",
      "${lib_name.lower()}-rewriting.ads",
      "${lib_name.lower()}-unparsing.adb",
      "${lib_name.lower()}-unparsing.ads",
      "${lib_name.lower()}-unparsing_implementation.adb",
      "${lib_name.lower()}-unparsing_implementation.ads",
      % for path in sorted(os_path.basename(p) \
                           for p in ctx.additional_source_files):
      "${path}",
      % endfor
      % if ctx.generate_gdb_hook:
      "gdb.c",
      % endif
      "quex_interface.c",
      "quex_interface.h",
      "quex_lexer.c",
      "quex_lexer-configuration.h",
      "quex_lexer.h",
      "quex_lexer-token.h",
      "quex_lexer-token_ids.h");

   <% source_dirs = ['../../include/{}'.format(lib_name.lower()),
                     ctx.extensions_src_dir] %>
   for Source_Dirs use
     (${', '.join(string_repr(d) for d in source_dirs if d)});

   for Library_Dir use "../${lib_name.lower()}." & Library_Kind_Param;
   for Object_Dir use "../../obj/${lib_name.lower()}." & Library_Kind_Param;

   Target := ${lib_name}'Target;

   package Compiler is

      --  If asked to, enable all warnings and treat them as errors, except:
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

      Common_Ada_Cargs := ();
      case Enable_Warnings is
         when "true" => Common_Ada_Cargs := ("-gnatwaCKMR", "-gnatyacknprt");
         when others => null;
      end case;

      Common_C_Cargs :=
        ("-I${quex_path}",
         "-DQUEX_OPTION_ASSERTS_DISABLED",
         "-DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED");

      case Build_Mode is
         when "dev" =>

            for Default_Switches ("Ada") use
               Common_Ada_Cargs & ("-g", "-O0", "-gnatwe", "-gnata");

            for Default_Switches ("C") use
               Common_C_Cargs & ("-g3", "-O0", "-DDEBUG=1");

            for Switches ("quex_lexer.c") use Common_C_Cargs & ("-g0", "-O0");
            --  This file is *huge* and the debugging information for it harms
            --  Valgrind runs. We almost never have to debug this file so
            --  this is acceptable.

         when "prod" =>
            --  Debug information is useful even with optimization for
            --  profiling, for instance.
            for Default_Switches ("Ada") use
               Common_Ada_Cargs & ("-g", "-Ofast", "-gnatp", "-gnatn2");

            for Default_Switches ("C") use Common_C_Cargs & ("-Ofast");

            for Switches ("quex_lexer.c") use Common_C_Cargs
              & ("-O1", "-fno-ree", "-fdisable-rtl-cprop_hardreg",
                 "-fdisable-rtl-sched2", "-mno-stv");
            --  Deactivate because of memory usage, see P726-024. This
            --  limits the memory usage peaks of GCC 6 based compilers
            --  and should prevent OOM on 32-bit platforms.

            ## TODO: This extension point is added to change the flags of
            ## Libadalang specific extension files. It is a temporary
            ## workaround, waiting for QC05-038 to be fixed.
            ${exts.include_extension(ctx.ext("prod_additional_flags"))}

      end case;
   end Compiler;

end ${lib_name};
