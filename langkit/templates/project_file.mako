## vim: filetype=makoada

with "gnatcoll";
with "gnatcoll_iconv";
with "langkit_support";

library project ${lib_name} is

   type Build_Mode_Type is ("dev", "prod");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   type Library_Kind_Type is ("static", "relocatable");
   Library_Kind_Param : Library_Kind_Type :=
     external ("LIBRARY_TYPE", "relocatable");

   type Boolean is ("false", "true");
   Enable_Warnings : Boolean :=
     external ("${lib_name.upper()}_WARNINGS", "false");

   for Languages use ("Ada", "C");
   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;
   for Interfaces use
     ("adalog.ads",
      "adalog-abstract_relation.ads",
      "adalog-debug.ads",
      "adalog-eq_same.ads",
      "adalog-logic_ref.adb",
      "adalog-logic_ref.ads",
      "adalog-logic_var.ads",
      "adalog-logic_var_predicate.ads",
      "adalog-operations.adb",
      "adalog-operations.ads",
      "adalog-unify.adb",
      "adalog-unify.ads",
      "adalog-unify_one_side.adb",
      "adalog-unify_one_side.ads",
      "adalog-predicates.adb",
      "adalog-predicates.ads",
      "adalog-pure_relations.ads",
      "adalog-relations.adb",
      "adalog-relations.ads",
      "adalog-unify_lr.adb",
      "adalog-unify_lr.ads",
      "adalog-variadic_operations.adb",
      "adalog-variadic_operations.ads",
      "${lib_name.lower()}.ads",
      "${lib_name.lower()}-analysis.adb",
      "${lib_name.lower()}-analysis.ads",
      "${lib_name.lower()}-analysis-c.adb",
      "${lib_name.lower()}-analysis-c.ads",
      "${lib_name.lower()}-analysis-parsers.adb",
      "${lib_name.lower()}-analysis-parsers.ads",
      "${lib_name.lower()}-debug.adb",
      "${lib_name.lower()}-debug.ads",
      "${lib_name.lower()}-init.adb",
      "${lib_name.lower()}-init.ads",
      "${lib_name.lower()}-lexer.adb",
      "${lib_name.lower()}-lexer.ads",
      % for path in ctx.additional_source_files:
      "${os_path.basename(path)}",
      % endfor
      "quex_interface.c",
      "quex_interface.h",
      "quex_lexer.c",
      "quex_lexer-configuration.h",
      "quex_lexer.h",
      "quex_lexer-token.h",
      "quex_lexer-token_ids.h");

   for Source_Dirs use ("../../include/${lib_name.lower()}");
   for Library_Dir use "../${lib_name.lower()}." & Library_Kind_Param;
   for Object_Dir use "../../obj/${lib_name.lower()}." & Library_Kind_Param;

   Target := ${lib_name}'Target;

   package Compiler is

      Common_Ada_Cargs := ();
      case Enable_Warnings is
         when "true" => Common_Ada_Cargs := ("-gnatwaCKMR");
         when others => null;
      end case;

      Common_C_Cargs :=
        ("-I${quex_path}",
         "-DQUEX_OPTION_ASSERTS_DISABLED",
         "-DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED");

      case Build_Mode is
         when "dev" =>
            --  If asked to, enable all warnings and treat them as errors,
            --  except:
            --    * conditional expressions used in tests that are known to be
            --      True or False at compile time (C), as this is very common
            --      in generated properties code;
            --    * variables that could be turned into constants (K), as this
            --      is very common in generated parsers code;
            --    * variables that are assigned and never read (M), as this is
            --      also common in generated parsers code;
            --    * redundant constructs (R), as we do have redundant
            --      conversions for AST nodes (A'Class and B'Class are not
            --      compatible even though B derives from A).

            for Default_Switches ("Ada") use
               Common_Ada_Cargs & ("-g", "-O0", "-gnatwe", "-gnata");

            for Default_Switches ("C") use Common_C_Cargs & ("-g3", "-O0");

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

            case Target is
               when "x86-linux" | "x86-windows" =>
                  for Switches ("quex_lexer.c") use Common_C_Cargs
                    & ("-Ofast", "-fno-ree", "-fdisable-rtl-cprop_hardreg",
                       "-fdisable-rtl-sched2", "-mno-stv");
                  --  Deactivate because of memory usage, see P726-024. This
                  --  limits the memory usage peaks of GCC 6 based compilers
                  --  and should prevent OOM on 32-bit platforms.
               when others =>
                  for Switches ("quex_lexer.c") use Common_C_Cargs
                    & ("-Ofast", "-fno-ree");
                  --  ... and this prevents OOM on other platforms
            end case;
      end case;
   end Compiler;

end ${lib_name};
