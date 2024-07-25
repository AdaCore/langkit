## vim: ft=makoada

with "langkit_support";
with "${lib_name.lower()}";

project Mains is

   type Build_Mode_Type is ("dev", "prod", "prof");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   for Languages use ("Ada");

   <% all_source_dirs = source_dirs | {'src-mains'} %>
   for Source_Dirs use (
      ${', '.join(sorted(ascii_repr(sdir) for sdir in all_source_dirs))}
   );
   for Object_Dir use "obj-mains/" & Build_Mode;

   for Main use (
      ${', '.join(sorted(ascii_repr('{}.adb'.format(main))
                         for main in main_programs))}
   );

   package Builder is
      ## Add a language-specific prefix to the name of the default main
      ## programs for convenience, so that programs from multiple
      ## Langkit-generated libraries can coexist.
      % for main in ["parse", "unparse"]:
         % if main in main_programs:
            for Executable ("${main}") use "${ctx.short_name_or_long}_${main}";
         % endif
      % endfor
   end Builder;

   Common_Ada_Cargs := ("-gnatX");

   package Compiler is
      for Local_Configuration_Pragmas use "gnat.adc";

      case Build_Mode is
         when "dev" =>
            for Default_Switches ("Ada") use
              Common_Ada_Cargs & ("-g", "-O0", "-gnatyg", "-gnatwae");

         when "prod" =>
            --  Debug information is useful even with optimization for
            --  profiling, for instance.
            for Default_Switches ("Ada") use
              Common_Ada_Cargs & ("-g", "-Ofast");

         when "prof" =>
            --  Ensure that we have a relatively fast build but with all
            --  possible stack info & debug info, for profiling.
            for Default_Switches ("Ada") use
               Common_Ada_Cargs
               & ("-Og", "-ggdb", "-g3", "-fno-omit-frame-pointer");
      end case;

      --  A version of s-memory may be included for memory monitoring
      --  with GNATCOLL.Memory: if so, it requires -gnatg.
      for Switches ("s-memory.adb") use ("-g", "-O2", "-gnatpg");
   end Compiler;

   package Binder is
      case Build_Mode is
         when "dev" =>
            for Switches ("Ada") use ("-Es");
         when "prod" | "prof" =>
            for Switches ("Ada") use ("-E");
      end case;
   end Binder;

end Mains;
