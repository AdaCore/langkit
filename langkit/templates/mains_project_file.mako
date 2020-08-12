## vim: ft=makoada

with "langkit_support";
with "${lib_name.lower()}";

project Mains is

   type Build_Mode_Type is ("dev", "prod");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   for Languages use ("Ada");

   <% all_source_dirs = source_dirs | {'.'} %>
   for Source_Dirs use (
      ${', '.join(sorted(string_repr(sdir) for sdir in all_source_dirs))}
   );
   for Exec_Dir use "../bin";
   for Object_Dir use "../obj/mains";

   for Main use (
      ${', '.join(sorted(string_repr('{}.adb'.format(main))
                         for main in main_programs))}
   );

   package Compiler is
      case Build_Mode is
         when "dev" =>
            for Default_Switches ("Ada") use ("-g", "-O0", "-gnata", "-gnatyg",
                                              "-gnatwae");

         when "prod" =>
            --  Debug information is useful even with optimization for
            --  profiling, for instance.
            for Default_Switches ("Ada") use ("-g", "-Ofast");
      end case;

      --  A version of s-memory may be included for memory monitoring
      --  with GNATCOLL.Memory: if so, it requires -gnatg.
      for Switches ("s-memory.adb") use ("-g", "-O2", "-gnatpg");
   end Compiler;


   package Binder is
      for Switches ("ada") use ("-E");
   end Binder;

end Mains;
