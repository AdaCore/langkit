## vim: ft=makoada

with "langkit_support";
with "${lib_name.lower()}";

project Mains is

   type Build_Mode_Type is ("dev", "prod");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   for Languages use ("Ada");

   for Source_Dirs use (".");
   for Exec_Dir use "../bin";
   for Object_Dir use "../obj/mains";

   for Main use (
      % for i, main in enumerate(main_programs):
         % if i > 0:
            ,
         % endif
         "${main}.adb"
      % endfor
   );

   package Compiler is
      case Build_Mode is
         when "dev" =>
            for Default_Switches ("Ada") use ("-g", "-O0", "-gnata");

         when "prod" =>
            --  Debug information is useful even with optimization for
            --  profiling, for instance.
            for Default_Switches ("Ada") use ("-g", "-Ofast", "-gnatp");
      end case;
   end Compiler;

end Mains;
