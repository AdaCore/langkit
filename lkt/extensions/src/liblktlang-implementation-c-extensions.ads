package Liblktlang.Implementation.C.Extensions is

   function lkt_create_default_provider return lkt_unit_provider
   with Export, Convention => C;

   function lkt_create_default_provider_from_directories
     (Directories : System.Address) return lkt_unit_provider
   with Export, Convention => C;

end Liblktlang.Implementation.C.Extensions;
