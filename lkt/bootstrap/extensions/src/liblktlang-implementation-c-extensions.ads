package Liblktlang.Implementation.C.Extensions is

   function lkt_create_default_provider
     (Mode : Language_Mode) return lkt_unit_provider
   with Export, Convention => C;

   function lkt_create_default_provider_from_directories
     (Mode              : Language_Mode;
      Directories       : System.Address;
      Directories_Count : int)
      return lkt_unit_provider
   with Export, Convention => C;

end Liblktlang.Implementation.C.Extensions;
