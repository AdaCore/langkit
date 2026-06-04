

--  This package provides contants to refer to the default unparsing
--  configuration and builtin configuration overridings.

package Liblktlang.Generic_API.Unparsing is

   Default_Configuration_Filename : constant String :=
     "builtin://unparsing/default_config.json";
   --  Filename to use to refer to the default unparsing configuration

   package Builtin_Overridings is
   end Builtin_Overridings;
   --  Filenames to use to refer to builtin configuration overridings

end Liblktlang.Generic_API.Unparsing;
