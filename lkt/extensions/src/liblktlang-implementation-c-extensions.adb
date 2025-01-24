with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;

with Liblktlang.Default_Provider; use Liblktlang.Default_Provider;

package body Liblktlang.Implementation.C.Extensions is

   ---------------------------------
   -- lkt_create_default_provider --
   ---------------------------------

   function lkt_create_default_provider
     (Directories : System.Address) return lkt_unit_provider
   is
      Result : Internal_Unit_Provider_Access;
   begin
      Clear_Last_Exception;

      declare
         Ada_Dirs : String_Vectors.Vector;
         C_Dirs   : constant array (Positive) of chars_ptr
           with Import, Address => Directories;
      begin
         for C_Dirname of C_Dirs loop
            exit when C_Dirname = Null_Ptr;
            Ada_Dirs.Append (US.To_Unbounded_String (Value (C_Dirname)));
         end loop;

         Result := Create_From_Dirs (Ada_Dirs);
      end;

      return Wrap_Private_Provider (Result);

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return lkt_unit_provider (System.Null_Address);
   end lkt_create_default_provider;

end Liblktlang.Implementation.C.Extensions;
