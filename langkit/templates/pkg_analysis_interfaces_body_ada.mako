## vim: filetype=makoada

with Ada.Unchecked_Conversion;

with System;

package body ${_self.ada_api_settings.lib_name}.Analysis_Interfaces is

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit   : access Analysis_Unit_Interface_Type'Class;
      Object : T_Access)
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Destroy_Procedure);
   begin
      Unit.Register_Destroyable_Helper
        (Object.all'Address,
         Convert (Destroy'Address));
   end Register_Destroyable;

end ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
