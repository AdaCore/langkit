## vim: filetype=makoada

with System;

with ${ada_lib_name}.Generic_Impl; use ${ada_lib_name}.Generic_Impl;

package body ${ada_lib_name}.Generic_API is

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "${ada_lib_name}__language_id";

   procedure Dummy is null;

end ${ada_lib_name}.Generic_API;
