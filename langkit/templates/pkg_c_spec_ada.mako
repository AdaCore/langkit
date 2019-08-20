## vim: filetype=makoada

with System;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;

--  This package provides conversion helpers to switch between types as found
--  in ${ada_lib_name}'s public Ada API and the corresponding C API. Use it
--  when interfacing with foreign code.

package ${ada_lib_name}.C is

   function C_Context (Context : Analysis_Context) return System.Address;
   function Ada_Context (Context : System.Address) return Analysis_Context;

   function C_Unit (Unit : Analysis_Unit) return System.Address;
   function Ada_Unit (Unit : System.Address) return Analysis_Unit;

end ${ada_lib_name}.C;
