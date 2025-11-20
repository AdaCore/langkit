## vim: filetype=makoada

with System;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
private with ${ada_lib_name}.Implementation;

--  This package provides conversion helpers to switch between types as found
--  in ${ada_lib_name}'s public Ada API and the corresponding C API. Use it
--  when interfacing with foreign code.

package ${ada_lib_name}.C is

   function C_Context (Context : Analysis_Context) return System.Address;
   function Ada_Context (Context : System.Address) return Analysis_Context;

   function C_Unit (Unit : Analysis_Unit) return System.Address;
   function Ada_Unit (Unit : System.Address) return Analysis_Unit;

   type C_Node_Type is private;

   function C_Node
     (Node : Analysis.${root_entity.api_name}) return C_Node_Type;
   function Ada_Node
     (Node : C_Node_Type) return Analysis.${root_entity.api_name};

private

   type C_Node_Type is new ${ada_lib_name}.Implementation.${root_entity.name}
   with Convention => C;

end ${ada_lib_name}.C;
