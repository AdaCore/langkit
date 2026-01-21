
with System;

with Liblktlang.Analysis; use Liblktlang.Analysis;
private with Liblktlang.Implementation;

--  This package provides conversion helpers to switch between types as found
--  in Liblktlang's public Ada API and the corresponding C API. Use it
--  when interfacing with foreign code.

package Liblktlang.C is

   function C_Context (Context : Analysis_Context) return System.Address;
   function Ada_Context (Context : System.Address) return Analysis_Context;

   function C_Unit (Unit : Analysis_Unit) return System.Address;
   function Ada_Unit (Unit : System.Address) return Analysis_Unit;

   type C_Node_Type is private;

   function C_Node
     (Node : Analysis.Lkt_Node) return C_Node_Type;
   function Ada_Node
     (Node : C_Node_Type) return Analysis.Lkt_Node;

private

   type C_Node_Type is new Liblktlang.Implementation.Internal_Entity
   with Convention => C;

end Liblktlang.C;
