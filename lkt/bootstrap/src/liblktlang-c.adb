
with Ada.Unchecked_Conversion;

with Liblktlang.Implementation;    use Liblktlang.Implementation;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;

package body Liblktlang.C is

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, System.Address);
   function "+" is new Ada.Unchecked_Conversion
     (System.Address, Internal_Context);

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Unit, System.Address);
   function "+" is new Ada.Unchecked_Conversion
     (System.Address, Internal_Unit);

   ---------------
   -- C_Context --
   ---------------

   function C_Context (Context : Analysis_Context) return System.Address is
   begin
      return +Unwrap_Context (Context);
   end C_Context;

   -----------------
   -- Ada_Context --
   -----------------

   function Ada_Context (Context : System.Address) return Analysis_Context is
   begin
      return Wrap_Context (+Context);
   end Ada_Context;

   ------------
   -- C_Unit --
   ------------

   function C_Unit (Unit : Analysis_Unit) return System.Address is
   begin
      return +Unwrap_Unit (Unit);
   end C_Unit;

   --------------
   -- Ada_Unit --
   --------------

   function Ada_Unit (Unit : System.Address) return Analysis_Unit is
   begin
      return Wrap_Unit (+Unit);
   end Ada_Unit;

   ------------
   -- C_Node --
   ------------

   function C_Node (Node : Analysis.Lkt_Node) return C_Node_Type
   is
      N : constant Implementation.Internal_Entity := Unwrap_Entity (Node);
   begin
      return C_Node_Type (N);
   end C_Node;

   --------------
   -- Ada_Node --
   --------------

   function Ada_Node
     (Node : C_Node_Type) return Analysis.Lkt_Node is
   begin
      return Wrap_Node (Node.Node, Node.Info);
   end Ada_Node;

end Liblktlang.C;
