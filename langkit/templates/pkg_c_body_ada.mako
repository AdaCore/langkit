## vim: filetype=makoada

with Ada.Unchecked_Conversion;

with ${ada_lib_name}.Converters; use ${ada_lib_name}.Converters;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;

package body ${ada_lib_name}.C is

   --  The following conversions are used only at the interface between Ada and
   --  C (i.e. as parameters and return types for C entry points) for access
   --  types.  All read/writes for the pointed values are made through the
   --  access values and never through the System.Address values.  Thus, strict
   --  aliasing issues should not arise for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "possible aliasing problem for type");

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, System.Address);
   function "+" is new Ada.Unchecked_Conversion
     (System.Address, Internal_Context);

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Unit, System.Address);
   function "+" is new Ada.Unchecked_Conversion
     (System.Address, Internal_Unit);

   pragma Warnings (On, "possible aliasing problem for type");

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

end ${ada_lib_name}.C;
