## vim: filetype=makoada

with Ada.Unchecked_Conversion;

with System;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Internal.Analysis;
use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;

with ${ada_lib_name}.Generic_Impl;      use ${ada_lib_name}.Generic_Impl;
with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;

package body ${ada_lib_name}.Generic_API is

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "${ada_lib_name}__language_id";

   package Lk_Convs renames Langkit_Support.Internal.Conversions;

   subtype Generic_Internal_Context is
     Langkit_Support.Internal.Analysis.Internal_Context;
   subtype Specific_Internal_Context is
     ${ada_lib_name}.Implementation.Internal_Context;

   pragma Warnings (Off, "possible aliasing problem for type");
   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Context, Specific_Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Context, Generic_Internal_Context);
   pragma Warnings (On, "possible aliasing problem for type");

   ------------------------
   -- To_Generic_Context --
   ------------------------

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context
   is
      Ctx : constant Specific_Internal_Context := Unwrap_Context.all (Context);
   begin
      return Lk_Convs.Wrap_Context (Self_Id, +Ctx);
   end To_Generic_Context;

   --------------------------
   -- From_Generic_Context --
   --------------------------

   function From_Generic_Context
     (Context : Lk_Context) return Analysis_Context
   is
      Ctx : constant Generic_Internal_Context :=
        Lk_Convs.Unwrap_Context (Context);
   begin
      if Language (Context) /= Self_Id then
         raise Precondition_Failure with "context belongs to another language";
      end if;
      return Wrap_Context.all (+Ctx);
   end From_Generic_Context;

end ${ada_lib_name}.Generic_API;
