## vim: filetype=makoada

with Ada.Unchecked_Conversion;

with System;

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

   subtype Generic_Internal_Unit is
     Langkit_Support.Internal.Analysis.Internal_Unit;
   subtype Specific_Internal_Unit is
     ${ada_lib_name}.Implementation.Internal_Unit;

   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Context, Specific_Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Context, Generic_Internal_Context);

   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Unit, Specific_Internal_Unit);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Unit, Generic_Internal_Unit);

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

   function From_Generic_Context (Context : Lk_Context) return Analysis_Context
   is
   begin
      if Context = No_Lk_Context then
         return No_Analysis_Context;
      elsif Context.Language /= Self_Id then
         raise Precondition_Failure with "context belongs to another language";
      else
         declare
            Ctx : constant Generic_Internal_Context :=
              Lk_Convs.Unwrap_Context (Context);
         begin
            return Wrap_Context.all (+Ctx);
         end;
      end if;
   end From_Generic_Context;

   ---------------------
   -- To_Generic_Unit --
   ---------------------

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit is
      U : constant Specific_Internal_Unit := Unwrap_Unit.all (Unit);
   begin
      return Lk_Convs.Wrap_Unit (Self_Id, +U);
   end To_Generic_Unit;

   -----------------------
   -- From_Generic_Unit --
   -----------------------

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit is
   begin
      if Unit = No_Lk_Unit then
         return No_Analysis_Unit;
      elsif Unit.Language /= Self_Id then
         raise Precondition_Failure with "unit belongs to another language";
      else
         declare
            U : constant Generic_Internal_Unit := Lk_Convs.Unwrap_Unit (Unit);
         begin
            return Wrap_Unit.all (+U);
         end;
      end if;
   end From_Generic_Unit;

   -----------------------------
   -- To_Generic_Grammar_Rule --
   -----------------------------

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Langkit_Support.Generic_API.Grammar_Rule_Ref
   is
   begin
      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return From_Index (Self_Id, Grammar_Rule'Pos (Rule) + 1);
   end To_Generic_Grammar_Rule;

   -------------------------------
   -- From_Generic_Grammar_Rule --
   -------------------------------

   function From_Generic_Grammar_Rule
     (Rule : Langkit_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule
   is
   begin
      if Rule = Langkit_Support.Generic_API.No_Grammar_Rule_Ref then
         raise Precondition_Failure
           with "null grammar rule";
      elsif Language (Rule) /= Self_Id then
         raise Precondition_Failure
           with "grammar rule belongs to another language";
      end if;

      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return Grammar_Rule'Val (To_Index (Rule) - 1);
   end From_Generic_Grammar_Rule;

   ---------------------
   -- To_Generic_Node --
   ---------------------

   function To_Generic_Node
     (Node : ${root_entity.api_name}'Class) return Lk_Node
   is
      E : constant Implementation.${root_entity.name} :=
        Unwrap_Entity.all (Node);
   begin
      return Lk_Convs.Wrap_Node (Self_Id, +E);
   end To_Generic_Node;

   -----------------------
   -- From_Generic_Node --
   -----------------------

   function From_Generic_Node (Node : Lk_Node) return ${root_entity.api_name}
   is
   begin
      if Node.Is_Null then
         return No_${root_entity.api_name};
      elsif Node.Language /= Self_Id then
         raise Precondition_Failure with "node belongs to another language";
      else
         declare
            N : constant Langkit_Support.Internal.Analysis.Internal_Entity :=
              Lk_Convs.Unwrap_Node (Node);
            E : constant Implementation.${root_entity.name} := +N;
         begin
            return Wrap_Node.all (E.Node, E.Info);
         end;
      end if;
   end From_Generic_Node;

end ${ada_lib_name}.Generic_API;
