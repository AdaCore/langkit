## vim: filetype=makoada

pragma Warnings (Off, "referenced");
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;
use Langkit_Support.Internal.Conversions;

with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Generic_Impl;      use ${ada_lib_name}.Generic_Impl;
with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;
pragma Warnings (On, "referenced");

package body ${ada_lib_name}.Generic_Introspection is

   <% G = generic_api %>

   % for t in G.enum_types:

      <% vt = G.internal_value_type(t) %>

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : ${vt}) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : ${vt}) return Type_Index is
      begin
         return ${G.type_index(t)};
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : ${vt}) return String is
      begin
         return "${t.name}(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : ${vt}) return Enum_Value_Index
      is
      begin
         ## 'Pos returns a 0-based index, while Enum_Value_Index is 1-based
         return ${t.name}'Pos (Value.Value) + 1;
      end Value_Index;

   % endfor

   -----------------
   -- Create_Enum --
   -----------------

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access
   is
   begin
      case Enum_Type is
         % for t in G.enum_types:
            when ${G.type_index(t)} =>
               declare
                  Result : constant ${G.internal_value_access(t)} :=
                    new ${G.internal_value_type(t)};
               begin
                  ## 'Pos takes a 0-based index, while Enum_Value_Index is
                  ## 1-based.
                  Result.Value := ${t.name}'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
         % endfor

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-enum types.
            raise Program_Error;
      end case;
   end Create_Enum;

   % for t in G.array_types:
      <%
         elt_type = t.element_type
         vt = G.internal_value_type(t)
      %>

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : ${vt}) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out ${vt}) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : ${vt}) return Type_Index is
      begin
         return ${G.type_index(t)};
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : ${vt}) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : ${vt}; Index : Positive) return Internal_Value_Access
      is
         Item   : ${elt_type.api_name} renames Value.Value.all (Index);
         Result : ${G.internal_value_access(elt_type)} :=
           new ${G.internal_value_type(elt_type)};
      begin
         % if elt_type.is_analysis_unit_type:
            Set_Unit (Result, Item, Value.Id);
         % elif elt_type.is_big_integer_type:
            Set_Big_Int (Result, Item);
         % elif elt_type.is_entity_type:
            Set_Node (Result, Item, Value.Id);
         % else:
            Result.Value := Item;
         % endif
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return ${G.internal_value_access(t)}
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant ${G.internal_value_access(t)} := new ${vt} do
            Result.Value := new ${t.api_name} (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : ${elt_type.api_name} renames
                    Result.Value (Result_Index);
                  Value       : ${G.internal_value_type(elt_type)} renames
                    ${G.internal_value_access(elt_type)} (Values (I)).all;
               begin
                  % if elt_type.is_analysis_unit_type:
                     Result_Item := Get_Unit (Value);
                  % elif elt_type.is_big_integer_type:
                     Get_Big_Int (Value, Result_Item);
                  % elif elt_type.is_entity_type:
                     <%
                        public_node = "Get_Node (Value)"
                        if not elt_type.element_type.is_root_node:
                           public_node += f".As_{elt_type.api_name}"
                     %>
                     Result_Item := ${public_node};
                  % else:
                     Result_Item := Value.Value;
                  % endif
               end;
            end loop;
         end return;
      end Create_Array;

   % endfor

   ------------------
   -- Create_Array --
   ------------------

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access is
   begin
      case Array_Type is
         % for t in G.array_types:
            when ${G.type_index(t)} =>
               declare
                  Result : constant ${G.internal_value_access(t)} :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
         % endfor

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            raise Program_Error;
      end case;
   end Create_Array;

   % for t in G.struct_types:
      <% vt = G.internal_value_type(t) %>

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : ${vt}) return Type_Index is
      begin
         return ${G.type_index(t)};
      end Type_Of;
   % endfor

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
     (Intr_Value   : ${G.internal_value_access(T.AnalysisUnit)};
      Actual_Value : ${T.AnalysisUnit.api_name};
      Id           : Language_Id)
   is
      U : constant Internal_Unit :=
        +Public_Converters.Unwrap_Unit (Actual_Value);
   begin
      Intr_Value.Value :=
        Langkit_Support.Internal.Conversions.Wrap_Unit (Id, U);
   end Set_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Intr_Value : ${G.internal_value_type(T.AnalysisUnit)})
      return ${T.AnalysisUnit.api_name}
   is
      U : constant Implementation.Internal_Unit :=
        +Langkit_Support.Internal.Conversions.Unwrap_Unit (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Unit (U);
   end Get_Unit;

   -----------------
   -- Set_Big_Int --
   -----------------

   procedure Set_Big_Int
     (Intr_Value   : ${G.internal_value_access(T.BigInt)};
      Actual_Value : ${T.BigInt.api_name}) is
   begin
      Intr_Value.Value.Set (Actual_Value);
   end Set_Big_Int;

   -----------------
   -- Get_Big_Int --
   -----------------

   procedure Get_Big_Int
     (Intr_Value   : ${G.internal_value_type(T.BigInt)};
      Actual_Value : out ${T.BigInt.api_name})
   is
   begin
      Actual_Value.Set (Intr_Value.Value);
   end Get_Big_Int;

   --------------
   -- Set_Node --
   --------------

   procedure Set_Node
     (Intr_Value   : ${G.internal_value_access(T.entity)};
      Actual_Value : ${T.entity.api_name}'Class;
      Id           : Language_Id)
   is
      E : constant Internal_Entity := +Unwrap_Entity (Actual_Value);
   begin
      Intr_Value.Value :=
        Langkit_Support.Internal.Conversions.Wrap_Node (Id, E);
   end Set_Node;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Intr_Value : ${G.internal_value_type(T.entity)})
      return ${T.entity.api_name}
   is
      E : constant Implementation.${root_entity.name} :=
        +Langkit_Support.Internal.Conversions.Unwrap_Node (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Node (E.Node, E.Info);
   end Get_Node;

end ${ada_lib_name}.Generic_Introspection;
