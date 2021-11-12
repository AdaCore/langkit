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

   % endfor

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

end ${ada_lib_name}.Generic_Introspection;
