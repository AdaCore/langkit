## vim: filetype=makoada

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

end ${ada_lib_name}.Generic_Introspection;
