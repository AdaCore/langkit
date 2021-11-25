## vim: filetype=makoada

pragma Warnings (Off, "referenced");
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;
use Langkit_Support.Internal.Conversions;

with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Generic_API;       use ${ada_lib_name}.Generic_API;
with ${ada_lib_name}.Generic_Impl;      use ${ada_lib_name}.Generic_Impl;
with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;
with ${ada_lib_name}.Private_Converters;
use ${ada_lib_name}.Private_Converters;
pragma Warnings (On, "referenced");

package body ${ada_lib_name}.Generic_Introspection is

   <%
      G = generic_api

      def declare_block(decls, stmts):
         """
         Code generation helper to create a declare block.
         """
         if decls:
            return ["declare"] + decls + ["begin"] + stmts + ["end;"]
         else:
            return stmts

      def value_to_public(var_name, expr, t, decls, stmts):
         """
         Code generation helper to convert the ``expr`` internal value
         (Internal_Value_Access), which is wrapping a ``t`` value, storing it
         into the ``var_name`` local variable. Return ``var_name`` for
         convenience.

         ``decls`` and ``stmts`` are respectively a list of declarations and a
         list of statements, where this appends the declarations/statements
         required to do the conversion. Note this function takes care of
         declaring the ``var_name`` local variable.
         """
         public_type = t.public_type

         # Convert the generic value access type to the specific one, so that
         # we have access to the actual value underneath.
         expr = f"{G.internal_value_access(t)} ({expr})"

         # For types that do not need a conversion, just do a renaming, saving
         # the copy.
         init_expr = None
         renaming = False
         if public_type.is_array_type:
            init_expr = f"{expr}.Value.all"
            renaming = True
         elif public_type.is_analysis_unit_type:
            init_expr = f"Get_Unit ({expr}.all)"
         elif public_type.is_string_type:
            init_expr = f"To_Text ({expr}.Value)"
         elif public_type.is_entity_type:
            init_expr = G.to_specific_node(f"{expr}.all", t)
         elif public_type.is_big_integer_type:
            stmts.append(f"Get_Big_Int ({expr}.all, {var_name});")
         elif public_type.is_token_type:
            init_expr = f"From_Generic ({expr}.Value)"
         else:
            init_expr = f"{expr}.Value"
            renaming = True

         decl = f"{var_name} : {public_type.api_name}"
         if renaming:
            decl += f" renames {init_expr}"
         elif init_expr:
            decl += " := " + init_expr
         decls.append(decl + ";")

         return var_name

      def public_to_value(var_name, expr, t, decls, stmts):
         """
         Code generation helper to convert the ``expr`` public value into an
         internal value. See ``value_to_public`` for the description of the
         signature.
         """
         public_type = t.public_type

         decls.append(
            f"{var_name} : {G.internal_value_access(t)} :="
            f"  new {G.internal_value_type(t)};"
         )
         init_stmt = None
         if public_type.is_analysis_unit_type:
            init_stmt = f"Set_Unit ({var_name}, {expr});"
         elif public_type.is_big_integer_type:
            init_stmt = f"Set_Big_Int ({var_name}, {expr});"
         elif public_type.is_entity_type:
            init_stmt = f"Set_Node ({var_name}, {expr});"
         elif public_type.is_string_type:
            init_stmt = f"{var_name}.Value := To_Unbounded_Text ({expr});"
         elif public_type.is_array_type:
            if public_type.element_type.is_big_integer_type:
               # Big integers are limited types, and arrays of big integers are
               # limited themselves, so we need to manually copy its elements.
               pub_var = f"Pub_{var_name}"
               decls.append(
                  f"{pub_var} : constant {t.api_name} := {expr};"
               )
               stmts.extend([
                  f"{var_name}.Value := new {t.api_name} ({pub_var}'Range);",
                  f"for I in {pub_var}'Range loop",
                  f"  {var_name}.Value.all (I).Set ({pub_var} (I));",
                  "end loop;",
               ])
            else:
               init_stmt = f"{var_name}.Value := new {t.api_name}'({expr});"
         elif public_type.is_token_type:
            init_stmt = f"{var_name}.Value := To_Generic ({expr});"
         else:
            init_stmt = f"{var_name}.Value := {expr};"

         if init_stmt:
            stmts.append(init_stmt)
         return var_name
   %>

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
         Item : ${elt_type.api_name} renames Value.Value.all (Index);

         <%
            decls = []
            stmts = []
            result_var = public_to_value(
               "Result", "Item", elt_type, decls, stmts
            )
         %>
         % for d in decls:
            ${d}
         % endfor
      begin
         % for s in stmts:
            ${s}
         % endfor
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
                  % elif elt_type.is_string_type:
                     Result_Item := To_Unbounded_Text (Value.Value)
                  % elif elt_type.public_type.is_entity_type:
                     Result_Item := ${G.to_specific_node("Value", elt_type)};
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

   % for t in G.iterator_types:
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

      ----------
      -- Next --
      ----------

      overriding function Next (Value : ${vt}) return Internal_Value_Access is
         <% elt_type = t.element_type %>
         Item : ${t.element_type.api_name};
      begin
         if Next (Value.Value, Item) then
            <%
               decls = []
               stmts = []
               result_var = public_to_value(
                  "Result", "Item", elt_type, decls, stmts
               )
               stmts.append("return Internal_Value_Access (Result);")
            %>
            ${"\n".join(declare_block(decls, stmts))}
         else
            return null;
         end if;
      end Next;

   % endfor

   % for t in G.struct_types:
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

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return ${G.internal_value_access(t)}
      is
         <%
            fields = t.get_fields()
            var_names = []
            decls = []
            stmts = []

            ## Extract fields into local variables
            for i, f in enumerate(fields, 1):
               var_names.append(value_to_public(
                  f"F_{f.name}", f"Values ({i})", f.type, decls, stmts
               ))
         %>
         % for d in decls:
            ${d}
         % endfor
      begin
         % for s in stmts:
            ${s}
         % endfor

         return Result : constant ${G.internal_value_access(t)} := new ${vt} do
            Result.Value := Create_${t.api_name} (${", ".join(var_names)});
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : ${vt};
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
            % for f in t.get_fields():
               <% public_type = f.type.public_type %>
               when ${G.member_index(f)} =>
                  declare
                     Item : constant ${public_type.api_name}
                        ## Due to Ada language constraints, public struct field
                        ## accessors that returns nodes actually return a
                        ## class-wide type.
                        % if public_type.is_entity_type:
                           'Class
                        % endif
                     := Analysis.${f.name} (Value.Value);

                     <%
                        decls = []
                        stmts = []
                        public_to_value("Result", "Item", f.type, decls, stmts)
                     %>

                     % for d in decls:
                        ${d}
                     % endfor
                  begin
                     % for s in stmts:
                        ${s}
                     % endfor
                     return Internal_Value_Access (Result);
                  end;
            % endfor

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

   % endfor

   -------------------
   -- Create_Struct --
   -------------------

   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access is
   begin
      % if not G.struct_types:
         pragma Unreferenced (Values);
      % endif

      case Struct_Type is
         % for t in G.struct_types:
            when ${G.type_index(t)} =>
               declare
                  Result : constant ${G.internal_value_access(t)} :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
         % endfor

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            return (raise Program_Error);
      end case;
   end Create_Struct;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
     (Intr_Value   : ${G.internal_value_access(T.AnalysisUnit)};
      Actual_Value : ${T.AnalysisUnit.api_name})
   is
      U : constant Internal_Unit :=
        +Public_Converters.Unwrap_Unit (Actual_Value);
   begin
      Intr_Value.Value :=
        Langkit_Support.Internal.Conversions.Wrap_Unit (Self_Id, U);
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
      Actual_Value : ${T.entity.api_name}'Class)
   is
      E : constant Internal_Entity := +Unwrap_Entity (Actual_Value);
   begin
      Intr_Value.Value :=
        Langkit_Support.Internal.Conversions.Wrap_Node (Self_Id, E);
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
