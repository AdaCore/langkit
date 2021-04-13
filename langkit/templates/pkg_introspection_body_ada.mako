## vim: filetype=makoada

with ${ada_lib_name}.Implementation;    use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Introspection_Implementation;
with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;

package body ${ada_lib_name}.Introspection is

   package Impl renames Introspection_Implementation;

   --  TODO: move implementation of functions dealing with values (Satisfies,
   --  Eval_Property, ...) to Impl. This is not not done yet as substantial
   --  work is required in order to convert back and forth public values
   --  (structures, symbols) to their internal representations.

   function Allocate (Kind : Value_Kind) return Value_Type;
   --  Allocate a polymorphic value of the given kind

   pragma Warnings (Off, "is not referenced");
   function To_Internal_Value
     (Value : Any_Value_Type) return Impl.Internal_Value;
   function From_Internal_Value
     (Value : Impl.Internal_Value) return Any_Value_Type;
   pragma Warnings (On, "is not referenced");

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Value_Access_Wrapper) is
   begin
      if Self.Value = null then
         return;
      end if;

      declare
         Rec : Value_Record renames Self.Value.all;
      begin
         Rec.Ref_Count := Rec.Ref_Count + 1;
      end;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Value_Access_Wrapper) is
   begin
      if Self.Value = null then
         return;
      end if;

      --  If Self is non-null, decrement the reference count of the referenced
      --  value.

      declare
         Rec : Value_Record renames Self.Value.all;
      begin
         Rec.Ref_Count := Rec.Ref_Count - 1;

         if Rec.Ref_Count = 0 then
            --  Reference count dropped to 0: time to free the value and what
            --  is inside.

            case Rec.Kind is
               % for t in ctx.array_types:
                  % if t.exposed:
                     when ${t.introspection_kind} =>
                        Free (Rec.${t.introspection_kind});
                  % endif
               % endfor
               when others => null;
            end case;

            Free (Self.Value);
         end if;
      end;
   end Finalize;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Value_Type) return Value_Kind is
   begin
      return Self.Value.Value.Kind;
   end Kind;

   --------------
   -- Allocate --
   --------------

   function Allocate (Kind : Value_Kind) return Value_Type is
      Result : Any_Value_Type;
   begin
      Result.Value.Value := new Value_Record (Kind);
      Result.Value.Value.Ref_Count := 1;
      return Result;
   end Allocate;

   -----------------------
   -- To_Internal_Value --
   -----------------------

   function To_Internal_Value
     (Value : Any_Value_Type) return Impl.Internal_Value is
   begin
      if Value = No_Value then
         return Impl.No_Internal_Value;
      end if;

      case Kind (Value) is
         when Boolean_Value =>
            return Impl.Create_Boolean (As_Boolean (Value));

         when Integer_Value =>
            return Impl.Create_Integer (As_Integer (Value));

         when Character_Value =>
            return Impl.Create_Character (As_Character (Value));

         when Node_Value =>
            return Impl.Create_Node (Unwrap_Entity (As_Node (Value)));

         when others =>
            --  For now we use this only to handle default values, so this
            --  should be unreachable.
            raise Program_Error;
      end case;
   end To_Internal_Value;

   -------------------------
   -- From_Internal_Value --
   -------------------------

   function From_Internal_Value
     (Value : Impl.Internal_Value) return Any_Value_Type is
   begin
      case Value.Kind is
         when None =>
            return No_Value;

         when Boolean_Value =>
            return Create_Boolean (Impl.As_Boolean (Value));

         when Integer_Value =>
            return Create_Integer (Impl.As_Integer (Value));

         when Character_Value =>
            return Create_Character (Impl.As_Character (Value));

         % for enum_type in ctx.enum_types:
            when ${enum_type.introspection_kind} =>
               return Create_${enum_type.api_name}
                 (Impl.As_${enum_type.api_name} (Value));
         % endfor

         when Node_Value =>
            declare
               N : constant ${T.entity.name} := Impl.As_Node (Value);
            begin
               return Create_Node (Wrap_Node (N.Node, N.Info));
            end;
      end case;
   end From_Internal_Value;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Value_Type) return Boolean is
   begin
      return Self.Value.Value.Boolean_Value;
   end As_Boolean;

   --------------------
   -- Create_Boolean --
   --------------------

   function Create_Boolean (Value : Boolean) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Boolean_Value) do
         Result.Value.Value.Boolean_Value := Value;
      end return;
   end Create_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Value_Type) return Integer is
   begin
      return Self.Value.Value.Integer_Value;
   end As_Integer;

   --------------------
   -- Create_Integer --
   --------------------

   function Create_Integer (Value : Integer) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Integer_Value) do
         Result.Value.Value.Integer_Value := Value;
      end return;
   end Create_Integer;

   --------------------
   -- As_Big_Integer --
   --------------------

   function As_Big_Integer (Self : Value_Type) return Big_Integer is
   begin
      return Result : Big_Integer do
         Result.Set (Self.Value.Value.Big_Integer_Value);
      end return;
   end As_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer (Value : Big_Integer) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Big_Integer_Value) do
         Result.Value.Value.Big_Integer_Value.Set (Value);
      end return;
   end Create_Big_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Value_Type) return Character_Type is
   begin
      return Self.Value.Value.Character_Value;
   end As_Character;

   ----------------------
   -- Create_Character --
   ----------------------

   function Create_Character (Value : Character_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Character_Value) do
         Result.Value.Value.Character_Value := Value;
      end return;
   end Create_Character;

   --------------
   -- As_Token --
   --------------

   function As_Token (Self : Value_Type) return Token_Reference is
   begin
      return Self.Value.Value.Token_Value;
   end As_Token;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token (Value : Token_Reference) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Token_Value)
      do
         Result.Value.Value.Token_Value := Value;
      end return;
   end Create_Token;

   -----------------------
   -- As_Unbounded_Text --
   -----------------------

   function As_Unbounded_Text (Self : Value_Type) return Unbounded_Text_Type is
   begin
      return Self.Value.Value.Unbounded_Text_Value;
   end As_Unbounded_Text;

   ---------------------------
   -- Create_Unbounded_Text --
   ---------------------------

   function Create_Unbounded_Text
     (Value : Unbounded_Text_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Unbounded_Text_Value)
      do
         Result.Value.Value.Unbounded_Text_Value := Value;
      end return;
   end Create_Unbounded_Text;

   ----------------------
   -- As_Analysis_Unit --
   ----------------------

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit is
   begin
      return Self.Value.Value.Analysis_Unit_Value;
   end As_Analysis_Unit;

   --------------------------
   -- Create_Analysis_Unit --
   --------------------------

   function Create_Analysis_Unit (Value : Analysis_Unit) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Analysis_Unit_Value) do
         Result.Value.Value.Analysis_Unit_Value := Value;
      end return;
   end Create_Analysis_Unit;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Value_Type) return ${root_entity.api_name} is
   begin
      return Self.Value.Value.Node_Value;
   end As_Node;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Value : ${root_entity.api_name}'Class) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Node_Value) do
         Result.Value.Value.Node_Value := Value.As_${root_entity.api_name};
      end return;
   end Create_Node;

   % for enum_type in ctx.enum_types:
      function As_${enum_type.introspection_prefix}
        (Self : Value_Type) return ${enum_type.api_name} is
      begin
         return Self.Value.Value.${enum_type.introspection_kind};
      end As_${enum_type.introspection_prefix};

      function Create_${enum_type.introspection_prefix}
        (Value : ${enum_type.api_name}) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (${enum_type.introspection_kind})
         do
            Result.Value.Value.${enum_type.introspection_kind} := Value;
         end return;
      end Create_${enum_type.introspection_prefix};
   % endfor

   % for t in ctx.composite_types:
      % if t.exposed and not t.is_entity_type:
         function As_${t.introspection_prefix}
           (Self : Value_Type) return ${t.api_name} is
         begin
            % if t.is_array_type:
               ## If `t` is an array, first allocate the array and then
               ## initialize it one item at a time.
               return Result : ${t.api_name}
                 (Self.Value.Value.${t.introspection_kind}'Range)
               do
                  for I in Result'Range loop
                     ## Special case for big integer types: they are limited,
                     ## so we cannot use mere assignment.
                     % if t.element_type.is_big_integer_type:
                        Result (I).Set
                          (Self.Value.Value.${t.introspection_kind}.all (I));
                     % else:
                        Result (I) :=
                           Self.Value.Value.${t.introspection_kind}.all (I);
                     % endif
                  end loop;
               end return;

            % else:
               ## For other types, a mere assignment is fine
               return Self.Value.Value.${t.introspection_kind};
            % endif
         end As_${t.introspection_prefix};

         function Create_${t.introspection_prefix}
           (Value : ${t.api_name}) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (${t.introspection_kind})
            do
               % if t.is_array_type:
                  ## If `t` is an array, first allocate the array and then
                  ## initialize it one item at a time.
                  Result.Value.Value.${t.introspection_kind} :=
                     new ${t.api_name} (Value'Range);
                  for I in Value'Range loop
                     ## Special case for big integer types: they are limited,
                     ## so we cannot use mere assignment.
                     % if t.element_type.is_big_integer_type:
                        Result.Value.Value.${t.introspection_kind}
                           .all (I).Set (Value (I));
                     % else:
                        Result.Value.Value.${t.introspection_kind}.all (I) :=
                           Value (I);
                     % endif
                  end loop;

               % else:
                  ## For other types, a mere assignment is fine
                  Result.Value.Value.${t.introspection_kind} := Value;
               % endif
            end return;
         end Create_${t.introspection_prefix};
      % endif
   % endfor

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type is
   begin
      return Impl.DSL_Name (Id);
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id is
   begin
      return Impl.Lookup_DSL_Name (Name);
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Abstract (Id);
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return ${T.node_kind} is
   begin
      return Impl.Kind_For (Id);
   end Kind_For;

   --------------------
   -- First_Kind_For --
   --------------------

   function First_Kind_For (Id : Node_Type_Id) return ${T.node_kind} is
   begin
      return Impl.First_Kind_For (Id);
   end First_Kind_For;

   -------------------
   -- Last_Kind_For --
   -------------------

   function Last_Kind_For (Id : Node_Type_Id) return ${T.node_kind} is
   begin
      return Impl.Last_Kind_For (Id);
   end Last_Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : ${T.node_kind}) return Node_Type_Id is
   begin
      return Impl.Id_For_Kind (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Root_Node (Id);
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      return Impl.Base_Type (Id);
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Impl.Derived_Types (Id);
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Derived_From (Id, Parent);
   end Is_Derived_From;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Constraint : Type_Constraint) return Text_Type is
   begin
      <% basic_types = [T.Bool, T.Int, T.BigInt, T.Character, T.Token,
                        T.Symbol, T.AnalysisUnit] %>
      case Constraint.Kind is
         % for t in basic_types + ctx.enum_types + ctx.composite_types:
            % if t.exposed and not t.is_entity_type:
               when ${t.introspection_kind} =>
                  return "${t.dsl_name}";
            % endif
         % endfor

         when Node_Value =>
            return DSL_Name (Constraint.Node_Type);
      end case;
   end DSL_Name;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies
     (Value : Value_Type; Constraint : Type_Constraint) return Boolean is
   begin
      if Value.Value.Value.Kind /= Constraint.Kind then
         return False;
      end if;

      case Constraint.Kind is
         when Node_Value =>
            return

              --  A null node always satisfies the type constraint
              Value.Value.Value.Node_Value.Is_Null

              --  Else, check that the type of the node is derived from the
              --  type of the constraint.
              or else Is_Derived_From
                (Id_For_Kind (Value.Value.Value.Node_Value.Kind),
                 Constraint.Node_Type);

         when others =>
            return True;
      end case;
   end Satisfies;

   <% enum_types = [t for t in ctx.enum_types if t.exposed] %>

   ---------------------
   -- Enum_Last_Value --
   ---------------------

   function Enum_Last_Value (Kind : Enum_Value_Kind) return Enum_Value_Index is
   begin
      case Kind is
         % for t in enum_types:
            when ${t.introspection_kind} =>
               return ${len(t.values)};
         % endfor
      end case;
   end Enum_Last_Value;

   ------------------------
   -- Enum_Default_Value --
   ------------------------

   function Enum_Default_Value
     (Kind : Enum_Value_Kind) return Any_Enum_Value_Index is
   begin
      case Kind is
         % for t in enum_types:
            when ${t.introspection_kind} =>
               % if t.default_val_name is None:
                  return No_Enum_Value_Index;
               % else:
                  return ${t.values_dict[t.default_val_name].index + 1};
               % endif
         % endfor
      end case;
   end Enum_Default_Value;

   ---------------------
   -- Enum_Value_Name --
   ---------------------

   function Enum_Value_Name
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Text_Type is
   begin
      case Kind is
         % for t in enum_types:
            when ${t.introspection_kind} =>
               case Index is
                  % for i, v in enumerate(t.values, start=1):
                     when ${i} =>
                        return "${v.name.lower}";
                  % endfor

                  when others => null;
               end case;
         % endfor
      end case;

      return (raise Out_Of_Bounds_Error with "out of bounds enum value index");
   end Enum_Value_Name;

   -----------------
   -- Create_Enum --
   -----------------

   function Create_Enum
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Value_Type is
   begin
      case Kind is
         % for t in enum_types:
            when ${t.introspection_kind} =>
               case Index is
                  % for v in t.values:
                     when ${v.index + 1} =>
                        return Create_${t.api_name} (${v.ada_name});
                  % endfor

                  when others => null;
               end case;
         % endfor
      end case;

      return (raise Out_Of_Bounds_Error with "out of bounds enum value index");
   end Create_Enum;

   ----------------
   -- Enum_Index --
   ----------------

   function Enum_Index (Value : Value_Type) return Enum_Value_Index is
   begin
      case Kind (Value) is
         % for t in enum_types:
            when ${t.introspection_kind} =>
               return ${t.api_name}'Pos (As_${t.api_name} (Value)) + 1;
         % endfor

         when others =>
            return (raise Bad_Type_Error with "not an enum value");
      end case;
   end Enum_Index;

   <% array_types = [t for t in ctx.array_types if t.exposed] %>

   ------------------------------
   -- Array_Element_Constraint --
   ------------------------------

   function Array_Element_Constraint
     (Kind : Array_Value_Kind) return Type_Constraint is
   begin
      case Kind is
         % for t in array_types:
            <% elt_type = t.element_type %>
            when ${t.introspection_kind} =>
               % if elt_type.is_entity_type:
                  <% node = elt_type.element_type %>
                  return (Kind      => Node_Value,
                          Node_Type => ${node.introspection_name});
               % else:
                  return (Kind => ${t.element_type.introspection_kind});
               % endif
         % endfor
      end case;
   end Array_Element_Constraint;

   ------------------
   -- Array_Length --
   ------------------

   function Array_Length (Self : Value_Type) return Natural is
   begin
      case Kind (Self) is
         % for t in array_types:
            when ${t.introspection_kind} =>
               return Self.Value.Value.${t.introspection_kind}.all'Length;
         % endfor

         when others =>
            return (raise Bad_Type_Error with "input value is not an array");
      end case;
   end Array_Length;

   -------------------
   -- Array_Element --
   -------------------

   function Array_Element
     (Self : Value_Type; Index : Positive) return Value_Type is
   begin
      case Kind (Self) is
         % for t in array_types:
            when ${t.introspection_kind} =>
               declare
                  A : ${t.api_name} renames
                     Self.Value.Value.${t.introspection_kind}.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_${t.element_type.introspection_prefix}
                    (A (Index));
               end;
         % endfor

         when others =>
            return (raise Bad_Type_Error with "input value is not an array");
      end case;
   end Array_Element;

   ------------------
   -- Create_Array --
   ------------------

   function Create_Array
     (Kind : Array_Value_Kind; Values : Value_Array) return Value_Type
   is
      Elt_Cons : constant Type_Constraint := Array_Element_Constraint (Kind);
   begin
      --  First check that all input values have the expected type

      for I in Values'Range loop
         if not Satisfies (Values (I), Elt_Cons) then
            raise Bad_Type_Error with "invalid value at index " & I'Image;
         end if;
      end loop;

      --  Then create the array to return

      case Kind is
         % for t in array_types:
            <%
               elt_type = t.element_type
               converter = f'As_{elt_type.introspection_prefix}'
            %>
            when ${t.introspection_kind} =>
               declare
                  A : ${t.api_name} (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                     % if elt_type.is_big_integer_type:
                        ## Since the public API type for big integers is
                        ## limited, we need to call its special copy primitive.
                        A (I).Set (${converter} (Values (I)));

                     % elif elt_type.is_entity_type:
                        ## If this is an array of node subclasses, we need to
                        ## perform the corresponding downcast for array
                        ## assignment.
                        <% node = elt_type.element_type %>
                        A (I) :=
                           ${converter} (Values (I))
                           % if not node.is_root_node:
                              .As_${node.entity.api_name}
                           % endif
                        ;

                     % else:
                        A (I) := ${converter} (Values (I));
                     % endif
                  end loop;
                  return Create_${t.introspection_prefix} (A);
               end;
         % endfor
      end case;
   end Create_Array;

   -------------------
   -- Struct_Fields --
   -------------------

   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Struct_Fields (Kind);
      pragma Warnings (On, "value not in range of type");
   end Struct_Fields;

   -------------------
   -- Create_Struct --
   -------------------

   pragma Warnings (Off, "referenced");
   function Create_Struct
     (Kind : Struct_Value_Kind; Values : Value_Array) return Value_Type
   is
      pragma Warnings (On, "referenced");
   begin
      % if ctx.sorted_public_structs:
         --  First check that input values have the expected format
         declare
            Fields : constant Struct_Field_Reference_Array :=
               Struct_Fields (Kind);
         begin
            if Fields'Length /= Values'Length then
               raise Bad_Type_Error with "unexpected number of values";
            end if;

            for I in Fields'Range loop
               declare
                  F : constant Struct_Field_Reference := Fields (I);
                  T : constant Type_Constraint := Member_Type (F);
                  V : Value_Type renames Values (I + Values'First - 1);
               begin
                  if not Satisfies (V, T) then
                     raise Bad_Type_Error with
                        "type mismatch for " & Image (Member_Name (F));
                  end if;
               end;
            end loop;
         end;

         --  Only then, use input values to create the struct
         case Kind is
         % for t in ctx.sorted_public_structs:
            when ${t.introspection_kind} =>
               <% fields = t.get_fields() %>
               declare
                  ## Extract $.Analysis values from the polymorphic input
                  ## values, create the $.Analysis struct and then wrap it into
                  ## the polymorphic result. The usual complications come with
                  ## the handling of nodes and entities.
                  % for i, f in enumerate(fields):
                     <%
                        value = f"Values (Values'First + {i})"
                        ftype = f.type.entity if f.type.is_ast_node else f.type
                        if ftype.is_entity_type:
                           value = 'As_Node ({})'.format(value)
                           if not ftype.element_type.is_root_node:
                              value = f'{value}.As_{ftype.api_name}'
                        else:
                           value = 'As_{} ({})'.format(ftype.api_name,
                                                       value)
                     %>
                     ## Add a "F_" prefix to helper locals to avoid name
                     ## clashes (for instance, it's commot to have a struct
                     ## field called "Values").
                     F_${f.name} : constant ${ftype.api_name} := ${value};
                  % endfor
                  Result : constant ${t.api_name} :=
                     Analysis.Create_${t.api_name}
                       (${', '.join(f"F_{f.name}" for f in fields)});
               begin
                  return Introspection.Create_${t.api_name} (Result);
               end;
         % endfor
         end case;

      % else:
         return (raise Program_Error);
      % endif
   end Create_Struct;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Member : Member_Reference) return Text_Type is
   begin
      return Impl.Member_Name (Member);
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type (Member : Member_Reference) return Type_Constraint is
   begin
      return Impl.Member_Type (Member);
   end Member_Type;

   function Eval_Member
     (Prefix    : Value_Type;
      Member    : Member_Reference;
      Arguments : Value_Array) return Value_Type
   is
      Prefix_Val : Value_Record renames Prefix.Value.Value.all;
   begin
      case Prefix_Val.Kind is
      when Struct_Value_Kind =>
         if Member not in Struct_Field_Reference then
            return (raise Bad_Type_Error with "no such member");
         elsif Arguments'Length /= 0 then
            return (raise Bad_Type_Error
                    with "struct fields take no argument");
         else
            pragma Warnings (Off, "value not in range of type");
            return Eval_Member (Prefix, Member);
            pragma Warnings (On, "value not in range of type");
         end if;

      when Node_Value =>
         return Eval_Member (Prefix_Val.Node_Value, Member, Arguments);

      when others =>
         return (raise Bad_Type_Error with "invalid prefix type");
      end case;
   end Eval_Member;

   -----------------
   -- Eval_Member --
   -----------------

   pragma Warnings (Off, "referenced");
   function Eval_Member
     (Prefix : Value_Type; Field : Struct_Field_Reference) return Value_Type
   is
      pragma Warnings (On, "referenced");
      Prefix_Val : Value_Record renames Prefix.Value.Value.all;
   begin
      case Prefix_Val.Kind is

      % for t in ctx.sorted_public_structs:
         when ${t.introspection_kind} =>
            case Field is
            % for f in t.get_fields():
               when ${f.introspection_enum_literal} =>
                  declare
                     ## Extract the $.Analysis struct value, do the actual
                     ## field access and then wrap the result into a
                     ## polymorphic value. The usual complications come with
                     ## the handling of nodes and entities.
                     <%
                        struct_value = f"Prefix_Val.{t.introspection_kind}"
                        field_value = f"Analysis.{f.name} ({struct_value})"
                        ftype = f.type.entity if f.type.is_ast_node else f.type
                        poly_constructor = (
                           "Create_Node"
                           if ftype.is_entity_type
                           else f"Create_{ftype.api_name}"
                        )
                     %>
                  begin
                     return ${poly_constructor} (${field_value});
                  end;
            % endfor

            when others => null;
            end case;
      % endfor

      when others =>
         return (raise Program_Error);
      end case;

      % if ctx.sorted_public_structs:
         return (raise Bad_Type_Error with "no such member");
      % endif
   end Eval_Member;

   -----------------
   -- Eval_Member --
   -----------------

   function Eval_Member
     (Node      : ${T.entity.api_name}'Class;
      Member    : Node_Member_Reference;
      Arguments : Value_Array) return Value_Type is
   begin
      case Member is
         when Syntax_Field_Reference =>
            if Arguments'Length > 0 then
               raise Bad_Type_Error with "fields take no argument";
            end if;
            pragma Warnings (Off, "value not in range of type");
            return Create_Node (Eval_Syntax_Field (Node, Member));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Eval_Property (Node, Member, Arguments);
      end case;
   end Eval_Member;

   -------------------
   -- Lookup_Member --
   -------------------

   function Lookup_Member
     (Prefix : Value_Type;
      Name   : Text_Type) return Any_Member_Reference
   is
      Prefix_Val : Value_Record renames Prefix.Value.Value.all;
   begin
      case Prefix_Val.Kind is
      when Struct_Value_Kind =>
         pragma Warnings (Off, "value not in range of type");
         return Impl.Lookup_Member_Struct (Prefix_Val.Kind, Name);
         pragma Warnings (On, "value not in range of type");

      when Node_Value =>
         declare
            Node : constant ${T.entity.api_name} := Prefix_Val.Node_Value;
         begin
            if Node.Is_Null then
               raise Bad_Type_Error with "invalid null prefix node";
            end if;
            return Impl.Lookup_Member_Node (Impl.Id_For_Kind (Node.Kind), Name);
         end;

      when others =>
         return (raise Bad_Type_Error with "invalid prefix type");
      end case;
   end Lookup_Member;

   function Lookup_Member
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference is
   begin
      return Impl.Lookup_Member_Node (Id, Name);
   end Lookup_Member;

   -----------------------
   -- Eval_Syntax_Field --
   -----------------------

   function Eval_Syntax_Field
     (Node  : ${T.entity.api_name}'Class;
      Field : Syntax_Field_Reference) return ${T.entity.api_name}
   is
      Ent : constant ${T.entity.name} := Unwrap_Entity (Node);

      pragma Warnings (Off, "value not in range of type");
      Result : constant ${T.root_node.name} :=
         Impl.Eval_Syntax_Field (Ent.Node, Field);
      pragma Warnings (On, "value not in range of type");
   begin
      return Wrap_Node (Result, Ent.Info);
   end Eval_Syntax_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : ${T.node_kind}; Field : Syntax_Field_Reference) return Positive is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Index (Kind, Field);
      pragma Warnings (On, "value not in range of type");
   end Index;

   ---------------------------------------
   -- Syntax_Field_Reference_From_Index --
   ---------------------------------------

   function Syntax_Field_Reference_From_Index
     (Kind : ${T.node_kind}; Index : Positive) return Syntax_Field_Reference is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Syntax_Field_Reference_From_Index (Kind, Index);
      pragma Warnings (On, "value not in range of type");
   end Syntax_Field_Reference_From_Index;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Kind : ${T.node_kind}) return Syntax_Field_Reference_Array is
   begin
      return Impl.Syntax_Fields (Kind);
   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array is
   begin
      return Impl.Syntax_Fields (Id);
   end Syntax_Fields;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array is
   begin
      return Impl.Property_Argument_Types (Property);
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type is
   begin
      return Impl.Property_Argument_Name (Property, Argument_Number);
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Any_Value_Type
   is
      Desc : Impl.Property_Descriptor renames
         Impl.Property_Descriptors (Property).all;
   begin
      Impl.Check_Argument_Number (Desc, Argument_Number);
      return From_Internal_Value
        (Desc.Argument_Default_Values (Argument_Number));
   end Property_Argument_Default_Value;

   -------------------
   -- Eval_Property --
   -------------------

   function Eval_Property
     (Node      : ${T.entity.api_name}'Class;
      Property  : Property_Reference;
      Arguments : Value_Array) return Value_Type
   is
      Kind   : constant ${T.node_kind} := Node.Kind;
      Desc   : Impl.Property_Descriptor renames
         Impl.Property_Descriptors (Property).all;
      Result : Any_Value_Type := No_Value;
   begin
      --  First, check that arguments match the property signature

      if Arguments'Length /= Desc.Arity then
         raise Bad_Type_Error with "invalid number of arguments";
      end if;

      for I in Desc.Argument_Types'Range loop
         declare
            Arg : Value_Type renames Arguments (I - 1 + Arguments'First);
         begin
            if not Satisfies (Arg, Desc.Argument_Types (I)) then
               raise Bad_Type_Error with
                  "invalid type for argument " & Desc.Argument_Names (I).all;
            end if;
         end;
      end loop;

      --  Now, we can proceed with the property evaluation

      <%
         def add_property_actions(result, node_expr, p):
            """
            Append to `result` the Ada code to call the `p` property on the
            `node_expr` node.
            """
            if p.arguments:
               # Unwrap arguments in local variables (convert Value_Type to
               # contrete types).
               result.append('declare')
               for i, arg in enumerate(p.arguments):
                  t = arg.public_type
                  result.append('{} : constant {} :='
                                .format(arg.name, t.api_name))
                  result.append("As_{} (Arguments (Arguments'First + {}))"
                                .format(t.introspection_prefix, i))

                  # Make sure we convert nodes to the precise type
                  if t.is_entity_type and t != T.entity:
                     result[-1] += '.As_{}'.format(t.api_name)

                  result[-1] += ';'

               result.append('begin')

            # Format a call to the property itself, passing arguments if there
            # are some.
            property_call = '{}.{}{}'.format(
               node_expr, p.api_name,
               (' ({})'.format(', '.join(str(arg.name) for arg in p.arguments))
                if p.arguments else ''))

            # Wrap this call to convert its result (concrete type to
            # Value_Type).
            result.append('Result := Create_{} ({});'
                          .format(p.type.introspection_prefix, property_call))

            if p.arguments:
               result.append('end;')

         def get_actions(astnode, node_expr):
            # TODO: due to what seems to be a Mako bug, we cannot create
            # "properties" with a list comprehension.
            properties = astnode.get_properties(
               predicate=lambda p: p.is_public,
               include_inherited=False)
            for i, p in enumerate(properties):
               if p.overriding:
                  properties[i] = p.base

            result = []

            if properties:
               result.append('case Property is')
               for p in properties:
                  result.append('when {} =>'
                                .format(p.introspection_enum_literal))
                  add_property_actions(result, node_expr, p)
               result.append('when others => null;')
               result.append('end case;')

            return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'Kind', get_actions,
                                           public_nodes=True)}

      ## If we haven't matched the requested field on Node, report an error,
      ## otherwise, return the property result.
      if Result = No_Value then
         raise Bad_Type_Error with "no such field on this node";
      end if;
      return Result;
   end Eval_Property;

   ----------------
   -- Properties --
   ----------------

   function Properties (Kind : ${T.node_kind}) return Property_Reference_Array
   is
   begin
      return Impl.Properties (Kind);
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
   begin
      return Impl.Properties (Id);
   end Properties;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : ${T.node_kind}) return Token_Kind is
   begin
      return Impl.Token_Node_Kind (Kind);
   end Token_Node_Kind;

end ${ada_lib_name}.Introspection;
