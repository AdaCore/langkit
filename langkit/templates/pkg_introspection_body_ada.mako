## vim: filetype=makoada

<% list_kind_range = ctx.generic_list_type.ada_kind_range_name %>

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation;

<%def name="return_program_error()">
   pragma Warnings (Off, "value not in range of type");
   return (raise Program_Error);
   pragma Warnings (On, "value not in range of type");
</%def>

package body ${ada_lib_name}.Introspection is

   ------------------------------
   -- Syntax field descriptors --
   ------------------------------

   type Syntax_Field_Descriptor (Name_Length : Natural) is record
      Field_Type : Node_Type_Id;
      Name       : String (1 .. Name_Length);
   end record;
   --  General description of a field (independent of field implementations)

   type Syntax_Field_Descriptor_Access is
      access constant Syntax_Field_Descriptor;

   --  Descriptors for syntax fields

   % for f in ctx.sorted_parse_fields:
      <% name = f._name.lower %>
      Desc_For_${f.introspection_enum_literal} : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => ${len(name)},
            Field_Type  => ${f.type.introspection_name},
            Name        => ${string_repr(name)}
         );
   % endfor

   Syntax_Field_Descriptors : constant
      array (Field_Reference) of Syntax_Field_Descriptor_Access := (
      % if ctx.sorted_parse_fields:
         ${', '.join("{name} => Desc_For_{name}'Access"
                     .format(name=f.introspection_enum_literal)
                     for f in ctx.sorted_parse_fields)}
      % else:
         Field_Reference => <>
      % endif
   );

   --------------------------
   -- Property descriptors --
   --------------------------

   type Property_Descriptor (Name_Length : Natural) is record
      Kind_First, Kind_Last : ${root_node_kind_name};
      --  Kind range for nodes that implement this property

      Name : String (1 .. Name_Length);
   end record;

   type Property_Descriptor_Access is access constant Property_Descriptor;

   --  Descriptors for properties

   % for p in ctx.sorted_properties:
      <%
         name = p._name.lower
         kind_first, kind_last = p.struct.ada_kind_range_bounds
      %>
      Desc_For_${p.introspection_enum_literal} : aliased constant
         Property_Descriptor := (
            Name_Length => ${len(name)},
            Kind_First  => ${kind_first},
            Kind_Last   => ${kind_last},
            Name        => ${string_repr(name)}
         );
   % endfor

   % if ctx.sorted_properties:
      Property_Descriptors : constant
         array (Property_Reference) of Property_Descriptor_Access := (
         % if ctx.sorted_properties:
            ${', '.join("Desc_For_{}'Access"
                        .format(p.introspection_enum_literal)
                        for p in ctx.sorted_properties)}
         % else:
            Property_Reference => <>
         % endif
      );
   % endif

   ---------------------------
   -- Node type descriptors --
   ---------------------------

   type Node_Field_Descriptor (Is_Abstract_Or_Null : Boolean) is record
      Field : Field_Reference;
      --  Reference to the field this describes

      --  Only non-null concrete fields are assigned an index

      case Is_Abstract_Or_Null is
         when False =>
            Index : Positive;
            --  Index for this field

         when True =>
            null;
      end case;
   end record;
   --  Description of a field as implemented by a specific node

   type Node_Field_Descriptor_Access is access constant Node_Field_Descriptor;
   type Node_Field_Descriptor_Array is
      array (Positive range <>) of Node_Field_Descriptor_Access;

   type Node_Type_Descriptor
     (Is_Abstract       : Boolean;
      Derivations_Count : Natural;
      Fields_Count      : Natural)
   is record
      Base_Type : Any_Node_Type_Id;
      --  Reference to the node type from which this derives

      Derivations : Node_Type_Id_Array (1 .. Derivations_Count);
      --  List of references for all node types that derives from this

      DSL_Name : Unbounded_String;
      --  Name for this type in the Langkit DSL

      Inherited_Fields : Natural;
      --  Number of syntax field inherited from the base type

      Fields : Node_Field_Descriptor_Array (1 .. Fields_Count);
      --  For regular node types, list of syntax fields that are specific for
      --  this derivation (i.e. excluding fields from the base type).

      --  Only concrete nodes are assigned a node kind

      case Is_Abstract is
         when False =>
            Kind : ${root_node_kind_name};
            --  Kind corresponding this this node type

         when True =>
            null;
      end case;
   end record;

   type Node_Type_Descriptor_Access is access constant Node_Type_Descriptor;

   --  Descriptors for node types and their syntax fields

   % for n in ctx.astnode_types:
   <%
      fields = n.get_parse_fields(include_inherited=False)

      if n.is_root_node:
         inherited_fields = []
      else:
         inherited_fields = n.base.get_parse_fields(
            predicate=lambda f: not (f.abstract or f.null),
            include_inherited=True)
   %>

   % for f in fields:
   ${f.name}_For_${n.kwless_raw_name} : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => ${f.abstract or f.null},
      Field               => ${(f.base or f).introspection_enum_literal}

      % if not f.abstract and not f.null:
         , Index => ${f.index + 1}
      % endif
   );
   % endfor

   Desc_For_${n.kwless_raw_name} : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => ${n.abstract},
      Derivations_Count => ${len(n.subclasses)},
      Fields_Count      => ${len(fields)},

      Base_Type   => ${n.base.introspection_name if n.base else 'None'},
      Derivations =>
         ${('({})'.format(', '.join(
            '{} => {}'.format(i, child.introspection_name)
            for i, child in enumerate(n.subclasses, 1)
         )) if n.subclasses else '(1 .. 0 => <>)')},

      DSL_Name => To_Unbounded_String ("${n.dsl_name}"),

      Inherited_Fields => ${len(inherited_fields)},
      Fields           => (
         % if fields:
            ${', '.join("{} => {}_For_{}'Access"
                        .format(i, f.name, n.kwless_raw_name)
                        for i, f in enumerate(fields, 1))}
         % else:
            1 .. 0 => <>
         % endif
      )

      % if not n.abstract:
      , Kind => ${n.ada_kind_name}
      % endif
   );
   % endfor

   Node_Type_Descriptors : constant
      array (Node_Type_Id) of Node_Type_Descriptor_Access
   := (${', '.join("Desc_For_{}'Access".format(n.kwless_raw_name)
                   for n in ctx.astnode_types)});

   ----------------------
   -- Various mappings --
   ----------------------

   package Node_Type_Id_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Node_Type_Id,
      Equivalent_Keys => "=",
      Hash            => Hash);

   DSL_Name_To_Node_Type : Node_Type_Id_Maps.Map;
   --  Lookup table for DSL names to node type references. Created at
   --  elaboration time and never updated after.

   Kind_To_Id : constant array (${root_node_kind_name}) of Node_Type_Id := (
      ${', '.join('{n.ada_kind_name} => {n.introspection_name}'.format(n=n)
                  for n in ctx.astnode_types
                  if not n.abstract)}
   );

   -------------
   -- Helpers --
   -------------

   function Fields
     (Id : Node_Type_Id; Concrete_Only : Boolean) return Field_Reference_Array;
   --  Return the list of fields associated to Id. If Concrete_Only is true,
   --  collect only non-null and concrete fields. Otherwise, collect all
   --  fields.

   function Kind_Matches
     (Kind       : ${root_node_kind_name};
      Descriptor : Property_Descriptor)
      return Boolean
   is (Kind in Descriptor.Kind_First .. Descriptor.Kind_Last);

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return String is
   begin
      return To_String (Node_Type_Descriptors (Id).DSL_Name);
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id is
      use Node_Type_Id_Maps;

      Position : constant Cursor :=
         DSL_Name_To_Node_Type.Find (To_Unbounded_String (Name));
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return None;
      end if;
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Node_Type_Descriptors (Id).Is_Abstract;
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return ${root_node_kind_name} is
      Desc : Node_Type_Descriptor renames Node_Type_Descriptors (Id).all;
   begin
      if Desc.Is_Abstract then
         raise Constraint_Error with "trying to get kind for abstract node";
      end if;
      return Desc.Kind;
   end Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : ${root_node_kind_name}) return Node_Type_Id is
   begin
      return Kind_To_Id (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Id = ${T.root_node.introspection_name};
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      if Is_Root_Node (Id) then
         raise Constraint_Error with "trying to get base type of root node";
      end if;
      return Node_Type_Descriptors (Id).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Node_Type_Descriptors (Id).Derivations;
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
      Cursor : Any_Node_Type_Id := Id;
   begin
      while Cursor /= None loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Node_Type_Descriptors (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Value_Type) return Value_Kind is
   begin
      return Self.Value.Value.Kind;
   end Kind;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Value_Type) return Boolean is
   begin
      return Self.Value.Value.Boolean_Value;
   end As_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Value_Type) return Integer is
   begin
      return Self.Value.Value.Integer_Value;
   end As_Integer;

   --------------------
   -- As_Big_Integer --
   --------------------

   function As_Big_Integer (Self : Value_Type) return Big_Integer is
   begin
      return Result : Big_Integer do
         Result.Set (Self.Value.Value.Big_Integer_Value);
      end return;
   end As_Big_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Value_Type) return Character_Type is
   begin
      return Self.Value.Value.Character_Value;
   end As_Character;

   --------------
   -- As_Token --
   --------------

   function As_Token (Self : Value_Type) return Token_Reference is
   begin
      return Self.Value.Value.Token_Value;
   end As_Token;

   -----------------------
   -- As_Unbounded_Text --
   -----------------------

   function As_Unbounded_Text (Self : Value_Type) return Unbounded_Text_Type is
   begin
      return Self.Value.Value.Unbounded_Text_Value;
   end As_Unbounded_Text;

   ----------------------
   -- As_Analysis_Unit --
   ----------------------

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit is
   begin
      return Self.Value.Value.Analysis_Unit_Value;
   end As_Analysis_Unit;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Value_Type) return ${root_entity.api_name} is
   begin
      return Self.Value.Value.Node_Value;
   end As_Node;

   % for enum_type in ctx.enum_types:
      function As_${enum_type.introspection_radix}
        (Self : Value_Type) return ${enum_type.api_name} is
      begin
         return Self.Value.Value.${enum_type.introspection_kind};
      end As_${enum_type.introspection_radix};

   % endfor

   % for t in ctx.composite_types:
      % if t.exposed and not t.is_entity_type:
         function As_${t.introspection_radix}
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
         end As_${t.introspection_radix};
      % endif
   % endfor

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies
     (Value : Value_Type; Constraint : Value_Constraint) return Boolean is
   begin
      if Value.Value.Value.Kind /= Constraint.Kind then
         return False;
      end if;

      case Constraint.Kind is
         when Node_Value =>
            return Is_Derived_From
              (Id_For_Kind (Value.Value.Value.Node_Value.Kind),
               Constraint.Node_Type);

         when others =>
            return True;
      end case;
   end Satisfies;

   --------------------
   -- Node_Data_Name --
   --------------------

   function Node_Data_Name
     (Node_Data : Abstract_Node_Data_Reference) return String is
   begin
      case Node_Data is
         when Field_Reference =>
            pragma Warnings (Off, "value not in range of subtype");
            return Syntax_Field_Descriptors (Node_Data).Name;
            pragma Warnings (On, "value not in range of subtype");

         when Property_Reference =>
            return Property_Descriptors (Node_Data).Name;
      end case;
   end Node_Data_Name;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Field : Field_Reference) return String is
   begin
      return Node_Data_Name (Field);
   end Field_Name;

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type (Field : Field_Reference) return Node_Type_Id is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Syntax_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Field_Type;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : ${root_node_kind_name}; Field : Field_Reference) return Positive
   is
   begin
      % if ctx.sorted_parse_fields:
         <%
            concrete_astnodes = [n for n in ctx.astnode_types
                                 if not n.abstract]
            def enum_literal(f):
               return (f.overriding or f).introspection_enum_literal
         %>
         case Kind is
            % for n in concrete_astnodes:
               when ${n.ada_kind_name} =>
               return (case Field is
                       % for f in n.get_parse_fields( \
                          predicate=lambda f: not f.null \
                       ):
                       when ${enum_literal(f)} => ${f.index + 1},
                       % endfor
                       when others => raise Constraint_Error);
            % endfor
         end case;

      % else:
         return (raise Program_Error);
      % endif
   end Index;

   --------------------------------
   -- Field_Reference_From_Index --
   --------------------------------

   function Field_Reference_From_Index
     (Kind : ${root_node_kind_name}; Index : Positive) return Field_Reference
   is
   begin
      <%
         def get_actions(astnode, node_expr):
            fields = astnode.get_parse_fields(
               predicate=lambda f: not f.abstract and not f.null,
               include_inherited=False
            )
            result = []

            # List types have no field, so just raise an error if a list kind
            # is passed.
            if astnode.is_generic_list_type:
               result.append(
                  'raise Invalid_Field with "List AST nodes have no field";'
               )
            elif astnode.is_list:
               pass

            # Otherwise, dispatch on the index to return the corresponding
            # field enum value.
            elif fields:
               result.append('case Index is')
               for f in fields:
                  result.append('when {} => return {};'.format(
                     f.index + 1,
                     (f.overriding or f).introspection_enum_literal
                  ))
               result.append('when others => null;')
               result.append('end case;')

            return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy(None, 'Kind', get_actions)}

      pragma Warnings (Off, "value not in range of type");
      return (raise Invalid_Field with "Index is out of bounds");
      pragma Warnings (On, "value not in range of type");
   end Field_Reference_From_Index;

   ------------
   -- Fields --
   ------------

   function Fields (Kind : ${root_node_kind_name}) return Field_Reference_Array
   is
   begin
      % if ctx.sorted_parse_fields:
         return Fields (Id_For_Kind (Kind), Concrete_Only => True);
      % else:
         ${return_program_error()}
      % endif
   end Fields;

   ------------
   -- Fields --
   ------------

   function Fields
     (Id : Node_Type_Id; Concrete_Only : Boolean) return Field_Reference_Array
   is
      Cursor : Any_Node_Type_Id := Id;

      Added_Fields : array (Field_Reference) of Boolean := (others => False);
      --  Set of field references that were added to Result

      Result : Field_Reference_Array (1 .. Added_Fields'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin
      % if ctx.sorted_parse_fields:

         --  Go through the derivation chain for Id and collect fields. Do
         --  it in reverse order as we process base types last.
         while Cursor /= None loop
            declare
               Node_Desc : Node_Type_Descriptor renames
                  Node_Type_Descriptors (Cursor).all;
            begin
               for Field_Index in reverse Node_Desc.Fields'Range loop
                  declare
                     Field_Desc : Node_Field_Descriptor renames
                        Node_Desc.Fields (Field_Index).all;
                     Field      : Field_Reference renames Field_Desc.Field;
                  begin
                     --  Abstract fields share the same Field_Reference value
                     --  with the corresponding concrete fields, so collect
                     --  fields only once. We process fields in reverse order,
                     --  so we know that concrete ones will be processed before
                     --  the abstract fields they override.
                     if not (Concrete_Only
                             and then Field_Desc.Is_Abstract_Or_Null)
                        and then not Added_Fields (Field)
                     then
                        Added_Fields (Field) := True;
                        Last := Last + 1;
                        Result (Last) := Field;
                     end if;
                  end;
               end loop;
               Cursor := Node_Desc.Base_Type;
            end;
         end loop;

         --  At this point, Result contains elements in the opposite order as
         --  expected, so reverse it.

         for I in 1 .. Last / 2 loop
            declare
               Other_I : constant Positive := Last - I + 1;
               Swap    : constant Field_Reference := Result (I);
            begin
               Result (I) := Result (Other_I);
               Result (Other_I) := Swap;
            end;
         end loop;

         return Result (1 .. Last);

      % else:
         ${return_program_error()}
      % endif
   end Fields;

   ------------
   -- Fields --
   ------------

   function Fields (Id : Node_Type_Id) return Field_Reference_Array is
   begin
      return Fields (Id, Concrete_Only => False);
   end Fields;

   % if ctx.sorted_properties:

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return String is
   begin
      return Node_Data_Name (Property);
   end Property_Name;

   ----------------
   -- Properties --
   ----------------

   function Properties
     (Kind : ${root_node_kind_name}) return Property_Reference_Array
   is
      Count : Natural := 0;
   begin
      --  Count how many properties we will return
      for Desc of Property_Descriptors loop
         if Kind_Matches (Kind, Desc.all) then
            Count := Count + 1;
         end if;
      end loop;

      --  Now create the result array and fill it
      return Result : Property_Reference_Array (1 .. Count) do
         declare
            Next : Positive := 1;
         begin
            for Property in Property_Descriptors'Range loop
               if Kind_Matches (Kind, Property_Descriptors (Property).all) then
                  Result (Next) := Property;
                  Next := Next + 1;
               end if;
            end loop;
         end;
      end return;
   end Properties;

   % endif

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind
     (Kind : ${root_node_kind_name}) return Token_Kind
   is
      <% token_nodes = [n for n in ctx.astnode_types
                        if not n.abstract and n.is_token_node] %>
   begin
      % if ctx.generate_unparser:
         case Kind is
            % for n in token_nodes:
               when ${n.ada_kind_name} =>
                  return ${n.token_kind.ada_name};
            % endfor

            when others =>
               --  Kind is not a token node, and thus the precondition does not
               --  hold.
               return (raise Program_Error);
         end case;

      % else:
         pragma Unreferenced (Kind);
         return (raise Program_Error);
      % endif
   end Token_Node_Kind;

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

begin
   for D in Node_Type_Descriptors'Range loop
      DSL_Name_To_Node_Type.Insert
        (Node_Type_Descriptors (D).DSL_Name, D);
   end loop;
end ${ada_lib_name}.Introspection;
