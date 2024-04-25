## vim: filetype=makoada

with Ada.Unchecked_Deallocation;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
pragma Warnings (Off, "referenced");
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
pragma Warnings (On, "referenced");
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;
with Langkit_Support.Text;        use Langkit_Support.Text;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

--  This package provides description tables to enable the generic
--  introspection API in Langkit_Support to work with this Langkit-generated
--  library.

private package ${ada_lib_name}.Generic_Introspection is

   <%
      G = generic_api
      node_types = ctx.astnode_types
      all_types = G.all_types
      all_members = G.all_members
   %>

   --------------------------
   -- Type index constants --
   --------------------------

   % for i, t in enumerate(all_types, 1):
      ${G.type_index(t)} : constant Type_Index := ${i};
   % endfor

   ----------------------------
   -- Member index constants --
   ----------------------------

   % for i, m in enumerate(all_members, 1):
      ${G.member_index(m)} : constant Struct_Member_Index := ${i};
   % endfor

   --------------------------------
   -- Token kind index constants --
   --------------------------------

   % for i, t in enumerate(ctx.lexer.sorted_tokens, 1):
      ${G.token_kind_index(t)} : constant Token_Kind_Index := ${i};
   % endfor

   ----------------------------------
   -- Token family index constants --
   ----------------------------------

   % for i, t in enumerate(ctx.lexer.tokens.token_families, 1):
      ${G.token_family_index(t)} : constant Token_Family_Index := ${i};
   % endfor

   ------------------------------
   -- Grammar rule descriptors --
   ------------------------------

   <%
      rule_descs = []
      indexes = ctx.grammar.user_defined_rules_indexes
   %>
   % for i, name in enumerate(ctx.grammar.user_defined_rules, 1):
      <%
         rule = ctx.grammar.rules[name]
         desc_const = f"Rule_Desc_{i}"
         name_const = f"Rule_Name_{i}"
         doc_const = f"Rule_Doc_{i}"
         rule_descs.append(f"{indexes[name]} => {desc_const}'Access")
      %>
      ${name_const} : aliased constant Text_Type :=
        ${text_repr(names.Name.from_lower(name).camel_with_underscores)};
      ${doc_const} : aliased constant Text_Type :=
        ${text_repr(ctx.grammar.user_defined_rules_docs[name])};
      ${desc_const} : aliased constant Grammar_Rule_Descriptor :=
        (Name        => ${name_const}'Access,
         Is_Public   => ${name in ctx.grammar.entry_points},
         Doc         => ${doc_const}'Access,
         Return_Type => ${G.type_index(rule.type.entity)});
   % endfor

   Grammar_Rules : aliased constant Grammar_Rule_Descriptor_Array := (
      ${",\n".join(rule_descs)}
   );

   ------------------------------------
   -- General value type descriptors --
   ------------------------------------

   <% type_descs = [] %>
   % for t in all_types:
      <%
         desc_const = f"Desc_For_{t.name}"
         debug_name_const = f"Debug_Name_For_{t.name}"
         type_descs.append(f"{desc_const}'Access")

         # Do not include ".entity" for entity types, as we do not expose bare
         # nodes.
         def debug_name(t):
            if t.is_entity_type:
               return debug_name(t.element_type)
            elif t.is_array_type:
               return f"{debug_name(t.element_type)}.array"
            elif t.is_iterator_type:
               return f"{debug_name(t.element_type)}.iterator"
            else:
               return t.dsl_name

         def category(t):
            if t.is_analysis_unit_type:
               return "Analysis_Unit_Category"
            elif t.is_big_int_type:
               return "Big_Int_Category"
            elif t.is_bool_type:
               return "Bool_Category"
            elif t.is_character_type:
               return "Char_Category"
            elif t.is_int_type:
               return "Int_Category"
            elif t == T.SourceLocation:
               return "Source_Location_Category"
            elif t == T.SourceLocationRange:
               return "Source_Location_Range_Category"
            elif t.is_string_type:
               return "String_Category"
            elif t.is_token_type:
               return "Token_Category"
            elif t.is_symbol_type:
               return "Symbol_Category"
            elif t.is_enum_type:
               return "Enum_Category"
            elif t.is_array_type:
               return "Array_Category"
            elif t.is_iterator_type:
               return "Iterator_Category"
            elif t.is_base_struct_type:
               return "Struct_Category"
            else:
               assert False, f"unhandled type: {t}"
      %>
      ${debug_name_const} : aliased constant String :=
        ${ascii_repr(debug_name(t))};
      ${desc_const} : aliased constant Type_Descriptor :=
        (Category   => ${category(t)},
         Debug_Name => ${debug_name_const}'Access);
   % endfor

   Types : aliased constant Type_Descriptor_Array := (
      ${",\n".join(type_descs)}
   );

   ---------------------------
   -- Enum type descriptors --
   ---------------------------

   <% enum_type_descs = [] %>
   % for t in G.enum_types:
      <%
         desc_const = f"Enum_Desc_For_{t.name}"
         name_const = f"Enum_Name_For_{t.name}"
         values_const = f"Enum_Values_Name_For_{t.name}"
         enum_type_descs.append(f"{G.type_index(t)} => {desc_const}'Access")
      %>

      ## Output descriptors for each enum value
      % for i, value in enumerate(t.values, 1):
         ${name_const}_${i} : aliased constant Text_Type :=
           ${text_repr(value.name.camel_with_underscores)};
      % endfor

      ## Output descriptors for the enum type itself
      ${name_const} : aliased constant Text_Type :=
        ${text_repr(t.api_name.camel_with_underscores)};
      ${desc_const} : aliased constant Enum_Type_Descriptor := (
         Last_Value    => ${len(t.values)},
         Name          => ${name_const}'Access,
         Default_Value => ${(t.values_dict[t.default_val_name].index + 1
                             if t.default_val_name
                             else 0)},
         Value_Names   => (
            ${",\n".join(f"{i} => {name_const}_{i}'Access"
                         for i, value in enumerate(t.values, 1))}
         )
      );
   % endfor
   Enum_Types : aliased constant Enum_Type_Descriptor_Array := (
      ${",\n".join(enum_type_descs)}
   );

   ------------------------------------
   -- Introspection values for enums --
   ------------------------------------

   % for t in G.enum_types:
      <% vt = G.internal_value_type(t) %>
      type ${vt} is new Base_Internal_Enum_Value with record
         Value : ${t.api_name};
      end record;
      type ${G.internal_value_access(t)} is access all ${vt};

      overriding function "=" (Left, Right : ${vt}) return Boolean;
      overriding function Type_Of (Value : ${vt}) return Type_Index;
      overriding function Image (Value : ${vt}) return String;
      overriding function Value_Index (Value : ${vt}) return Enum_Value_Index;
   % endfor

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access;
   --  Implementation of the Create_Enum operation in the lanugage descriptor

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   <%
      array_type_descs = [
         f"{G.type_index(t)}"
         f" => (Element_Type => {G.type_index(t.element_type)})"
         for t in G.array_types
      ]
   %>
   Array_Types : aliased constant Array_Type_Descriptor_Array := (
      ${",\n".join(array_type_descs)}
   );

   -------------------------------------
   -- Introspection values for arrays --
   -------------------------------------

   % for t in G.array_types:
      <%
         vt = G.internal_value_type(t)
         at = G.array_access_type(t)
      %>

      type ${at} is access all ${t.api_name};
      procedure Free is new Ada.Unchecked_Deallocation (${t.api_name}, ${at});

      type ${vt} is new Base_Internal_Array_Value with record
         Value : ${at};
      end record;
      type ${G.internal_value_access(t)} is access all ${vt};

      overriding function "=" (Left, Right : ${vt}) return Boolean;
      overriding procedure Destroy (Value : in out ${vt});
      overriding function Type_Of (Value : ${vt}) return Type_Index;
      overriding function Array_Length (Value : ${vt}) return Natural;
      overriding function Array_Item
        (Value : ${vt}; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return ${G.internal_value_access(t)};
   % endfor

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation of the Create_Array operation in the language descriptor

   -------------------------------
   -- Iterator type descriptors --
   -------------------------------

   <%
      iterator_type_descs = [
         f"{G.type_index(t)}"
         f" => (Element_Type => {G.type_index(t.element_type)})"
         for t in G.iterator_types
      ]
   %>
   Iterator_Types : aliased constant Iterator_Type_Descriptor_Array := (
      % if iterator_type_descs:
         ${",\n".join(iterator_type_descs)}
      % else:
         1 .. 0 => <>
      % endif
   );

   % for t in G.iterator_types:
      <% vt = G.internal_value_type(t) %>

      type ${vt} is new Base_Internal_Iterator_Value with record
         Value : ${t.api_name};
      end record;
      type ${G.internal_value_access(t)} is access all ${vt};

      overriding function "=" (Left, Right : ${vt}) return Boolean;
      overriding function Type_Of (Value : ${vt}) return Type_Index;
      overriding function Next (Value : ${vt}) return Internal_Value_Access;
   % endfor

   --------------------------------------
   -- Introspection values for structs --
   --------------------------------------

   % for t in G.struct_types:
      <% vt = G.internal_value_type(t) %>

      type ${vt} is new Base_Internal_Struct_Value with record
         Value : ${t.api_name};
      end record;
      type ${G.internal_value_access(t)} is access all ${vt};

      overriding function "=" (Left, Right : ${vt}) return Boolean;
      overriding function Type_Of (Value : ${vt}) return Type_Index;
      overriding function Eval_Member
        (Value  : ${vt};
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return ${G.internal_value_access(t)};
   % endfor

   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation for the Create_Struct operation in the language
   --  descriptor.

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   <%
      member_descs = []
      arg_no = 1
   %>
   % for m in all_members:
      <%
         name = G.member_name(m)
         desc_name = f"Member_Desc_For_{name}"
         name_const = f"Member_Name_For_{name}"
         member_descs.append(f"{G.member_index(m)} => {desc_name}'Access")
         args = []
      %>

      % for i, arg in enumerate(m.arguments, 1):
         Arg_Name_${arg_no} : aliased constant Text_Type :=
           ${text_repr(arg.name.camel_with_underscores)};
         <%
            val = arg.default_value
            if val is None:
               val_expr = "(Kind => None)"
            elif val.type.is_bool_type:
               val_expr = (
                  f"(Kind => Boolean_Value, Boolean_Value => {val.value})"
               )
            elif val.type.is_int_type:
               val_expr = (
                  f"(Kind => Integer_Value, Integer_Value => {val.value})"
               )
            elif val.type.is_character_type:
               val_expr = (
                  f"(Kind            => Character_Value,"
                  f" Character_Value => {val.ada_value})"
               )
            elif val.type.is_enum_type:
               # The EnumValue.index field is 0-based, but the introspection
               # descriptor tables expect 1-based indexes.
               val_expr = (
                  f"(Kind       => Enum_Value,"
                  f" Enum_Type  => {G.type_index(val.type)},"
                  f" Enum_Value => {val.value.index + 1})"
               )
            else:
               assert val.type.public_type.is_entity_type
               val_expr = "(Kind => Null_Node_Value)"

            args.append(
               f"{arg_no} =>\n"
               + ada_block_with_parens(
                  [
                     f"Name          => Arg_Name_{arg_no}'Access",
                     f"Argument_Type => {G.type_index(arg.type)}",
                     f"Default_Value => {val_expr}",
                  ],
                  12,
                  indent_first=True,
               )
            )
            arg_no += 1
         %>
      % endfor

      ## If this member is a syntax field that is always null for some owning
      ## node, emit a table that describes when it is null.
      <%
         # Name for the constant array that describes the null fields, or None
         # if there are no null fields.
         null_for_const = None

         # If we generate such an array, associations to initialize it
         null_for_assocs = {}

         # Determine the set of null fields in m's derivations. Note that only
         # derived fields can be null, so if m is concrete, we know that it is
         # never null.
         null_fields = set()
         if isinstance(m, Field) and m.abstract:
            null_fields = {f for f in m.concrete_overridings if f.null}

         # If there is at least one null field, generate the array that
         # describes them.
         if null_fields:
            null_for_const = f"Null_For_{name}"

            # Initialize null_for_assocs for all nodes that have the "f" field
            def add(node):
               null_for_assocs[node] = False
               for child in node.subclasses:
                  add(child)
            add(m.struct)

            # Set the flag to True for all concrete node for which this field
            # is null.
            for f in null_fields:
               for t in TypeSet(types={f.struct}).matched_types:
                  null_for_assocs[t] = True
      %>
      % if null_for_const:
         ${null_for_const} : aliased constant Type_Flags := (${",\n".join(
            f"{G.type_index(node)} => {value}"
            for node, value in null_for_assocs.items()
         )});
      % endif

      ## If this member is a syntax field, emit a table that provides its index
      ## for each node that has it as a concrete and non-null field.
      <%
         # Name for the constant array, or None if this is not a syntax field
         indexes_const = None

         # Mapping from node types that have this field to the corresponding
         # indexes, or to 0 if this field is abstract or null.
         index_assocs = {}

         if isinstance(m, Field):
            indexes_const = f"Indexes_For_{name}"
            for t in m.struct.type_set:
               # Determine the index for the "m" syntax field in the "t"
               # concrete node.
               index = 0
               if not t.abstract:
                  t_fields = t.get_parse_fields(include_inherited=True)
                  current_index = 0
                  found = False
                  for f in t_fields:
                     # "t" is a concrete node, so it should not have any
                     # abstract syntax field.
                     assert not f.abstract

                     # Since null fields do not have an index, we must skip
                     # them for index computation.
                     if not f.null:
                        current_index += 1

                     if f.root == m:
                        found = True
                        break

                  # "t" is a concrete node, so it should not have any abstract
                  # syntax field.
                  assert found
                  if not f.null:
                     index = current_index

               index_assocs[t] = index
      %>
      % if indexes_const:
         ${indexes_const} : aliased constant Syntax_Field_Indexes :=
           (${",\n".join(
              f"{G.type_index(node)} => {value}"
              for node, value in index_assocs.items()
           )});
      % endif

      ${name_const} : aliased constant Text_Type :=
        ${text_repr(m.api_name.camel_with_underscores)};
      ${desc_name} : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => ${len(args)},
         Name          => ${name_const}'Access,
         Owner         => ${G.type_index(m.struct)},
         Member_Type   => ${G.type_index(m.type)},
         Null_For      => ${(
            "null" if null_for_const is None else f"{null_for_const}'Access"
         )},
         Indexes       => ${(
            "null" if indexes_const is None else f"{indexes_const}'Access"
         )},
         Arguments     =>
         % if m.arguments:
         ${ada_block_with_parens(args, 9)}
         % else:
           (1 .. 0 => <>)
         % endif
        );

   % endfor

   Struct_Members : aliased constant Struct_Member_Descriptor_Array :=
   ${ada_block_with_parens(member_descs, 3)};

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   <% struct_type_descs = [] %>
   % for t in G.struct_types + node_types:
      <%
         if t.is_ast_node:
            name = t.kwless_raw_name
            repr_name = t.repr_name()
            base = t.base
            abstract = t.abstract
            token_node = t.is_token_node
            token_node_kind = (
               G.token_kind_index(t.token_kind)
               if ctx.generate_unparser and token_node else
               "No_Token_Kind_Index"
            )
            list_node = t.is_list_type
            subclasses = t.subclasses
         else:
            name = t.api_name
            repr_name = None
            base = None
            abstract = False
            token_node = False
            token_node_kind = "No_Token_Kind_Index"
            list_node = False
            subclasses = []
         desc_const = f"Node_Desc_For_{name}"
         name_const = f"Node_Name_For_{name}"
         repr_name_const = (
            None if repr_name is None else f"Node_Repr_Name_For_{name}"
         )
         struct_type_descs.append(f"{G.type_index(t)} => {desc_const}'Access")

         def get_members(include_inherited):
            # Include all parse fieds (for node) and regular fields (for
            # structs): they are all public.
            fields = (
               t.get_parse_fields(include_inherited=include_inherited)
               if t.is_ast_node
               else t.get_fields()
            )

            # For properties, it is a bit more nuanced...
            all_properties = t.get_properties(
               include_inherited=include_inherited
            )
            properties = []
            for p in all_properties:
               # If "p" is overriding, get its dispatcher, which acts as the
               # reference property for the whole property derivation tree in
               # the introspection API.
               p = p.dispatcher or p
               if p in properties:
                  continue

               # Exclude private properties, and also inherited properties if
               # only t's own members are requested.
               if p.is_public and (include_inherited or p.struct == t):
                  properties.append(p)

            return fields + properties

         inherited_members = get_members(True)
         members = get_members(False)
      %>
      ${name_const} : aliased constant Text_Type :=
        ${text_repr(name.camel_with_underscores)};
      % if repr_name is not None:
         ${repr_name_const} : aliased constant Text_Type :=
           ${text_repr(repr_name)};
      % endif
      ${desc_const} : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => ${len(subclasses)},
         Member_Count      => ${len(members)},
         Base_Type         => ${G.type_index(base)},
         Is_Abstract       => ${abstract},
         Is_Token_Node     => ${token_node},
         Token_Node_Kind   => ${token_node_kind},
         Is_List_Node      => ${list_node},
         Name              => ${name_const}'Access,
         Repr_Name         => ${(
            "null" if repr_name is None else f"{repr_name_const}'Access"
         )},
         Inherited_Members => ${len(inherited_members)},
         Derivations       => (
           % if subclasses:
             ${",\n".join(f"{i} => {G.type_index(sc)}"
                         for i, sc in enumerate(subclasses, 1))}
           % else:
             1 .. 0 => <>
           % endif
         ),
         Members           => (
            % if members:
              ${",\n".join(f"{i} => {G.member_index(m)}"
                           for i, m in enumerate(members, 1))}
            % else:
              1 .. 0 => <>
            % endif
         ));
   % endfor

   Struct_Types : aliased constant Struct_Type_Descriptor_Array := (
      ${",\n".join(struct_type_descs)}
   );

   First_Node     : constant Type_Index := ${G.type_index(node_types[0])};
   First_Property : constant Struct_Member_Index :=
     ${G.member_index(ctx.sorted_properties[0])};

   function Eval_Node_Member
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation for the Eval_Node_Member operation in the language
   --  descriptor.

   Builtin_Types : aliased constant Builtin_Types_Record :=
     (Analysis_Unit         => ${G.type_index(T.AnalysisUnit)},
      Big_Int               => ${G.type_index(T.BigInt)},
      Bool                  => ${G.type_index(T.Bool)},
      Char                  => ${G.type_index(T.Character)},
      Int                   => ${G.type_index(T.Int)},
      Source_Location       => ${G.type_index(T.SourceLocation)},
      Source_Location_Range => ${G.type_index(T.SourceLocationRange)},
      String                => ${G.type_index(T.String)},
      Token                 => ${G.type_index(T.Token)},
      Symbol                => ${G.type_index(T.Symbol)});

   Node_Kinds : constant array (${T.node_kind}) of Type_Index :=
   ${ada_block_with_parens(
      [
         f"{n.ada_kind_name} => {G.type_index(n)}"
         for n in ctx.astnode_types
         if not n.abstract
      ],
      3,
   )};
   --  Associate a type index to each concrete node

   -----------------------------------------------------
   --  Getter/setter helpers for introspection values --
   -----------------------------------------------------

   --  These helpers factorize common code needed in array/struct generic
   --  access/construction operations.

   procedure Set_Unit
     (Intr_Value   : ${G.internal_value_access(T.AnalysisUnit)};
      Actual_Value : ${T.AnalysisUnit.api_name});

   function Get_Unit
     (Intr_Value : ${G.internal_value_type(T.AnalysisUnit)})
      return ${T.AnalysisUnit.api_name};

   procedure Set_Big_Int
     (Intr_Value   : ${G.internal_value_access(T.BigInt)};
      Actual_Value : ${T.BigInt.api_name});

   procedure Get_Big_Int
     (Intr_Value   : ${G.internal_value_type(T.BigInt)};
      Actual_Value : out ${T.BigInt.api_name});

   procedure Set_Node
     (Intr_Value   : ${G.internal_value_access(T.entity)};
      Actual_Value : ${T.entity.api_name}'Class);

   function Get_Node
     (Intr_Value : ${G.internal_value_type(T.entity)})
      return ${T.entity.api_name};

end ${ada_lib_name}.Generic_Introspection;
