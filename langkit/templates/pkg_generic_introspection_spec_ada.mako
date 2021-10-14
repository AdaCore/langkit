## vim: filetype=makoada

with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;
with Langkit_Support.Text; use Langkit_Support.Text;

--  This package provides description tables to enable the generic
--  introspection API in Langkit_Support to work with this Langkit-generated
--  library.

private package ${ada_lib_name}.Generic_Introspection is

   <%
      # Expose through the introspection API...
      enum_types = ctx.enum_types

      # All exposed array types
      array_types = [t for t in ctx.array_types if t.exposed]

      # All exposed struct types (except entities for now: see below)
      struct_types = [t for t in ctx.struct_types
                      if t.exposed and not t.is_entity_type]

      # Include all entity types (including abstract ones), in hierarchical
      # order.
      node_types = ctx.astnode_types
      entity_types = [t.entity for t in node_types]

      all_types = enum_types + array_types + struct_types + entity_types

      # Fetch all exposed types that are not listed above (builtin types)
      other_types = [t for t in T.all_types
                     if t.exposed and not t.is_ast_node and t not in all_types]
      all_types = sorted(other_types, key=lambda t: t.api_name) + all_types

      def type_name(t):
         """
         Return a short type name for ``t``, to be used in the generation of
         constants.
         """
         if t.is_ast_node:
            t = t.entity

         return t.api_name.camel_with_underscores

      def type_index(t):
         """
         Return the name of the constant for ``t``'s type index, or
         ``No_Type_Index`` if ``t`` is None.

         For convenience, also automatically handle bare nodes as entities
         (bare nodes are not exposed).
         """
         return (
            "No_Type_Index"
            if t is None
            else f"Type_Index_For_{type_name(t)}"
         )

      # We also need to expose all base struct members: struct fields, node
      # syntax fields and properties.
      all_members = (
         ctx.sorted_struct_fields
         + ctx.sorted_parse_fields
         + ctx.sorted_properties
      )

      def root_member(m):
         # In the case of overriding node members (abstract syntax fields,
         # overriding properties), we emit a single member descriptor for the
         # whole derivation tree, so always refer to the root member.
         if m.struct.is_ast_node:
            while True:
               base = m.base
               if base is None:
                  break
               m = base
         return m

      def member_name(m):
         """
         Return a unique name for the ``m`` struct member.
         """
         m = root_member(m)
         # Node members are already qualified by the node type name, so we need
         # to add the type name only for structs.
         return (
            m.name
            if m.struct.is_ast_node
            else f"{type_name(m.struct)}_{m.name}"
         )

      def member_index(m):
         """
         Return the name of the constant for ``m``'s struct member index.
         """
         return f"Member_Index_For_{member_name(m)}"
   %>

   --------------------------
   -- Type index constants --
   --------------------------

   % for i, t in enumerate(all_types, 1):
      ${type_index(t)} : constant Type_Index := ${i};
   % endfor

   ----------------------------
   -- Member index constants --
   ----------------------------

   % for i, m in enumerate(all_members, 1):
      ${member_index(m)} : constant Struct_Member_Index := ${i};
   % endfor

   ------------------------------------
   -- General value type descriptors --
   ------------------------------------

   <% type_descs = [] %>
   % for t in all_types:
      <%
         desc_const = f"Desc_For_{t.name}"
         debug_name_const = f"Debug_Name_For_{t.name}"
         type_descs.append(f"{desc_const}'Access")
      %>
      ${debug_name_const} : aliased constant String :=
        ${ascii_repr(t.dsl_name)};
      ${desc_const} : aliased constant Type_Descriptor :=
        (Debug_Name => ${debug_name_const}'Access);
   % endfor

   Types : aliased constant Type_Descriptor_Array := (
      ${",\n".join(type_descs)}
   );

   ---------------------------
   -- Enum type descriptors --
   ---------------------------

   <% enum_type_descs = [] %>
   % for t in enum_types:
      <%
         desc_const = f"Enum_Desc_For_{t.name}"
         name_const = f"Enum_Name_For_{t.name}"
         values_const = f"Enum_Values_Name_For_{t.name}"
         enum_type_descs.append(f"{type_index(t)} => {desc_const}'Access")
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

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   <%
      array_type_descs = [
         f"{type_index(t)} => (Element_Type => {type_index(t.element_type)})"
         for t in array_types
      ]
   %>
   Array_Types : aliased constant Array_Type_Descriptor_Array := (
      ${",\n".join(array_type_descs)}
   );

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   <%
      member_descs = []
      arg_no = 1
   %>
   % for m in all_members:
      <%
         name = member_name(m)
         desc_name = f"Member_Desc_For_{name}"
         name_const = f"Member_Name_For_{name}"
         member_descs.append(f"{member_index(m)} => {desc_name}'Access")
         args = []
      %>

      % for i, arg in enumerate(m.arguments, 1):
         Arg_Name_${arg_no} : aliased constant Text_Type :=
           ${text_repr(arg.name.camel_with_underscores)};
         <%
            args.append(
               f"{arg_no} => (Name => Arg_Name_{arg_no}'Access,"
               f" Argument_Type => {type_index(arg.type)})"
            )
            arg_no += 1
         %>
      % endfor

      ${name_const} : aliased constant Text_Type :=
        ${text_repr(m.name.camel_with_underscores)};
      ${desc_name} : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => ${len(args)},
         Name          => ${name_const}'Access,
         Member_Type   => ${type_index(m.type)},
         Arguments     => (
            % if m.arguments:
               ${",\n".join(args)}
            % else:
               1 .. 0 => <>
            % endif
        ));

   % endfor

   Struct_Members : aliased constant Struct_Member_Descriptor_Array := (
      ${",\n".join(member_descs)}
   );

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   <% struct_type_descs = [] %>
   % for t in struct_types + node_types:
      <%
         if t.is_ast_node:
            name = t.kwless_raw_name
            base = t.base
            abstract = t.abstract
            subclasses = t.subclasses
         else:
            name = t.api_name
            base = None
            abstract = False
            subclasses = []
         desc_const = f"Node_Desc_For_{name}"
         name_const = f"Node_Name_For_{name}"
         struct_type_descs.append(f"{type_index(t)} => {desc_const}'Access")

         def get_members(include_inherited):
            result = (
               t.get_parse_fields(include_inherited=include_inherited)
               if t.is_ast_node
               else t.get_fields()
            )
            return result + t.get_properties(
               predicate=lambda p: p.is_public and not p.overriding,
               include_inherited=include_inherited
            )

         inherited_members = get_members(True)
         members = get_members(False)
      %>
      ${name_const} : aliased constant Text_Type :=
        ${text_repr(name.camel_with_underscores)};
      ${desc_const} : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => ${len(subclasses)},
         Member_Count      => ${len(members)},
         Base_Type         => ${type_index(base)},
         Is_Abstract       => ${abstract},
         Name              => ${name_const}'Access,
         Inherited_Members => ${len(inherited_members)},
         Derivations       => (
           % if subclasses:
             ${",\n".join(f"{i} => {type_index(sc)}"
                         for i, sc in enumerate(subclasses, 1))}
           % else:
             1 .. 0 => <>
           % endif
         ),
         Members           => (
            % if members:
              ${",\n".join(f"{i} => {member_index(m)}"
                           for i, m in enumerate(members, 1))}
            % else:
              1 .. 0 => <>
            % endif
         ));
   % endfor

   Struct_Types : aliased constant Struct_Type_Descriptor_Array := (
      ${",\n".join(struct_type_descs)}
   );

   First_Node     : constant Type_Index := ${type_index(node_types[0])};
   First_Property : constant Struct_Member_Index :=
     ${member_index(ctx.sorted_properties[0])};

end ${ada_lib_name}.Generic_Introspection;
