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

      def type_index(t):
         """
         Return the name of the constant for ``t``'s type index, or
         ``No_Value_Type`` if ``t`` is None.

         For convenience, also automatically handle bare nodes as entities
         (bare nodes are not exposed).
         """
         if t is None:
            return "No_Value_Type"

         if t.is_ast_node:
            t = t.entity

         return f"Index_For_{t.name}"
   %>

   --------------------------
   -- Type index constants --
   --------------------------

   % for i, t in enumerate(all_types, 1):
      ${type_index(t)} : constant Value_Type := ${i};
   % endfor

   ------------------------------------
   -- General value type descriptors --
   ------------------------------------

   <% value_type_descs = [] %>
   % for t in all_types:
      <%
         desc_const = f"Desc_For_{t.name}"
         debug_name_const = f"Debug_Name_For_{t.name}"
         value_type_descs.append(f"{desc_const}'Access")
      %>
      ${debug_name_const} : aliased constant String :=
        ${ascii_repr(t.dsl_name)};
      ${desc_const} : aliased constant Value_Type_Descriptor :=
        (Debug_Name => ${debug_name_const}'Access);
   % endfor

   Value_Types : aliased constant Value_Type_Descriptor_Array := (
      ${",\n".join(value_type_descs)}
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

   ---------------------------
   -- Node type descriptors --
   ---------------------------

   <% node_type_descs = [] %>
   % for n in node_types:
      <%
         desc_const = f"Node_Desc_For_{n.kwless_raw_name}"
         name_const = f"Node_Name_For_{n.kwless_raw_name}"
         node_type_descs.append(f"{type_index(n)} => {desc_const}'Access")
      %>
      ${name_const} : aliased constant Text_Type :=
        ${text_repr(n.kwless_raw_name.camel_with_underscores)};
      ${desc_const} : aliased constant Node_Type_Descriptor :=
        (Derivations_Count => ${len(n.subclasses)},
         Base_Type         => ${type_index(n.base)},
         Is_Abstract       => ${n.abstract},
         Name              => ${name_const}'Access,
         Derivations       => (
           % if n.subclasses:
             ${",\n".join(f"{i} => {type_index(n)}"
                         for i, n in enumerate(n.subclasses, 1))}
           % else:
             1 .. 0 => <>
           % endif
         ));
   % endfor

   Node_Types : aliased constant Node_Type_Descriptor_Array := (
      ${",\n".join(node_type_descs)}
   );

end ${ada_lib_name}.Generic_Introspection;
