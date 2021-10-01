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
      all_types = (
         # All enum types
         ctx.enum_types

         # All exposed array types
         + [t for t in ctx.array_types if t.exposed]

         # All exposed struct types (except entities for now: see below)
         + [t for t in ctx.struct_types if t.exposed and not t.is_entity_type]

         # Include all entity types (including abstract ones), in hierarchical
         # order.
         + [t.entity for t in ctx.astnode_types]
      )

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
   -- Node type descriptors --
   ---------------------------

   <% node_type_descs = [] %>
   % for n in ctx.astnode_types:
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
