## vim: filetype=makoada

<%namespace name="astnode_types" file="astnode_types_ada.mako" />

## This file provides templates for the code generation of root AST list types.
## Derived AST list types are handled directly in astnode_types_ada.mako, as
## their definition is much more like the ones of regular AST nodes.

<%def name="public_incomplete_decl(element_type)">

   <% list_type = element_type.list %>

   type ${list_type.value_type_name()};
   type ${list_type.name} is access all ${list_type.value_type_name()};

   ${list_type.null_constant} : constant ${list_type.name} := null;

   ${astnode_types.bare_node_converters(list_type)}
</%def>

<%def name="private_decl(element_type)">

   <%
      list_type = element_type.list
      value_type = list_type.value_type_name()
      type_name = list_type.name
   %>

   type ${value_type} is record
      Base : ${generic_list_value_type};
   end record
      with Convention => C;

   ## Helpers generated for properties code. Used in CollectionGet's and
   ## Map/Quantifier's code.
   function Item
     (Node : access ${value_type}; Index : Positive)
      return ${element_type.name};

   function Get
     (Node    : access ${value_type};
      Index   : Integer;
      Or_Null : Boolean := False) return ${element_type.name};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
</%def>

<%def name="body(element_type)">

   <%
      list_type = element_type.list
      value_type = list_type.value_type_name()
      type_name = list_type.name
   %>

   ---------
   -- Get --
   ---------

   function Get
     (Node    : access ${value_type};
      Index   : Integer;
      Or_Null : Boolean := False) return ${element_type.name}
   is
      function Absolute_Get
        (L : ${type_name}; Index : Integer) return ${element_type.name};
      --  L.Nodes is 1-based but Index is 0-based

      ------------------
      -- Absolute_Get --
      ------------------

      function Absolute_Get
        (L : ${type_name}; Index : Integer) return ${element_type.name}
      is
         GL : constant ${ctx.generic_list_type.name} :=
            ${ctx.generic_list_type.internal_conversion(list_type, 'L')};
      begin
         return ${element_type.internal_conversion(T.root_node,
                                                   'GL.Nodes (Index + 1)')};
      end Absolute_Get;

      function Length (Node : ${type_name}) return Natural is
        (${ctx.generic_list_type.internal_conversion(list_type, 'Node')}.Count);
      --  Wrapper around the Length primitive to get the compiler happy for the
      --  the package instantiation below.

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${element_type.name},
         Sequence_Type => ${type_name},
         Length        => Length,
         Get           => Absolute_Get);

      Result : ${element_type.name};
   begin
      if Relative_Get (Node, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         raise Property_Error with "out-of-bounds AST list access";
      end if;
   end Get;

   ----------
   -- Item --
   ----------

   function Item
     (Node : access ${value_type}; Index : Positive)
      return ${element_type.name}
   is
      Result : constant ${root_node_type_name} :=
        Child (${T.root_node.internal_conversion(list_type, 'Node')}, Index);
   begin
      return ${element_type.internal_conversion(T.root_node, 'Result')};
   end Item;
</%def>
