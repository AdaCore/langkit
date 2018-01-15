## vim: filetype=makoada

## This file provides templates for the code generation of root AST list types.
## Derived AST list types are handled directly in astnode_types_ada.mako, as
## their definition is much more like the ones of regular AST nodes.

<%namespace name="unparsers" file="unparsers_ada.mako" />

<%def name="public_incomplete_decl(element_type)">

   <%
      list_type = element_type.list
      value_type = list_type.value_type_name()
      type_name = list_type.name
   %>

   type ${value_type};
   type ${type_name} is access all ${list_type.value_type_name()}'Class;

</%def>

<%def name="private_decl(element_type)">

   <%
      list_type = element_type.list
      value_type = list_type.value_type_name()
      type_name = list_type.name
   %>

   type ${value_type} is
      ${'abstract' if element_type.has_abstract_list else ''}
      new ${generic_list_value_type} with null record;

   % if not element_type.has_abstract_list:
      % if ctx.generate_unparser:
         procedure Unparse_${list_type.name}
           (Node   : ${list_type.name};
            Result : in out Unbounded_Wide_Wide_String);
      % endif
   % endif

   ## Helpers generated for properties code. Used in CollectionGet's and
   ## Map/Quantifier's code.
   function Item
     (Node  : access ${value_type}'Class; Index : Positive)
      return ${element_type.name};

   function Get
     (Node    : access ${value_type}'Class;
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

   % if not element_type.has_abstract_list:

      % if ctx.generate_unparser:
         procedure Unparse_${list_type.name}
           (Node   : ${list_type.name};
            Result : in out Unbounded_Wide_Wide_String) is
         begin
            ${unparsers.unparser(list_type)}
         end Unparse_${list_type.name};
      % endif

   % endif

   ---------
   -- Get --
   ---------

   function Get
     (Node    : access ${value_type}'Class;
      Index   : Integer;
      Or_Null : Boolean := False) return ${element_type.name}
   is
      function Absolute_Get
        (L : ${type_name}; Index : Integer)
         return ${element_type.name}
      is
        (${element_type.name} (L.Nodes (Index + 1)));
      --  L.Nodes is 1-based but Index is 0-based

      function Length (Node : ${type_name}) return Natural is (Node.Count);
      --  Wrapper around the Length primitive to get the compiler happy for the
      --  the package instantiation below.

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${element_type.name},
         Sequence_Type => ${type_name},
         Length        => Length,
         Get           => Absolute_Get);

      Result : ${element_type.name};
   begin
      if Relative_Get (${type_name} (Node), Index, Result) then
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
     (Node  : access ${value_type}'Class; Index : Positive)
      return ${element_type.name}
   is (${element_type.name} (Node.Child (Index)));
</%def>
