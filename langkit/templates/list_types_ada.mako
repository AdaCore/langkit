## vim: filetype=makoada

<%def name="public_decl(element_type)">

   <%
      elt_type = element_type.name()
      value_type = 'List_{}_Type'.format(elt_type)
      type_name = 'List_{}'.format(elt_type)
   %>

   type ${value_type} is new ${root_node_value_type} with private;
   type ${type_name} is access all ${value_type}'Class;

</%def>


<%def name="private_decl(element_type)">

   <%
      elt_type = element_type.name()
      pkg_name = 'Lists_{}'.format(elt_type)
      value_type = 'List_{}_Type'.format(elt_type)
      type_name = 'List_{}'.format(elt_type)
      access_type = 'List_{}_Access'.format(elt_type)
   %>

   package ${pkg_name} is new List
     (Node_Type   => ${elt_type}_Type,
      Node_Access => ${elt_type});

   type ${value_type} is
      new ${pkg_name}.List_Type with null record;

   type ${access_type} is access all ${value_type};

   package List_${elt_type}_Alloc is
     new Tagged_Alloc (${value_type});

   ## Helper generated for properties code. Used in CollectionGet's code
   function Get
     (Node    : ${type_name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${root_node_type_name};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.

   ## Helper for properties code
   function Length (Node : ${type_name}) return Natural is
     (Node.Child_Count);

</%def>

<%def name="body(element_type)">

   <%
      elt_type = element_type.name()
      pkg_name = 'Lists_{}'.format(elt_type)
      type_name = 'List_{}'.format(elt_type)
   %>

   ---------
   -- Get --
   ---------

   function Get
     (Node    : ${type_name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${root_node_type_name}
   is
      function Absolute_Get
        (L : ${type_name}; Index : Integer)
         return ${root_node_type_name}
      is
        (${root_node_type_name}
          (${pkg_name}.Node_Vectors.Get_At_Index (L.Vec, Index)));

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${root_node_type_name},
         Sequence_Type => ${type_name},
         Length        => Length,
         Get           => Absolute_Get);

      Result : ${root_node_type_name};
   begin
      if Relative_Get (Node, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         raise Property_Error;
      end if;
   end Get;

</%def>
