## vim: filetype=makoada

<%def name="public_decl(element_type)">

   <% type = element_type.storage_type_name() %>

   type List_${type}_Type is new ${root_node_value_type} with private;
   type List_${type} is access all List_${type}_Type'Class;

</%def>


<%def name="private_decl(element_type)">

   <% type = element_type.storage_type_name() %>

   package Lists_${type} is new List
     (Node_Type   => ${type}_Type,
      Node_Access => ${type});

   type List_${type}_Type is
      new Lists_${type}.List_Type with null record;

   type List_${type}_Access is access all List_${type}_Type;

   package List_${type}_Alloc is
     new Tagged_Alloc (List_${type}_Type);

   ## Helper generated for properties code. Used in CollectionGet's code
   function Get
     (Node    : List_${type};
      Index   : Integer;
      Or_Null : Boolean := False) return ${root_node_type_name};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.

   ## Helper for properties code
   function Length (Node : List_${type}) return Natural is
     (Node.Child_Count);

</%def>

<%def name="body(element_type)">

   <% type = element_type.storage_type_name() %>

   ---------
   -- Get --
   ---------

   function Get
     (Node    : List_${type};
      Index   : Integer;
      Or_Null : Boolean := False) return ${root_node_type_name}
   is
      function Absolute_Get
        (L : List_${type}; Index : Integer)
         return ${root_node_type_name}
      is
        (${root_node_type_name}
          (Lists_${type}.Node_Vectors.Get_At_Index (L.Vec, Index)));

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${root_node_type_name},
         Sequence_Type => List_${type},
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
