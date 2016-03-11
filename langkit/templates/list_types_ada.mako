## vim: filetype=makoada

<%def name="public_decl(element_type)">

   <% type = decl_type(element_type) %>

   type List_${type}_Type is new ${root_node_value_type} with private;
   type List_${type} is access all List_${type}_Type'Class;

</%def>


<%def name="private_decl(element_type)">

   <% type = decl_type(element_type) %>

   package Lists_${type} is new List
     (Node_Type   => ${type}_Type,
      Node_Access => ${type});

   type List_${type}_Type is
      new Lists_${type}.List_Type with null record;

   type List_${type}_Access is access all List_${type}_Type;

   package List_${type}_Alloc is
     new Tagged_Alloc (List_${type}_Type);

   ## Helper getter generated for properties code. Used in CollectionGet's code
   function Get
     (Node    : List_${type};
      Index   : Integer;
      Or_Null : Boolean := False) return ${root_node_type_name}
   is
     (if Index in 0 .. Node.Child_Count - 1
      then ${root_node_type_name}
        (Lists_${type}.Node_Vectors.Get_At_Index (Node.Vec, Index))
      else (if Index in -Node.Child_Count + 1 .. -1
            then ${root_node_type_name}
              (Lists_${type}.Node_Vectors.Get_At_Index
                 (Node.Vec, Node.Child_Count - Index - 1))
            else (if Or_Null then null else raise Property_Error)));
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.

   ## Helper for properties code
   function Length (Node : List_${type}) return Natural is
     (Node.Child_Count);

</%def>
