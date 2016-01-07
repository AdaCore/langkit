## vim: filetype=makoada

<%def name="public_decl(element_type)">

   <% type = decl_type(element_type) %>

   type List_${type}_Type is new ${root_node_value_type} with private;
   type List_${type} is
      access all List_${type}_Type'Class;

</%def>


<%def name="private_decl(element_type)">

   <% type = decl_type(element_type) %>

   package Lists_${type} is new AST.List
     (Node_Type   => ${type}_Type,
      Node_Access => ${type});

   type List_${type}_Type is
      new Lists_${type}.List_Type with null record;

   type List_${type}_Access is access all List_${type}_Type;

   package List_${type}_Alloc is
     new Tagged_Alloc (List_${type}_Type, List_${type}_Access);

</%def>
