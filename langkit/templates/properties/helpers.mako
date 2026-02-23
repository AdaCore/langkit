## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} : ${property.prefix_var.type.name}
   % for arg in property.arguments:
      ; ${arg.name} : ${arg.type.name}
      % if arg.default_value is not None:
         := ${arg.default_value.render_private_ada_constant()}
      % endif
   % endfor
   % if property.uses_entity_info:
   ; ${property.entity_info_name} : ${T.EntityInfo.name} :=
      ${T.EntityInfo.nullexpr}
   % endif
  )
</%def>

<%def name="logic_predicates(prop)">
   <%
      type_name = pred.id
      formal_node_types = prop.get_concrete_node_types(pred)
      enumerated_arg_types = list(enumerate(formal_node_types[1:], 1))
      arity = len(formal_node_types)
      has_refcounted_args = any(
         pa.type.is_refcounted for pa in pred.partial_args
      )
      has_multiple_concrete_nodes = len(T.root_node.concrete_subclasses) > 1
      is_variadic = arity > 1 and formal_node_types[1].is_array_type
   %>
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted:
      Inc_Ref (${var.name});
   % endif
</%def>
