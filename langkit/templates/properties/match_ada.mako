## vim: filetype=makoada

<%
    prefix_node_type = expr.prefix_var.type
    if prefix_node_type.is_entity_type:
        prefix_node_type = prefix_node_type.element_type
    prefix_node_expr = '{}{}'.format(
      expr.prefix_var.codegen_name,
      '.Node' if expr.prefix_var.type.is_entity_type else ''
    )

    # Expression to compute the kind of the prefix, on which we dispatch. Take
    # care of converting it to the actual kind subrange so that we get full
    # coverage in the CASE block below.
    kind_expr = '{} ({}.Kind)'.format(prefix_node_type.ada_kind_range_name,
                                      prefix_node_expr)
%>

${expr.prefix_expr.render_pre()}
${assign_var(expr.prefix_var, expr.prefix_expr.render_expr())}

case ${kind_expr} is
   % for m in expr.matchers:
      % if m.matched_concrete_nodes:
         when ${ctx.astnode_kind_set(m.matched_concrete_nodes)} =>
            ${m.match_expr.render_pre()}
            ${assign_var(expr.result_var, m.match_expr.render_expr())}
      % endif
   % endfor
end case;
