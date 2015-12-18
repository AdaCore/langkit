## vim: filetype=makoada

<%
   ind_var = quantifier.induction_var.name
   result_var = quantifier.result_var.name
%>

${quantifier.collection.render_pre()}

${result_var} := ${'False' if quantifier.kind == ANY else 'True'};
for ${ind_var} of
   % if quantifier.collection.type.is_list_type:
      ${quantifier.collection.render_expr()}.Vec
   % else:
      ${quantifier.collection.render_expr()}.Items
   % endif
loop
   ${quantifier.expr.render_pre()}

   ## Depending on the kind of the quantifier, we want to abort as soon as the
   ## predicate holds or as soon as it does not hold.
   % if quantifier.kind == ANY:
      if ${quantifier.expr.render_expr()} then
         ${result_var} := True;
         exit;
      end if;
   % else:
      if not (${quantifier.expr.render_expr()}) then
         ${result_var} := False;
         exit;
      end if;
   % endif
end loop;
