## vim: filetype=makoada

--  Start or_code

${parser.pos_var} := No_Token_Index;
${parser.res_var} := ${parser.get_type().storage_nullexpr()};
% for ctx in results:
    ${ctx.code}
    if ${ctx.pos_var_name} /= No_Token_Index then
        ${parser.pos_var} := ${ctx.pos_var_name};
        ${parser.res_var} := ${parser.get_type().storage_type_name()}
          (${ctx.res_var_name});
        goto ${exit_label};
    end if;
% endfor
<<${exit_label}>>

--  End or_code
