## vim: filetype=makoada

--  Start or_code

${parser.pos_var} := No_Token_Index;
${parser.res_var} := ${parser.get_type().storage_nullexpr};
% for subparser in parser.parsers:
    ${subparser.generate_code()}
    if ${subparser.pos_var} /= No_Token_Index then
        ${parser.pos_var} := ${subparser.pos_var};
        ${parser.res_var} := ${parser.get_type().storage_type_name}
          (${subparser.res_var});
        goto ${exit_label};
    end if;
% endfor
<<${exit_label}>>

--  End or_code
