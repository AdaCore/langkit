## vim: filetype=makoada

${parser.pos_var} := No_Token_Index;
${parser.res_var} := ${parser.type.storage_nullexpr};
% for subparser in parser.parsers:
    ${subparser.generate_code()}
    if ${subparser.pos_var} /= No_Token_Index then
        ${parser.pos_var} := ${subparser.pos_var};
        ${parser.res_var} := ${subparser.res_var};
        goto ${exit_label};
    end if;
% endfor
<<${exit_label}>>
