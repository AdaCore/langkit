## vim: filetype=makoada

--  Start or_code

${parser.pos_var} := No_Token_Index;
${parser.res_var} := ${parser.type.storage_nullexpr};
% for subparser in parser.parsers:
    ${subparser.generate_code()}
    if ${subparser.pos_var} /= No_Token_Index then
        ${parser.pos_var} := ${subparser.pos_var};
        ${parser.res_var} :=
            % if parser.type.is_ast_node:
                ${(parser.type.internal_conversion(subparser.type,
                                                   subparser.res_var))}
            % else:
                ${subparser.res_var}
            % endif
        ;
        goto ${exit_label};
    end if;
% endfor
<<${exit_label}>>

--  End or_code
