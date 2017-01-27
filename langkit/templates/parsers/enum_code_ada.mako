## vim: filetype=makoada

--  Start enum_code

${parser_context.code}

if ${parser_context.pos_var_name} /= No_Token_Index then
    ${res} := ${parser.enum_type_inst.enumerator};
end if;

--  End enum_code
