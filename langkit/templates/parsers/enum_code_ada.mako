## vim: filetype=makoada

--  Start enum_code

${parser.parser.generate_code() if parser.parser else ""}

% if parser.parser:
if ${parser.pos_var} /= No_Token_Index then
    ${res} := ${parser.enum_type_inst.enumerator};
end if;
% else
    ${res} := ${parser.enum_type_inst.enumerator};
% endif

--  End enum_code
