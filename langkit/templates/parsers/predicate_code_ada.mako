## vim: filetype=makoada

--  Start enum_code

${parser.parser.generate_code()}

if ${parser.property_ref.name} (${parser.parser.res_var}) then
    ${parser.res_var} := ${parser.parser.res_var};
    ${parser.pos_var} := ${parser.parser.pos_var};
else
    ${parser.pos_var} := No_Token_Index;
    ${parser.res_var} := null;
end if;

--  End enum_code
