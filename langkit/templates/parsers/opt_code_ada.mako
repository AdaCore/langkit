## vim: filetype=makoada

--  Start opt_code

${parser.parser.generate_code()}

<%
parser_type = parser.parser.get_type()
if parser._booleanize:
   base = parser.booleanized_type
   if not base.is_bool_type:
      alt_true, alt_false = base._alternatives
%>

if ${parser.parser.pos_var} = No_Token_Index then
    % if parser._booleanize:
      % if base.is_bool_type:
         ${parser.res_var} := False;
      % else:
         ${parser.res_var} := ${base.name}
           (${alt_false.name}_Alloc.Alloc (Parser.Mem_Pool));
         ${parser.res_var}.Kind := ${alt_false.ada_kind_name};
         ${parser.res_var}.Unit := Parser.Unit;
         ${parser.res_var}.Token_Start_Index := ${parser.start_pos};
         ${parser.res_var}.Token_End_Index := No_Token_Index;
      % endif
    % elif parser_type and parser_type.is_list_type:
        ${parser.parser.res_var} :=
          (${parser_type.storage_type_name}_Alloc.Alloc (Parser.Mem_Pool));
        ${parser.res_var}.Kind := ${parser_type.ada_kind_name};
        ${parser.parser.res_var}.Unit := Parser.Unit;
        ${parser.parser.res_var}.Count := 0;
        ${parser.parser.res_var}.Nodes :=
           Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, 0);
        ${parser.parser.res_var}.Token_Start_Index := ${parser.start_pos} - 1;
        ${parser.parser.res_var}.Token_End_Index := No_Token_Index;
    % elif parser_type:
        ${parser.parser.res_var} :=
           ${parser_type.storage_nullexpr};
    % endif

    % if parser._is_error:
        ## Emit a diagnostic informing the user that the sub parser has not
        ## succeeded.
        Append (Parser.Diagnostics,
                Get_Token (Parser.TDH.all, ${parser.start_pos}).Sloc_Range,
                To_Text ("Missing '${parser.parser.error_repr}'"));
    % endif

    ${parser.parser.pos_var} := ${parser.start_pos};

% if parser._booleanize:
else
   % if base.is_bool_type:
      ${parser.res_var} := True;
   % else:
      ${parser.res_var} := ${base.name}
        (${alt_true.name}_Alloc.Alloc (Parser.Mem_Pool));
      ${parser.res_var}.Kind := ${alt_true.ada_kind_name};
      ${parser.res_var}.Unit := Parser.Unit;
      ${parser.res_var}.Token_Start_Index := ${parser.start_pos};
      ${parser.res_var}.Token_End_Index := No_Token_Index;
   % endif
% endif

end if;

--  End opt_code
