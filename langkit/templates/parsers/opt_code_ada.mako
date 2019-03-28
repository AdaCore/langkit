## vim: filetype=makoada

--  Start opt_code

<%
subparser = parser.parser

parser_type = subparser.type
if parser._booleanize:
   base = parser.booleanized_type
   if not base.is_bool_type:
      alt_true, alt_false = base._alternatives
%>

${subparser.generate_code()}

if ${subparser.pos_var} = No_Token_Index then
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
         ${parser.res_var}.Self_Env := AST_Envs.Empty_Env;
      % endif
    % elif parser_type and parser_type.is_list_type:
        ${subparser.res_var} :=
          (${parser_type.storage_type_name}_Alloc.Alloc (Parser.Mem_Pool));
        ${parser.res_var}.Kind := ${parser_type.ada_kind_name};
        ${subparser.res_var}.Unit := Parser.Unit;
        ${subparser.res_var}.Count := 0;
        ${subparser.res_var}.Nodes :=
           Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, 0);
        ${subparser.res_var}.Token_Start_Index := ${parser.start_pos} - 1;
        ${subparser.res_var}.Token_End_Index := No_Token_Index;
         ${parser.res_var}.Self_Env := AST_Envs.Empty_Env;
    % elif parser_type:
        ${subparser.res_var} :=
           ${parser_type.storage_nullexpr};
    % endif

    % if parser._is_error:
        ## Emit a diagnostic informing the user that the sub parser has not
        ## succeeded.
        Append (Parser.Diagnostics,
                Get_Token (Parser.TDH.all, ${parser.start_pos}).Sloc_Range,
                To_Text ("Missing '${subparser.error_repr}'"));
    % endif

    ${subparser.pos_var} := ${parser.start_pos};

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
      ${parser.res_var}.Self_Env := AST_Envs.Empty_Env;
   % endif
% endif

end if;

--  End opt_code
