## vim: filetype=makoada

--  Start opt_code

${parser_context.code}

<%
parser_type = _self.parser.get_type()
if _self._booleanize:
   base, alt_true, alt_false = _self._booleanize
%>

if ${parser_context.pos_var_name} = No_Token_Index then
    % if _self._booleanize:
      % if is_bool(base):
         ${bool_res} := False;
      % else:
         ${bool_res} := ${base.name()}
           (${alt_false.name()}_Alloc.Alloc (Parser.Mem_Pool));
         ${bool_res}.Unit := Analysis_Unit_Interface (Parser.Unit);
         ${bool_res}.Token_Start := ${pos_name};
         ${bool_res}.Token_End := No_Token_Index;
      % endif
    % elif parser_type.is_list_type:
        ${parser_context.res_var_name} :=
          (${parser_type.storage_type_name()}_Alloc.Alloc (Parser.Mem_Pool));
        ${parser_context.res_var_name}.Unit :=
           Analysis_Unit_Interface (Parser.Unit);
        ${parser_context.res_var_name}.Token_Start := ${pos_name} - 1;
        ${parser_context.res_var_name}.Token_End := No_Token_Index;
    % else:
        ${parser_context.res_var_name} :=
           ${parser_type.storage_nullexpr()};
    % endif

    % if _self._is_error:
        ## Emit a diagnostic informing the user that the sub parser has not
        ## succeeded.
        <% missing_item = (_self.parser.val
                           if is_tok(_self.parser) else
                           repr(_self.parser)) %>
        Parser.Diagnostics.Append
          ((Get_Token (Parser.TDH.all, ${pos_name}).Sloc_Range,
            To_Unbounded_Wide_Wide_String (To_Text
            ("Missing '${missing_item}'"))));
    % endif

    ${parser_context.pos_var_name} := ${pos_name};

% if _self._booleanize:
else
   % if is_bool (base):
      ${bool_res} := True;
   % else:
      ${bool_res} := ${base.name()}
        (${alt_true.name()}_Alloc.Alloc (Parser.Mem_Pool));
      ${bool_res}.Unit := Analysis_Unit_Interface (Parser.Unit);
      ${bool_res}.Token_Start := ${pos_name};
      ${bool_res}.Token_End := No_Token_Index;
   % endif
% endif

end if;

--  End opt_code
