## vim: filetype=makoada

--  Start opt_code

${parser_context.code}

% if _self._booleanize:
    ${bool_res} := True;
% endif

if ${parser_context.pos_var_name} = No_Token_Index then
    % if _self._booleanize:
        ${bool_res} := False;
    % else:
        ${parser_context.res_var_name} :=
           ${_self.parser.get_type().storage_nullexpr()};
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
end if;

--  End opt_code
