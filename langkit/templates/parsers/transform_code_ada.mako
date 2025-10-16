## vim: filetype=makoada

${parser.parser.generate_code()}

## If this parser is part of a no_backtrack hierarchy, then we want to
## recover.
% if parser.no_backtrack:
if ${parser.pos_var} = No_Token_Index and then ${parser.no_backtrack} then

   ## Resetting the pos var so that we continue parsing
   ${parser.pos_var} := Parser.Last_Fail.Pos;

   ## Record that parser has failed nonetheless (i.e. that the result will be
   ## incorrect/incomplete).
   ${parser.has_failed_var} := True;
end if;
% endif

if ${parser.pos_var} /= No_Token_Index then

   ## Create the transform wrapper node
   ${parser.res_var} := ${parser.type.parser_allocator} (Parser.Mem_Pool);

   ## Initialize components common to all nodes
   Initialize
     (Self => ${parser.res_var},
      Kind => ${parser.type.ada_kind_name},
      Unit => Parser.Unit,

      ## Compute and set the sloc range for this AST node. Reminders:
      ##   * start_pos the name for the position of the lexer before this
      ##     parser runs.
      ##   * parser.pos_var is the name for the position of the lexer
      ##     after this parser runs.
      ## If they are equal then we know that this parser consumed no token.
      ## As a result, the result must be a ghost node, i.e. with no
      ## token_end.
      Token_Start_Index => ${parser.start_pos},
      Token_End_Index   => (if ${parser.pos_var} = ${parser.start_pos}
                            then No_Token_Index
                            else ${parser.pos_var} - 1));

   ## Run the kind-specific initializer, if any
   % if parser.type.has_fields_initializer:
      <%
         init_args = [f"Self => {parser.res_var}"]
         init_args += [
            f"{field.names.codegen} => {subresult}"
            for field, subparser, subresult in args
         ]
      %>
      Initialize_Fields_For_${parser.type.kwless_raw_name}
      ${ada_block_with_parens(init_args, 6)};
   % endif

   % if args:
      ## Update Last_Attempted_Child for the created node depending on the
      ## subparsers' results.
      % for _, subparser, subresult in args:
         if ${subresult} /= null and then Is_Incomplete (${subresult}) then
            ${parser.res_var}.Last_Attempted_Child := 0;
         elsif ${subresult} /= null and then not Is_Ghost (${subresult}) then
            ${parser.res_var}.Last_Attempted_Child := -1;
         end if;
      % endfor
   % endif

   ## Propagate parsing errors
   % if parser.no_backtrack:
   if ${parser.has_failed_var} then
      ${parser.res_var}.Last_Attempted_Child :=
         ${parser.parser.progress_var if is_row(parser.parser) else 1};

      Append (Parser, ${parser.start_pos}, "Cannot parse <${parser.name}>");
      Add_Last_Fail_Diagnostic (Parser);
   end if;
   % endif

end if;
