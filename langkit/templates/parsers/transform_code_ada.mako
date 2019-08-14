## vim: filetype=makoada

--  Start transform_code

${parser.parser.generate_code()}

## If this parser is part of a no_backtrack hierarchy, then we want to
## recover.
% if parser.no_backtrack:
if ${parser.pos_var} = No_Token_Index and then ${parser.no_backtrack} then
   ${parser.pos_var} := Parser.Last_Fail.Pos;
   ${parser.has_failed_var} := True;
end if;
% endif

if ${parser.pos_var} /= No_Token_Index then

   ## Create the transform wrapper node
   ${parser.res_var} := ${parser.type.parser_allocator} (Parser.Mem_Pool);

   ## Initialize components common to all nodes
   Initialize
     (Self => ${T.root_node.internal_conversion(parser.type,
                                                parser.res_var)},
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
      Initialize_Fields_For_${parser.type.kwless_raw_name}
        (Self => ${parser.res_var}${''.join(
            ', {} => {}'.format(
               field.name,
               field.type.internal_conversion(subparser.type, subresult))
            for field, subparser, subresult in args)});
   % endif

   % if args:
      ## Update Last_Attempted_Child for the created node depending on the
      ## subparsers' results.
      declare
         N : constant ${T.root_node.name} :=
            ${T.root_node.internal_conversion(parser.type, parser.res_var)};
      begin
         % for _, subparser, subresult in args:
            declare
               C : constant ${T.root_node.name} :=
                  ${T.root_node.internal_conversion(subparser.type,
                                                    subresult)};
            begin
               if C /= null and then Is_Incomplete (C) then
                  N.Last_Attempted_Child := 0;
               elsif C /= null and then not Is_Ghost (C) then
                  N.Last_Attempted_Child := -1;
               end if;
            end;
         % endfor
      end;
   % endif

   ## Propagate parsing errors
   % if parser.no_backtrack:
   if ${parser.has_failed_var} then
      ${T.root_node.internal_conversion(parser.type, parser.res_var)}
         .Last_Attempted_Child :=
         ${parser.parser.progress_var if is_row(parser.parser) else 1};

      Append (Parser.Diagnostics,
              Get_Token (Parser.TDH.all, ${parser.start_pos}).Sloc_Range,
              To_Text ("Cannot parse <${parser.name}>"));

      Add_Last_Fail_Diagnostic (Parser);
   end if;
   % endif

end if;

--  End transform_code
