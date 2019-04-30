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
   ${parser.res_var} := ${parser.type.name}
     (${parser.type.name}_Alloc.Alloc (Parser.Mem_Pool));

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

   ## Initialize components for node fields
   <% fields_n_args = zip(
         parser.type.get_parse_fields(
            predicate=lambda f: not f.abstract and not f.null),
         args) %>

   % for i, (field, arg) in enumerate(fields_n_args):
      ## Set children fields into the created node
      ${parser.res_var}.${field.name} :=
         ${field.type.storage_type_name} (${arg});

      if ${arg} /= null and then ${arg}.Is_Incomplete then
         ${parser.res_var}.Last_Attempted_Child := 0;
      elsif ${arg} /= null and then not ${arg}.Is_Ghost then
         ${parser.res_var}.Last_Attempted_Child := -1;
      end if;
   % endfor

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
