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
   ${parser.res_var} := ${parser.get_type().name}
     (${parser.get_type().name}_Alloc.Alloc (Parser.Mem_Pool));
   ${parser.res_var}.Kind := ${parser.get_type().ada_kind_name};

   ## Compute and set the sloc range for this AST node. Reminders:
   ##   * start_pos the name for the position of the lexer before this parser
   ##     runs.
   ##   * parser.pos_var is the name for the position of the lexer
   ##     after this parser runs.
   ## If they are equal then we know that this parser consumed no token. As a
   ## result, the result must be a ghost node, i.e. with no token_end.

   ${parser.res_var}.Unit := Parser.Unit;
   ${parser.res_var}.Token_Start_Index := ${parser.start_pos};

   ${parser.res_var}.Token_End_Index :=
     (if ${parser.pos_var} = ${parser.start_pos}
      then No_Token_Index
      else ${parser.pos_var} - 1);
   <% fields_n_args = zip(parser.get_type().get_parse_fields(), args) %>

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

   % if parser.no_backtrack:
   if ${parser.has_failed_var} then
      ${parser.res_var}.Last_Attempted_Child :=
        ${parser.parser.progress_var if is_row(parser.parser) else 1};

        Append (Parser.Diagnostics,
                Get_Token (Parser.TDH.all, ${parser.start_pos}).Sloc_Range,
                To_Text ("Cannot parse <${parser.name}>"));

        Add_Last_Fail_Diagnostic (Parser);
   end if;

   % endif

end if;

--  End transform_code
