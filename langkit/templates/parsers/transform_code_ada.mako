## vim: filetype=makoada

--  Start transform_code

${parser_context.code}

if ${parser_context.pos_var_name} /= No_Token_Index then

   ## Create the transform wrapper node
   ${res} := ${parser.typ.name()}
     (${parser.typ.name()}_Alloc.Alloc (Parser.Mem_Pool));

   ## Compute and set the sloc range for this AST node. Reminders:
   ##   * pos_name the name for the position of the lexer before this parser
   ##     runs.
   ##   * parser_context.pos_var_name is the name for the position of the lexer
   ##     after this parser runs.
   ## If they are equal then we know that this parser consumed no token. As a
   ## result, the result must be a ghost node, i.e. with no token_end.
   ${res}.Unit := Parser.Unit;
   ${res}.Token_Start := ${pos_name};
   ${res}.Token_End := (if ${parser_context.pos_var_name} = ${pos_name}
                        then No_Token_Index
                        else ${parser_context.pos_var_name} - 1);

   % for field, arg in zip(parser.typ.get_parse_fields(), args):
      ## Set children fields into the created node
      ${res}.${field.name} :=
         % if is_ast_node(field.type):
            ${field.type.storage_type_name()} (${arg});
         % else:
            ${arg};
         % endif
   % endfor

end if;

--  End transform_code
