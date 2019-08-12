## vim: filetype=makoada

--  Start opt_code

<%
subparser = parser.parser

parser_type = subparser.type
if parser._booleanize:
   base = parser.booleanized_type
   if not base.is_bool_type:
      alt_true, alt_false = base._alternatives

if parser.type.is_ast_node:
   as_root_node = T.root_node.internal_conversion(parser.type, parser.res_var)
%>

${subparser.generate_code()}

if ${subparser.pos_var} = No_Token_Index then
    ## The subparser failed to match the input: produce result for the empty
    ## sequence.

    % if parser._booleanize:
      % if base.is_bool_type:
         ${parser.res_var} := False;
      % else:
         ${parser.res_var} := ${parser.type.internal_conversion(alt_false,
           '{} (Parser.Mem_Pool)'.format(alt_false.parser_allocator))};
         Initialize
           (Self              => ${as_root_node},
            Kind              => ${alt_false.ada_kind_name},
            Unit              => Parser.Unit,
            Token_Start_Index => ${parser.start_pos},
            Token_End_Index   => No_Token_Index);
      % endif
    % elif parser_type and parser_type.is_list_type:
        ${subparser.res_var} :=
           ${parser_type.parser_allocator} (Parser.Mem_Pool);
         Initialize
           (Self              => ${as_root_node},
            Kind              => ${parser_type.ada_kind_name},
            Unit              => Parser.Unit,
            Token_Start_Index => ${parser.start_pos} - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => ${ctx.generic_list_type.internal_conversion(
                           subparser.type, subparser.res_var)},
            Parser => Parser,
            Count  => 0);
    % elif parser_type:
        ${subparser.res_var} := ${parser_type.storage_nullexpr};
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
   ## The subparser succeeded: if this Opt parser must booleanize the result,
   ## create a result for the "true" alternative.

   % if base.is_bool_type:
      ${parser.res_var} := True;
   % else:
      ${parser.res_var} := ${parser.type.internal_conversion(alt_true,
        '{} (Parser.Mem_Pool)'.format(alt_true.parser_allocator))};
      Initialize
        (Self              => ${as_root_node},
         Kind              => ${alt_true.ada_kind_name},
         Unit              => Parser.Unit,
         Token_Start_Index => ${parser.start_pos},
         Token_End_Index   => ${subparser.pos_var} - 1);
   % endif
% endif

end if;

--  End opt_code
