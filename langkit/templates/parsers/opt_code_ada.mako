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
    ## The subparser failed to match the input: produce result for the empty
    ## sequence.

    % if parser._booleanize:
      % if base.is_bool_type:
         ${parser.res_var} := False;
      % else:
         ${parser.res_var} := ${alt_false.parser_allocator} (Parser.Mem_Pool);
         Initialize
           (Self              => ${parser.res_var},
            Kind              => ${alt_false.ada_kind_name},
            Unit              => Parser.Unit,
            Token_Start_Index => ${parser.start_pos},
            Token_End_Index   => No_Token_Index);
      % endif
    % elif parser_type and parser_type.is_list_type:
        ${subparser.res_var} :=
           ${parser_type.parser_allocator} (Parser.Mem_Pool);
         Initialize
           (Self              => ${parser.res_var},
            Kind              => ${parser_type.ada_kind_name},
            Unit              => Parser.Unit,
            Token_Start_Index => ${parser.start_pos} - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => ${subparser.res_var},
            Parser => Parser,
            Count  => 0);
    % elif parser_type:
        ${subparser.res_var} := ${parser_type.storage_nullexpr};
    % endif

    % if parser._is_error:
        ## Emit a diagnostic informing the user that the sub parser has not
        ## succeeded.
        Append (Parser.Diagnostics,
                Sloc_Range (Parser.TDH.all,
                            Get_Token (Parser.TDH.all, ${parser.start_pos})),
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
      ${parser.res_var} := ${alt_true.parser_allocator} (Parser.Mem_Pool);
      Initialize
        (Self              => ${parser.res_var},
         Kind              => ${alt_true.ada_kind_name},
         Unit              => Parser.Unit,
         Token_Start_Index => ${parser.start_pos},
         Token_End_Index   => ${subparser.pos_var} - 1);
   % endif
% endif

end if;

--  End opt_code
