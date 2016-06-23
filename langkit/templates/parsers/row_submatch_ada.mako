## vim: filetype=makoada

--  Start row_submatch

## Parse the element
${parser_context.code}

## If the parsing was successful then
if ${parser_context.pos_var_name} /= No_Token_Index then

   ## Set current position to the out position of the parsed row element
   ${pos} := ${parser_context.pos_var_name};

   ## Store the result if it is not discarded
   % if not parser.discard():
      ${subresult} := ${parser_context.res_var_name};
   % endif

else
   ## If the parsing was unsuccessful, then set the position accordingly
   ## and then skip the rest of the row parsing.
   ${pos} := No_Token_Index;
   goto ${exit_label}_0;

end if;

--  End row_submatch
