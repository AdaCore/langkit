## vim: filetype=makoada

--  Start row_code

${parser.pos_var} := ${parser.start_pos};

## This is the main body of the row, which is the concatenation of the code for
## each row part.
% for (subp, subres) in zip(parser.parsers, parser.subresults):

<% code = subp.generate_code() %>

## Parse the element
${code}

## If the parsing was successful then
if ${subp.pos_var} /= No_Token_Index then

   ## Set current position to the out position of the parsed row element
   ${parser.pos_var} := ${subp.pos_var};

   ## Store the result if it is not discarded
   % if not subp.discard():
      ${subres} := ${subp.res_var};
   % endif

else
   ## If the parsing was unsuccessful, then set the position accordingly
   ## and then skip the rest of the row parsing.
   ${parser.pos_var} := No_Token_Index;
   goto ${exit_label}_0;

end if;
% endfor

pragma Warnings (Off, "referenced");
<<${exit_label}_0>>
pragma Warnings (On, "referenced");

--  End row_code
