## vim: filetype=makoada

--  Start row_code

${parser.pos_var} := ${parser.start_pos};

## This is the main body of the row, which is the concatenation of the code for
## each row part.
% for subp in parser.parsers:

<% code = subp.generate_code() %>

## Parse the element
${code}

## If the parsing was successful then
if ${subp.pos_var} /= No_Token_Index then

   ## Set current position to the out position of the parsed row element
   ${parser.pos_var} := ${subp.pos_var};

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
