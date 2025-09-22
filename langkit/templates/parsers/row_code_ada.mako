## vim: filetype=makoada

${parser.pos_var} := ${parser.start_pos};

## This is the main body of the row, which is the concatenation of the code for
## each row part.
% for num, subparser in enumerate(parser.parsers, 1):

## Parse the element
${subparser.generate_code()}

## Propagate no_backtrack information. If a subparser sets its no_backtrack
## variable, it should propagate the result to its parent.
<%
   p_nobt = parser.no_backtrack
   sp_nobt = subparser.no_backtrack
%>
% if p_nobt and sp_nobt and sp_nobt != p_nobt:
   ${p_nobt} := ${sp_nobt};
% endif

% if parser.progress_var:
${parser.progress_var} := ${num};
% endif

## If the parsing was successful then
if ${subparser.pos_var} /= No_Token_Index then

   ## Set current position to the out position of the parsed row element
   ${parser.pos_var} := ${subparser.pos_var};

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
