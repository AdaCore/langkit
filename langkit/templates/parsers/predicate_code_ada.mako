## vim: filetype=makoada

--  Start predicate_code

${parser.parser.generate_code()}

if ${parser.parser.res_var} /= null
   and then ${parser.property_ref.qual_impl_name} (${parser.parser.res_var})
then
    ${parser.res_var} := ${parser.parser.res_var};
    ${parser.pos_var} := ${parser.parser.pos_var};
else
    ${parser.pos_var} := No_Token_Index;
    ${parser.res_var} := null;

    ## Document this failure so we can have a diagnostic at the end of
    ## parsing.
    if Parser.Last_Fail.Pos <= ${parser.start_pos} then
       Parser.Last_Fail :=
         (Kind           => Custom_Fail,
          Pos            => ${parser.start_pos},
          Custom_Message => Generic_Parsing_Error_Message_Access);
    end if;
end if;

--  End predicate_code
