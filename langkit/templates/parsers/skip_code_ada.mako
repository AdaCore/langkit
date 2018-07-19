## vim: filetype=makoada

if Get_Token (Parser.TDH.all, ${parser.start_pos}).Kind 
   = ${ctx.lexer.Termination.ada_name} 
then
   ${parser.pos_var} := No_Token_Index;
   goto ${exit_label};
end if;

for Fn of Parser.Private_Part.Dont_Skip loop
   ${parser.dummy_node} := Fn (Parser, ${parser.start_pos});

   if Parser.Current_Pos /= No_Token_Index  then
      ${parser.pos_var} := No_Token_Index;
      goto ${exit_label};
   end if;
end loop;

${parser.pos_var} := ${parser.start_pos} + 1;
${parser.dest_node_parser.generate_code()}
${parser.dest_node_parser.res_var}.Token_End_Index := ${parser.start_pos};

Append (Parser.Diagnostics,
        Get_Token (Parser.TDH.all, ${parser.start_pos}).Sloc_Range,
        To_Text ("Skipped token ")
        & Text (Wrap_Token_Reference (Parser.TDH,
                                      (${parser.start_pos}, No_Token_Index))));

<<${exit_label}>>
