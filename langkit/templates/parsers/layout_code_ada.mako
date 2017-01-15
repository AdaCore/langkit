## vim: filetype=makoada

declare
   T : Lexer.Token_Data_Type;
begin
   T := Token_Vectors.Get (Parser.TDH.Tokens, Natural (${pos_name}));
   if
      % if mode == "NEWLINE":
      T.Indent in Dedent | Indent | Nodent
      % else:
      T.Indent = ${mode.capitalize()}
      % endif
   then
      ${pos} := ${pos_name};
   else
      ${pos} := No_Token_Index;
      if Parser.Last_Fail.Pos <= ${pos_name} then
         Parser.Last_Fail :=
           (Kind => Custom_Fail,
            Pos => ${pos_name},
            Custom_Message => Fail_${mode.capitalize()});
      end if;
   end if;
end;
