## vim: filetype=makoada

--  Start tok_code

## Get the current token
${parser.res_var} := ${parser.start_pos};

declare
   T : constant Lexer.Token_Data_Type :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (${parser.res_var}));
begin
   if
      T.Kind /= ${token_kind}
      % if parser.matches_symbol:
      or else T.Symbol /= Parser.Symbol_Literals
        (${ctx.symbol_literals[match_text]})
      % elif match_text:
      or else Text (Parser.TDH.all, T) /= "${match_text}"
      % endif
   then
       ## If the result is not the one we expect, set pos to error
       ${parser.pos_var} := No_Token_Index;

       ## Document this failure so we can have a diagnostic at the end of
       ## parsing.
       if Parser.Last_Fail.Pos <= ${parser.start_pos} then
          Parser.Last_Fail :=
            (Kind => Token_Fail,
             Pos => ${parser.start_pos},
             Expected_Token_Id => ${token_kind},
             Found_Token_Id => T.Kind);
       end if;
   else
      ## We don't want to increment the position if we are matching the
      ## termination token (eg. the last token in the token stream).
      % if token_kind == ctx.lexer.Termination.ada_name:
          ${parser.pos_var} := ${parser.start_pos};
      ## Else increment the current position
      % else:
          ${parser.pos_var} := ${parser.start_pos} + 1;
      % endif
   end if;
end;

--  End tok_code
