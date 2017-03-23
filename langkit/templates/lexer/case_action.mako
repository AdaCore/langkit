## vim: filetype=makocpp


{
    switch (me->token->last_id) {
    % for alt in alts:
    % for tok in alt.prev_token_cond:
    case ${tok.quex_name}:
    % endfor

    // In the case of the case association, regardless of the action used,
    // we'll always send the lexeme. This lexeme will be discarded by the token
    // processing later in the chain
    self_send1(${alt.send.quex_name}, Lexeme);
    % if max_match_len - alt.match_size > 0:
        QUEX_NAME(seek_backward)(&self, ${max_match_len - alt.match_size});
    % endif
    break;
    % endfor

    default:
    self_send1(${default_alt.send.quex_name}, Lexeme);
    % if max_match_len - default_alt.match_size > 0:
        QUEX_NAME(seek_backward)(&self,
                                 ${max_match_len - default_alt.match_size});
    % endif

    }
    RETURN;
}
