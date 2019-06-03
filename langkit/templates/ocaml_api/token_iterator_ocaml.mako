<%def name="sig(arg_type)">
  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> ${arg_type} -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> ${arg_type}-> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> ${arg_type} -> 'a list
  (**
   * Map each token calling the given function
   *)

  val tokens : ${arg_type} -> Token.t list
  (**
   * Return a list of tokens for the given node
   *)
</%def>

<%def name="struct(token_start, token_end)">
  let fold_tokens f init node =
    let tok_start = ${token_start} node in
    let tok_end = ${token_end} node in
    let rec aux acc tok_curr =
      let new_acc = f acc tok_curr in
      if Token.equal tok_curr tok_end then
        new_acc
      else
        aux new_acc (Token.next tok_curr)
    in
    aux init tok_start

  let iter_tokens f node =
    let tok_start = ${token_start} node in
    let tok_end = ${token_end} node in
    let rec aux tok_curr =
      f tok_curr;
      if not (Token.equal tok_curr tok_end) then
        aux (Token.next tok_curr)
    in
    aux tok_start

  let map_tokens f node =
    let tok_start = ${token_start} node in
    let tok_end = ${token_end} node in
    let rec aux tok_curr =
      let value = f tok_curr in
      if Token.equal tok_curr tok_end then
        [value]
      else
        value :: aux (Token.next tok_curr)
    in
    aux tok_start

  let tokens node =
    map_tokens (fun x -> x) node
</%def>

