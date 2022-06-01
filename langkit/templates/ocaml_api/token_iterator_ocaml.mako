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
    match ${token_start} node, ${token_end} node with
    | Some tok_start, Some tok_end ->
        let rec aux acc tok_curr =
          let new_acc = f acc tok_curr in
          if Token.equal tok_curr tok_end then
            new_acc
          else (
            match Token.next tok_curr with
            | Some tok_next ->
                aux new_acc tok_next
            | None ->
                new_acc )
        in
        aux init tok_start
    | _ ->
        init

  let iter_tokens f node =
    match ${token_start} node, ${token_end} node with
    | Some tok_start, Some tok_end ->
        let rec aux tok_curr =
          f tok_curr;
          if not (Token.equal tok_curr tok_end) then (
            match Token.next tok_curr with
            | Some tok_next ->
                aux tok_next
            | None ->
                () )
        in
        aux tok_start
    | _ ->
        ()

  let map_tokens f node =
    match ${token_start} node, ${token_end} node with
    | Some tok_start, Some tok_end ->
        let rec aux tok_curr =
          let value = f tok_curr in
          if Token.equal tok_curr tok_end then
            [value]
          else (
            match Token.next tok_curr with
            | Some tok_next ->
                value :: aux tok_next
            | None ->
                [value] )
        in
        aux tok_start
    | _ ->
        []

  let tokens node =
    map_tokens (fun x -> x) node
</%def>

