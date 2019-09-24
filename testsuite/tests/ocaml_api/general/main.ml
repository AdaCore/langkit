open Libfoolang

let ctx = AnalysisContext.create ()

let pp_diag fmt {Diagnostic.message} = Format.pp_print_string fmt message

let pp_diags fmt diags =
  Format.fprintf fmt "@[<v>%a@ @]" (Format.pp_print_list pp_diag) diags

let print_exit_if_diags u =
  let diags = AnalysisUnit.diagnostics u in
  match diags with
  | [] ->
      ()
  | _ ->
      Format.printf "%a" pp_diags diags ;
      exit 1

let root_exn u =
  match AnalysisUnit.root u with
  | Some node ->
      node
  | None ->
      Format.printf "@[<v>unexpected None node@ @]" ;
      exit 1

let pp_image fmt node = Format.pp_print_string fmt (FooNode.short_image node)

let pp_image_opt fmt = function
  | Some node ->
      Format.pp_print_string fmt (FooNode.short_image node)
  | None ->
      Format.pp_print_string fmt "None"

let pp_text fmt node = Format.pp_print_string fmt (FooNode.text node)

let pp_entity fmt node = Format.pp_print_string fmt (FooNode.entity_image node)

let () =
  (* Diagnostics *)
  Format.printf "@[<v>=======DIAGNOSTICS=======@ @]" ;
  (* No file *)
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_file ctx "unknown" in
  Format.printf "%a" pp_diags (AnalysisUnit.diagnostics u) ;
  (* Parsing error *)
  let u = AnalysisContext.get_from_buffer ctx "foo.txt" "var identifier" in
  Format.printf "%a" pp_diags (AnalysisUnit.diagnostics u) ;
  Format.printf "@[<v>=========================@ @ @]"

let pp_token_info fmt with_trivia =
  (* read foo.txt and print information, with or without trivia about tokens *)
  let ctx = AnalysisContext.create ~with_trivia () in
  let u = AnalysisContext.get_from_file ~reparse:true ctx "foo.txt" in
  print_exit_if_diags u ;
  let root = root_exn u in
  let first_token = AnalysisUnit.first_token u in
  let last_token = AnalysisUnit.last_token u in
  let token_start = FooNode.token_start root in
  let token_end = FooNode.token_end root in
  let token_count = AnalysisUnit.token_count u in
  let trivia_count = AnalysisUnit.trivia_count u in
  Format.fprintf fmt "@[<v>@[<v 2>root text=@ %S@]@ @]" (FooNode.text root) ;
  Format.fprintf fmt
    "@[<v>first_token = %S@ last_token = %S@ token_start = %S@ token_end = \
     %S@ token_count = %d@ trivia_count = %d@]"
    first_token.text last_token.text token_start.text token_end.text
    token_count trivia_count

let () =
  (* Test token functions *)
  Format.printf "@[<v>=======TOKEN FUNCTIONS=======@ @]" ;
  Format.printf
    "@[<v>@[<v 2>With trivia:@ %a@]@ @ @[<v 2>Without trivia:@ %a@]@ @]"
    pp_token_info true pp_token_info false ;
  Format.printf "@[<v>=============================@ @ @]"

let () =
  (* Test Token iterators *)
  Format.printf "@[<v>=======TOKEN ITERATORS=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_file ~reparse:true ctx "foo.txt" in
  print_exit_if_diags u ;
  let pp_tok fmt tok = Format.fprintf fmt "%S" tok.Token.text in
  let pp_toks = Format.pp_print_list pp_tok in
  let root = root_exn u in
  (* Test on Analysis unit *)
  (* Fold *)
  let aux acc tok = acc @ [tok.Token.text] in
  Format.printf "@[<v>@[<v 2>Unit fold tokens:@ %a@]@ @ @]"
    (Format.pp_print_list (fun fmt -> Format.fprintf fmt "%S"))
    (AnalysisUnit.fold_tokens aux [] u) ;
  (* Iter *)
  let aux tok = Format.printf "%S@ " tok.Token.text in
  Format.printf "@[<v>@[<v 2>Unit iter tokens:@ " ;
  AnalysisUnit.iter_tokens aux u ;
  Format.printf "@]@ @]" ;
  (* Map *)
  let aux tok = tok.Token.text in
  Format.printf "@[<v>@[<v 2>Unit map tokens:@ %a@]@ @ @]"
    (Format.pp_print_list (fun fmt -> Format.fprintf fmt "%S"))
    (AnalysisUnit.map_tokens aux u) ;
  (* Tokens *)
  Format.printf "@[<v>@[<v 2>Unit token list:@ %a@]@ @ @]" pp_toks
    (AnalysisUnit.tokens u) ;
  (* Test on Node *)
  (* Fold *)
  let aux acc tok = acc @ [tok.Token.text] in
  Format.printf "@[<v>@[<v 2>Root node fold tokens:@ %a@]@ @ @]"
    (Format.pp_print_list (fun fmt -> Format.fprintf fmt "%S"))
    (FooNode.fold_tokens aux [] root) ;
  (* Iter *)
  let aux tok = Format.printf "%S@ " tok.Token.text in
  Format.printf "@[<v>@[<v 2>Root node iter tokens:@ " ;
  FooNode.iter_tokens aux root ;
  Format.printf "@]@ @]" ;
  (* Map *)
  let aux tok = tok.Token.text in
  Format.printf "@[<v>@[<v 2>Root node map tokens:@ %a@]@ @ @]"
    (Format.pp_print_list (fun fmt -> Format.fprintf fmt "%S"))
    (FooNode.map_tokens aux root) ;
  (* Tokens *)
  Format.printf "@[<v>@[<v 2>Root node token list:@ %a@]@ @]" pp_toks
    (FooNode.tokens root) ;
  Format.printf "@[<v>=============================@ @ @]"

let () =
  Format.printf "@[<v>=======NODE ITERATORS=======@ @]" ;
  (* Test all iterators *)
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_file ~reparse:true ctx "foo.txt" in
  print_exit_if_diags u ;
  let root = root_exn u in
  (* Test iterators on fields *)

  (* children_opt *)
  Format.printf "@[<v>@[<v 2>Children:@ %a@]@ @ @]"
    (Format.pp_print_list pp_image_opt)
    (FooNode.children_opt root) ;
  (* fold *)
  let aux acc node = acc ^ "\n  " ^ FooNode.short_image node in
  Format.printf "Fold fields: %s\n\n%!" (FooNode.fold_fields aux "" root) ;
  (* iter *)
  let aux node = Format.printf "%s@ " (FooNode.short_image node) in
  Format.printf "@[<v>@[<v 2>Iter fields:@ " ;
  FooNode.iter_fields aux root ;
  Format.printf "@]@ @]" ;
  (* for all and exists *)
  let aux node1 =
    FooNode.exists_fields (fun node2 -> FooNode.equal node1 node2) root
  in
  Format.printf
    "@[<v>proposition (for all fields f1 of root. exists fields f2 of root. \
     f1 = f2) is %b@ @ @]"
    (FooNode.for_all_fields aux root) ;
  let rec pp_node fmt (name, node) =
    match node with
    | Some node ->
        Format.fprintf fmt "@[<v 2>%s: %a@ %a@]" name pp_image node
          (Format.pp_print_list pp_node)
          (FooNode.fields_with_names node)
    | None ->
        Format.fprintf fmt "%s: None" name
  in
  Format.printf "@[<v>fields_with_names:@ %a@ @]" pp_node ("root", Some root) ;
  Format.printf "@[<v>pp_tree:@ %a@ @]" FooNode.pp_tree root ;
  Format.printf "@[<v>@ @]" ;
  (* Test tree iterators *)
  (* fold *)
  let aux acc node = acc ^ "\n  " ^ FooNode.short_image node in
  Format.printf "Fold: %s\n\n%!" (FooNode.fold aux "" root) ;
  (* iter *)
  let aux node = Format.printf "%s@ " (FooNode.short_image node) in
  Format.printf "@[<v>@[<v 2>Iter:@ " ;
  FooNode.iter aux root ;
  Format.printf "@]@ @]" ;
  (* for all and exists *)
  let aux node1 =
    FooNode.exists (fun node2 -> FooNode.equal node1 node2) root
  in
  Format.printf
    "@[<v>proposition (for all node n1. exists node n2. n1 = n2) is %b@ @]"
    (FooNode.for_all aux root) ;
  Format.printf "@[<v>============================@ @ @]"

let () =
  Format.printf "@[<v>=======PARENT(S)=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_file ~reparse:true ctx "foo.txt" in
  print_exit_if_diags u ;
  let root = root_exn u in
  let equal = List.for_all2 FooNode.equal in
  (* test parents *)
  let rec aux acc node =
    let acc = node :: acc in
    let parents = FooNode.parents node in
    Format.printf "@[<v>@[<v 2>%a parents:@ %a@]@ @ @]" pp_image node
      (Format.pp_print_list pp_image)
      parents ;
    if not (equal parents acc) then ( Format.printf "ERROR\n%!" ; exit 1 ) ;
    FooNode.iter_fields (aux acc) node
  in
  aux [] root ;
  (* test parent *)
  let aux node =
    FooNode.for_all_fields
      (fun child ->
        match FooNode.parent child with
        | Some parent ->
            FooNode.equal node parent
        | None ->
            FooNode.equal child root )
      node
  in
  Format.printf
    "@[<v>proposition (for all node n. for fields fields f of n. parent f = \
     n) is %b@ @]"
    (FooNode.for_all aux root) ;
  Format.printf "@[<v>=======================@ @ @]"

let () =
  Format.printf "@[<v>=======SIBLINGS=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_file ~reparse:true ctx "foo.txt" in
  print_exit_if_diags u ;
  let root = root_exn u in
  (* Previous and next sibling *)
  let equal opt1 opt2 =
    match (opt1, opt2) with
    | None, None ->
        true
    | Some x, Some y ->
        FooNode.equal x y
    | _ ->
        false
  in
  let rec aux acc node =
    let previous = FooNode.previous_sibling node in
    Format.printf "@[<v>%a previous sibling is %a\n%!" pp_image node
      pp_image_opt previous ;
    if not (equal acc previous) then ( Format.printf "ERROR\n%!" ; exit 1 ) ;
    match FooNode.next_sibling node with
    | Some next ->
        aux (Some node) next
    | None ->
        ()
  in
  ( match FooNode.children_opt root with
  | Some h :: _ ->
      aux None h
  | _ ->
      Format.printf "ERROR\n%!" ; exit 1 ) ;
  Format.printf "@[<v>======================@ @ @]"

let () =
  Format.printf "@[<v>=======ARRAY=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let u =
    AnalysisContext.get_from_buffer ctx "foo.txt"
      "example null null example var (example null example)"
  in
  print_exit_if_diags u ;
  let root = root_exn u in
  let examples = FooNode.findall Example root in
  Format.printf "@[<v>p_count: %d@ @]" (FooNode.p_count root examples) ;
  ( match FooNode.as_a Sequence root with
  | Some seq ->
      let pp_all_items : Format.formatter -> FooNode.t list -> unit =
        Format.pp_print_list pp_image
      in
      let pp_example_items : Format.formatter -> Example.t list -> unit =
        Format.pp_print_list pp_image
      in
      Format.printf "@[<v>@[<v 2>all_items:@ %a@]@ @ @]" pp_all_items
        (Sequence.p_all_items seq) ;
      Format.printf "@[<v>@[<v 2>example_items:@ %a@]@ @]" pp_example_items
        (Sequence.p_example_items seq)
  | None ->
      Format.printf "Expect a Sequence\n%!" ;
      exit 1 ) ;
  Format.printf "@[<v>===================@ @ @]"

let () =
  Format.printf "@[<v>=======SYMBOL=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_buffer ctx "foo.txt" "my_ident" in
  print_exit_if_diags u ;
  let root = root_exn u in
  let pp text fmt ident =
    try Format.fprintf fmt "= %s" (Ident.p_sym ident text)
    with InvalidSymbolError s ->
      Format.fprintf fmt "raise <InvalidSymbolError: %s>" s
  in
  let print_sym text =
    let idents = FooNode.findall Ident root in
    Format.printf "@[<v>p_sym root %s %a@ @]" text
      (Format.pp_print_list (pp text))
      idents
  in
  print_sym "my_ident" ;
  print_sym "MY_IDENT" ;
  print_sym "no_such_symbol" ;
  print_sym "invalid_symbol0" ;
  Format.printf "@[<v>====================@ @ @]"

let () =
  Format.printf "@[<v>=======UNICODE=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let pp_string_lit fmt node =
    Format.fprintf fmt "%S" (FooNode.find StringLiteral node |> FooNode.text)
  in
  let src_buffer_iso_8859_1 = "\"H\xe9llo w\xf6rld!\"" in
  let src_buffer_utf_8 = "\"H\xc3\xa8llo w\xc3\xb5rld!\"" in
  let u = AnalysisContext.get_from_buffer ctx "foo.txt" src_buffer_utf_8 in
  print_exit_if_diags u ;
  let root = root_exn u in
  Format.printf "@[<v>@[<2>Default charset:@ %a@]@ @]" pp_string_lit root ;
  let u =
    AnalysisContext.get_from_buffer ~charset:"utf-8" ctx "foo.txt"
      src_buffer_utf_8
  in
  print_exit_if_diags u ;
  let root = root_exn u in
  Format.printf "@[<v>@[<2>UTF-8 charset:@ %a@]@ @]" pp_string_lit root ;
  let u =
    AnalysisContext.get_from_buffer ~charset:"iso-8859-1" ctx "foo.txt"
      src_buffer_iso_8859_1
  in
  print_exit_if_diags u ;
  let root = root_exn u in
  Format.printf "@[<v>@[<2>ISO-8859-1 charset:@ %a@]@ @]" pp_string_lit root ;
  Format.printf "@[<v>=====================@ @ @]"

let () =
  Format.printf "@[<v>=======CHARACTER=======@ @]" ;
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_buffer ctx "foo.txt" "my_ident" in
  print_exit_if_diags u ;
  let root = root_exn u in
  Format.printf
    "@[<v>get_a: %S@ get_eacute: %S@ identity é: %S@ double a: %S@ @]"
    (FooNode.p_get_a root)
    (FooNode.p_get_eacute root)
    (FooNode.p_identity root "é")
    (FooNode.p_double root "a") ;
  Format.printf "@[<v>=======================@ @ @]"

let () =
  Format.printf "@[<v>=======ENUM=======@ @]" ;
  let open Color in
  let ctx = AnalysisContext.create () in
  let u = AnalysisContext.get_from_buffer ctx "foo.txt" "my_ident" in
  print_exit_if_diags u ;
  let root = root_exn u in
  let pp_color fmt = function
    | Red ->
        Format.pp_print_string fmt "Red"
    | Green ->
        Format.pp_print_string fmt "Green"
    | Blue ->
        Format.pp_print_string fmt "Blue"
  in
  let print c =
    Format.printf "@[<v>@[color: %a;@ same_color: %a@]@ @]" pp_color c pp_color
      (FooNode.p_same_color root c)
  in
  print Red ;
  print Green ;
  print Blue ;
  Format.printf "@[<v>=======================@ @]"
