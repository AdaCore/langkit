<%namespace name="struct_types"   file="struct_types_ocaml.mako"/>
<%namespace name="array_types"    file="array_types_ocaml.mako"/>
<%namespace name="astnode_types"  file="astnode_types_ocaml.mako"/>
<%namespace name="token_iterator" file="token_iterator_ocaml.mako" />
<%namespace name="exts"           file="/extensions.mako" />

<%
  root_entity_type = ocaml_api.type_public_name(root_entity)
%>

open Ctypes
open Foreign

let so_ext = if Sys.win32 || Sys.cygwin then "dll" else "so"
let c_lib_name = Format.sprintf "lib${c_api.shared_object_basename}.%s" so_ext
let c_lib = Dl.dlopen ~filename:c_lib_name ~flags:[Dl.RTLD_NOW]

% for _, exc in ctx.sorted_exception_types:
exception ${exc} of string

% endfor

${exts.include_extension(
   ctx.ext('ocaml_api', 'exceptions')
)}

exception SyntaxError

module Exception = struct

  type t = {
    kind : int;
    information : string;
  }

  let c_struct : t structure typ = structure "exception"
  let kind = field c_struct "kind" int
  let information = field c_struct "information" string
  let () = seal c_struct

  let wrap c_value_ptr =
    if is_null c_value_ptr then
      None
    else
      let c_value = !@ c_value_ptr in
      Some {
        kind = getf c_value kind;
        information = getf c_value information;
      }

  let unwrap value =
    match value with
    | None ->
        from_voidp c_struct null
    | Some value ->
        let c_value = make c_struct in
        setf c_value kind value.kind;
        setf c_value information value.information;
        allocate c_struct c_value

  let c_type = view (ptr c_struct) ~read:wrap ~write:unwrap

end

let get_last_exception = foreign ~from:c_lib
  "${capi.get_name('get_last_exception')}"
  (void @-> returning Exception.c_type)

(* When declaring an imported function with foreign, use raisable instead of
 returning, to check get_last_exception before returning *)
let raisable typ =
  let read value =
    match get_last_exception () with
    | None -> value
    | Some exc ->
        (match exc.kind with
   % for i, (_, exc_type) in enumerate(ctx.sorted_exception_types):
         | ${i} ->
             raise (${exc_type} exc.information)
   % endfor
         | _ -> assert false)
  in
  let write value = value in
  let new_typ = view typ ~read ~write in
  returning new_typ

let initialize = foreign ~from:c_lib "${capi.lib_name}_initialize"
  (void @-> raisable void)

let () = initialize ()

(* Module used to encode/decode UTF32 strings *)
module Camomile = CamomileLibrary.Make (CamomileLibrary.DefaultConfig)

module Text = struct
  type t = string

  let c_struct : t structure typ = structure "text"

  let chars = field c_struct "chars" (ptr uint32_t)

  let length = field c_struct "length" size_t

  let is_allocated = field c_struct "is_allocated" bool

  let () = seal c_struct

  let destroy_text = foreign ~from:c_lib "${capi.get_name('destroy_text')}"
    (ptr c_struct @-> raisable void)

  module UCS4Encoding = Camomile.CharEncoding.Make (Camomile.UCS4)

  let wrap (c_value : t structure) : t =
    let open Unsigned.Size_t in
    let open Camomile in
    let length = to_int (getf c_value length) in
    let chars = getf c_value chars in
    let f i =
      UChar.chr_of_uint (Unsigned.UInt32.to_int !@ (chars +@ i))
    in
    let result = UCS4.init length f in
    (* Now that the value is fully transformed to an ocaml value, we can
      free it by calling destroy_text *)
    destroy_text (addr c_value) ;
    UCS4Encoding.encode CharEncoding.utf8 result

  let unwrap (value : t) : t structure =
    let open Unsigned in
    let open Camomile in
    let text = UCS4Encoding.decode CharEncoding.utf8 value in
    let struct_length = Size_t.of_int (UCS4.length text) in
    let struct_chars = allocate_n uint32_t ~count:(UCS4.length text) in
    let i = ref 0 in
    let f c =
      struct_chars +@ !i <-@ (UInt32.of_int (UChar.code c));
      i := !i + 1
    in
    UCS4.iter f text ;
    let c_value = make c_struct in
    setf c_value length struct_length ;
    setf c_value chars struct_chars ;
    setf c_value is_allocated false ;
    (* We don't need to care about calling destroy_text here since we
     manually allocated the pointer, ctypes will take care of freeing the
     memory *)
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module Character = struct
  (* Characters are encoded as strings because ocaml char are not unicode
   characters *)
  type t = string

  module UCharEncoding = Camomile.CharEncoding.Make (Camomile.UText)

  let wrap (c_value : Unsigned.UInt32.t) : t =
    let open Camomile in
    let uchar = UChar.chr (Unsigned.UInt32.to_int c_value) in
    UCharEncoding.encode CharEncoding.utf8 (UText.init 1 (fun _ -> uchar))

  let unwrap (value : string) : Unsigned.UInt32.t =
    let open Camomile in
    let text = UCharEncoding.decode CharEncoding.utf8 value in
    let uchar = UText.get text 0 in
    Unsigned.UInt32.of_int (UChar.code uchar)

  let c_type = view uint32_t ~read:wrap ~write:unwrap
end

module BigInteger = struct

  (* TODO: use a int for now, should switch to a big integer. *)
  type t = int

  let create = foreign ~from:c_lib "${capi.get_name('create_big_integer')}"
    (ptr Text.c_type @-> raisable (ptr void))

  let text = foreign ~from:c_lib "${capi.get_name('big_integer_text')}"
    (ptr void @-> ptr Text.c_type @-> raisable void)

  let decref = foreign ~from:c_lib "${capi.get_name('big_integer_decref')}"
    (ptr void @-> raisable void)

  let wrap (c_value : unit ptr) : t =
    let c_text_ptr = allocate_n Text.c_type ~count:1 in
    text c_value c_text_ptr;
    decref c_value;
    int_of_string (!@ c_text_ptr)

  let unwrap (value : t) : unit ptr =
    let result = create (allocate Text.c_type (string_of_int value)) in
    (* Register decref to Gc so that the variable is freed when unreachable *)
    Gc.finalise decref result;
    result

  let c_type = view (ptr void) ~read:wrap ~write:unwrap
end

% for enum_type in ctx.enum_types:
module ${ocaml_api.module_name(enum_type)} = struct
  type t =
   % for v in enum_type.values:
  | ${v.name.camel}
   % endfor

  let name () = "${enum_type.api_name.camel}"

  let wrap (c_value : int) : t =
    match c_value with
      % for i, v in enumerate(enum_type.values):
    | ${i} -> ${v.name.camel}
      % endfor
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
      % for i, v in enumerate(enum_type.values):
    | ${v.name.camel} -> ${i}
      % endfor

   let c_type = view int ~read:wrap ~write:unwrap
end

% endfor

let default_grammar_rule = GrammarRule.${ctx.main_rule_api_name.camel}

% for struct_type in ctx.struct_types:
   % if struct_type.is_entity_type:
      % if struct_type == root_entity:
         ## We want only one module defining the entity c structure
   ${struct_types.decl_struct(struct_type)}
      % endif
   % elif struct_type is T.entity_info:
   ${struct_types.decl_struct(struct_type)}
   % elif struct_type is T.env_md:
   ${struct_types.decl_struct(struct_type)}
   % elif struct_type.exposed:
   ${struct_types.decl_struct(struct_type)}
   % endif
% endfor

module Sloc = struct
  type t = {
    line : int;
    column : int;
  }

  let c_struct : t structure typ = structure "sloc"
  let line = field c_struct "line" uint32_t
  let column = field c_struct "column" uint16_t
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    line = Unsigned.UInt32.to_int (getf c_value line);
    column = Unsigned.UInt16.to_int (getf c_value column);
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value line (Unsigned.UInt32.of_int (value.line));
    setf c_value column (Unsigned.UInt16.of_int (value.column));
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module SlocRange = struct
  type t = {
    loc_start : Sloc.t;
    loc_end : Sloc.t;
  }

  let c_struct : t structure typ = structure "sloc_range"
  let loc_start = field c_struct "loc_start" Sloc.c_type
  let loc_end = field c_struct "loc_end" Sloc.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    loc_start = getf c_value loc_start;
    loc_end = getf c_value loc_end;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value loc_start value.loc_start;
    setf c_value loc_end value.loc_end;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap

  let pp fmt sloc_range =
    Format.fprintf fmt "<SlocRange %d:%d-%d:%d>"
      sloc_range.loc_start.line
      sloc_range.loc_start.column
      sloc_range.loc_end.line
      sloc_range.loc_end.column
end

module Diagnostic = struct
  type t = {
    sloc_range : SlocRange.t;
    message : string
  }

  let c_struct : t structure typ = structure "diagnostic"
  let sloc_range = field c_struct "sloc_range" SlocRange.c_type
  let message = field c_struct "message" Text.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    sloc_range = getf c_value sloc_range;
    message = getf c_value message;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value sloc_range value.sloc_range;
    setf c_value message value.message;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module TokenData = struct
  type t = unit ptr
end

module Token = struct
  type t = {
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  let c_struct : t structure typ = structure "token"
  let token_data = field c_struct "token_data" (ptr void)
  let token_index = field c_struct "token_index" int
  let trivia_index = field c_struct "trivia_index" int
  let kind = field c_struct "kind" int
  let text = field c_struct "text" Text.c_type
  let sloc_range = field c_struct "sloc_range" SlocRange.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    token_data = getf c_value token_data;
    token_index = getf c_value token_index;
    trivia_index = getf c_value trivia_index;
    kind = getf c_value kind;
    text = getf c_value text;
    sloc_range = getf c_value sloc_range;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value token_data value.token_data;
    setf c_value token_index value.token_index;
    setf c_value trivia_index value.trivia_index;
    setf c_value kind value.kind;
    setf c_value text value.text;
    setf c_value sloc_range value.sloc_range;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap

  let token_kind_name = foreign ~from:c_lib
    "${capi.get_name('token_kind_name')}"
    (int @-> raisable string)

  let kind_name token = token_kind_name token.kind

  let token_range_text = foreign ~from:c_lib
    "${capi.get_name('token_range_text')}"
    (ptr c_type @-> ptr c_type @-> ptr Text.c_type @-> raisable int)

  let token_next = foreign ~from:c_lib
    "${capi.get_name('token_next')}"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let token_previous = foreign ~from:c_lib
    "${capi.get_name('token_previous')}"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let pp fmt token =
    let pp_text fmt = function
      | "" -> Format.pp_print_string fmt ""
      | _ as text -> Format.fprintf fmt " %S" text
    in
    Format.fprintf fmt "<Token %s%a at %a>"
      (kind_name token)
      pp_text token.text
      SlocRange.pp token.sloc_range

  let text_range token_first token_last =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    let res =
      token_range_text
        (allocate c_type token_first)
        (allocate c_type token_last)
        c_result_ptr
    in
    if res = 0 then
      raise (Invalid_argument
        (Format.asprintf "%a and %a come from different units"
          pp token_first
          pp token_last));
    !@ c_result_ptr

  let next token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_next (allocate c_type token) c_next_token_ptr ;
    !@ c_next_token_ptr

  let previous token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_previous (allocate c_type token) c_next_token_ptr ;
    !@ c_next_token_ptr

  let is_trivia token =
    token.trivia_index != 0

  let index token =
    match token.trivia_index with
    | 0 ->
        token.token_index - 1
    | _ ->
        token.trivia_index - 1

  let compare one other =
    let open Pervasives in
    let compare_token_data = compare one.token_data other.token_data in
    if compare_token_data = 0 then
      let compare_token_index = compare one.token_index other.token_index in
      if compare_token_index = 0 then
        compare one.trivia_index other.trivia_index
      else
        compare_token_index
    else
      compare_token_data

  let equal one other =
    compare one other = 0

  let hash token =
    Hashtbl.hash
      (token.token_data
       , token.token_index
       , token.trivia_index)

  let equiv one other =
    one.kind = other.kind
    && one.text = other.text
end

module AnalysisUnitStruct = struct
  (* Module defining the c structure of an analysis unit *)

  type t = unit ptr
  let c_type = ptr void

  let unit_root = foreign ~from:c_lib "${capi.get_name("unit_root")}"
    (c_type @-> ptr ${ocaml_api.c_type(root_entity)} @-> raisable void)

  let unit_diagnostic_count = foreign ~from:c_lib
    "${capi.get_name('unit_diagnostic_count')}"
    (c_type @-> raisable int)

  let unit_diagnostic = foreign ~from:c_lib
    "${capi.get_name('unit_diagnostic')}"
    (c_type @-> int @-> ptr Diagnostic.c_type @-> raisable int)

  let unit_reparse_from_file = foreign ~from:c_lib
    "${capi.get_name('unit_reparse_from_file')}"
    (c_type
     @-> string
     @-> raisable int)

  let unit_reparse_from_buffer = foreign ~from:c_lib
    "${capi.get_name('unit_reparse_from_buffer')}"
    (c_type
     @-> string
     @-> string
     @-> size_t
     @-> raisable int)

  let unit_first_token = foreign ~from:c_lib
    "${capi.get_name('unit_first_token')}"
    (c_type
     @-> ptr Token.c_type
     @-> raisable void)

  let unit_last_token = foreign ~from:c_lib
    "${capi.get_name('unit_last_token')}"
    (c_type
     @-> ptr Token.c_type
     @-> raisable void)

  let unit_token_count = foreign ~from:c_lib
    "${capi.get_name('unit_token_count')}"
    (c_type @-> raisable int)

  let unit_trivia_count = foreign ~from:c_lib
    "${capi.get_name('unit_trivia_count')}"
    (c_type @-> raisable int)
end

module UnitProvider = struct
  type t = unit ptr
  let c_type = ptr void

  ${exts.include_extension(
     ctx.ext('ocaml_api', 'unit_providers', 'module_struct')
  )}
end

module AnalysisContextStruct = struct
  (* Module defining the c structure of an analysis context *)

  type t = unit ptr
  let c_type = ptr void

  let create_analysis_context = foreign ~from:c_lib
    "${capi.get_name('create_analysis_context')}"
    (string @-> UnitProvider.c_type @-> bool @-> int @-> raisable c_type)

  let context_decref = foreign ~from:c_lib
    "${capi.get_name('context_decref')}"
    (c_type @-> raisable void)

  let get_analysis_unit_from_file = foreign ~from:c_lib
    "${capi.get_name('get_analysis_unit_from_file')}"
    (c_type
     @-> string
     @-> string
     @-> bool
     @-> GrammarRule.c_type
     @-> raisable AnalysisUnitStruct.c_type)

  let get_analysis_unit_from_buffer = foreign ~from:c_lib
    "${capi.get_name('get_analysis_unit_from_buffer')}"
    (c_type
     @-> string (* Filename *)
     @-> string (* Charset *)
     @-> string (* Buffer *)
     @-> size_t (* Buffer size *)
     @-> GrammarRule.c_type
     @-> raisable AnalysisUnitStruct.c_type)

end

module Symbol = struct
  type t = string

  let c_type : t structure typ = structure "symbol"
  let data = field c_type "data" (ptr void)
  let bounds = field c_type "bounds" (ptr void)
  let () = seal c_type

  let symbol_text = foreign ~from:c_lib "${capi.get_name('symbol_text')}"
    (ptr c_type @-> ptr Text.c_type @-> raisable void)

  let wrap (c_value : t structure) : t =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    symbol_text (addr c_value) c_result_ptr;
    !@ c_result_ptr

  let context_symbol = foreign ~from:c_lib "${capi.get_name('context_symbol')}"
    (AnalysisContextStruct.c_type
     @-> ptr Text.c_type
     @-> ptr c_type
     @-> raisable int)

  let unwrap (ctx : AnalysisContextStruct.t) (value : t) : t structure =
    let result = make c_type in
    let code =
      context_symbol ctx (allocate Text.c_type value) (addr result)
    in
    if code = 0 then
      raise (InvalidSymbolError value) ;
    result
end

% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
${array_types.decl_struct(array_type)}
    % endif
% endfor

module BareNode = struct
  type t = unit ptr
end

module Rebindings = struct
  type t = unit ptr
end

module CFunctions = struct
  let node_kind = foreign ~from:c_lib "${capi.get_name('node_kind')}"
    (ptr ${ocaml_api.c_type(root_entity)} @-> raisable int)

  let short_image = foreign ~from:c_lib "${capi.get_name('node_short_image')}"
    (ptr ${ocaml_api.c_type(root_entity)} @-> raisable Text.c_type)

  let node_sloc_range = foreign ~from:c_lib
    "${capi.get_name('node_sloc_range')}"
    (ptr ${ocaml_api.c_type(root_entity)}
     @-> ptr SlocRange.c_type
     @-> raisable void)

  let lookup_in_node = foreign ~from:c_lib
    "${capi.get_name('lookup_in_node')}"
    (ptr ${ocaml_api.c_type(root_entity)}
     @-> ptr Sloc.c_type
     @-> ptr ${ocaml_api.c_type(root_entity)}
     @-> raisable void)

  let entity_image = foreign ~from:c_lib
    "${capi.get_name('entity_image')}"
    (ptr ${ocaml_api.c_type(root_entity)}
     @-> raisable Text.c_type)

  let node_is_token_node = foreign ~from:c_lib
    "${capi.get_name('node_is_token_node')}"
    (ptr ${ocaml_api.c_type(root_entity)}
     @-> raisable bool)

% for astnode in ctx.astnode_types:
   % for field in astnode.fields_with_accessors():
  let ${field.accessor_basename.lower} = foreign ~from:c_lib
    "${capi.get_name(field.accessor_basename)}"
    (ptr ${ocaml_api.c_type(root_entity)}
      % for arg in field.arguments:
        <%
            type_expr = ocaml_api.c_type(arg.public_type)
            if arg.public_type.is_ada_record:
                type_expr = 'ptr {}'.format(type_expr)
        %>
    @-> ${type_expr}
      % endfor
    @-> ptr ${ocaml_api.c_type(field.public_type)}
    @-> raisable int)

   % endfor
% endfor

end

module rec ${ocaml_api.module_name(T.root_node)} : sig
  ${astnode_types.ast_type(T.root_node)}

  val wrap :
    AnalysisContext.t
    -> ${ocaml_api.c_value_type(root_entity)}
    -> ${ocaml_api.type_public_name(T.root_node, T.root_node)}

  val unwrap : [< t ] -> ${ocaml_api.c_value_type(root_entity)}

end = struct
  ${astnode_types.ast_type(T.root_node)}

  let wrap context c_value =
    (* Top level wrap function that dispatch to wrap function of concrete types
      depending on the node kind *)
    if is_null (getf c_value ${ocaml_api.struct_name(root_entity)}.node) then
      raise SyntaxError
    else
      let kind = CFunctions.node_kind (addr c_value) in
      match kind with
% for subclass in ctx.astnode_types:
   % if not subclass.abstract:
      | ${ctx.node_kind_constants[subclass]} ->
          (${ocaml_api.wrap_value('c_value', subclass.entity, 'context')}
           :> ${ocaml_api.type_public_name(T.root_node, T.root_node)})
   % endif
% endfor
      | _ -> assert false

  let unwrap value =
    (* This is the unique unwrap function that can be called for any node. *)
    match (value :> ${root_entity_type}) with
% for subclass in ctx.astnode_types:
   % if not subclass.abstract:
    | ${ocaml_api.polymorphic_variant_name(subclass)} fields -> fields.c_value
   % endif
% endfor

end

% for astnode in ctx.astnode_types:
   % if astnode != T.root_node:
      ## root node is defined separately
      ${astnode_types.decl_type(astnode)}
   % endif
% endfor

and Entity : sig
  type t = ${ocaml_api.c_value_type(root_entity)}

  val info : t -> ${ocaml_api.type_public_name(T.entity_info)}

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int
end = struct
  type t = ${ocaml_api.c_value_type(root_entity)}

  let info value =
    ${ocaml_api.wrap_value("getf value {}.info".format(
         ocaml_api.struct_name(root_entity)), T.entity_info, None)}

  let compare e1 e2 =
    let open Pervasives in
    let compare_node =
      compare (getf e1 EntityStruct.node) (getf e2 EntityStruct.node)
    in
    if compare_node = 0 then
      compare
        (getf (getf e1 EntityStruct.info) EntityInfoStruct.rebindings)
        (getf (getf e2 EntityStruct.info) EntityInfoStruct.rebindings)
    else
      compare_node

  let equal e1 e2 =
    compare e1 e2 = 0

  let hash e =
    Hashtbl.hash
      ( getf e EntityStruct.node
      , getf (getf e EntityStruct.info) EntityInfoStruct.rebindings )
end

and AnalysisUnit : sig
  type t = {
    c_value : AnalysisUnitStruct.t;
    context : AnalysisContext.t
  }

  val root : t -> ${root_entity_type} option
  val diagnostics : t -> Diagnostic.t list
  val reparse : ?charset:string -> ?buffer:string -> t -> unit
  val first_token : t -> Token.t
  val last_token : t -> Token.t
  val token_count : t -> int
  val trivia_count : t -> int

  ${token_iterator.sig("t")}

  val wrap : AnalysisContext.t -> AnalysisUnitStruct.t -> t
  val unwrap : t -> AnalysisUnitStruct.t
end = struct
  type t = {
    c_value : AnalysisUnitStruct.t;
    context : AnalysisContext.t
  }

  let root analysis_unit =
    let c_value = make ${ocaml_api.c_type(root_entity)} in
    AnalysisUnitStruct.unit_root
      (${ocaml_api.unwrap_value("analysis_unit", T.AnalysisUnit, None)})
      (addr c_value);
    ${ocaml_api.check_for_null('c_value', root_entity,
        "analysis_unit.context")}

  let diagnostics unit =
    let c_unit = ${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)} in
    let length = AnalysisUnitStruct.unit_diagnostic_count c_unit in
    let f i =
      let diag = allocate_n Diagnostic.c_type ~count:1 in
      let _ : int = AnalysisUnitStruct.unit_diagnostic c_unit i diag in
      !@ diag
    in
    List.init length f

  let reparse ?charset:(charset="") ?buffer ctx =
    match buffer with
    | None ->
        ignore
          (AnalysisUnitStruct.unit_reparse_from_file ctx.c_value charset)
    | Some buffer ->
        ignore (AnalysisUnitStruct.unit_reparse_from_buffer ctx.c_value
          charset buffer (Unsigned.Size_t.of_int (String.length buffer)))

  let first_token unit =
    let c_unit = ${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)} in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_first_token c_unit result_ptr ;
    !@ result_ptr

  let last_token unit =
    let c_unit = ${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)} in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_last_token c_unit result_ptr ;
    !@ result_ptr

  let token_count unit =
    AnalysisUnitStruct.unit_token_count
      (${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)})

  let trivia_count unit =
    AnalysisUnitStruct.unit_trivia_count
      (${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)})

  ${token_iterator.struct("first_token", "last_token")}

  let wrap context c_value = {
    c_value=c_value;
    context=context;
  }

  let unwrap {c_value} = c_value
end

and AnalysisContext : sig
  type t = {
    c_value : AnalysisContextStruct.t;
    unit_provider : UnitProvider.t option
  }

  val create :
    ?charset:string
    -> ?with_trivia:bool
    -> ?tab_stop:int
    -> ?unit_provider:UnitProvider.t
    -> unit
    -> t

  val get_from_file :
    ?charset:string
    -> ?reparse:bool
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> AnalysisUnit.t

  val get_from_buffer :
    ?charset:string
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> string
    -> AnalysisUnit.t
end = struct
  type t = {
    c_value : AnalysisContextStruct.t;
    unit_provider : UnitProvider.t option
  }

  let create
    ?charset:(charset="")
    ?with_trivia:(with_trivia=true)
    ?tab_stop:(tab_stop=${ctx.default_tab_stop})
    ?unit_provider () : t =
    if tab_stop < 1 then
      raise (Invalid_argument "Invalid tab_stop (positive integer expected)") ;
    let c_context =
      match unit_provider with
      | Some provider ->
          AnalysisContextStruct.create_analysis_context
            charset
            provider
            with_trivia
            tab_stop
      | None ->
          AnalysisContextStruct.create_analysis_context
            charset
            null
            with_trivia
            tab_stop
    in
    let context = {
      c_value = c_context;
      unit_provider = unit_provider
    }
    in
    Gc.finalise
      (fun x -> AnalysisContextStruct.context_decref x.c_value)
      context;
    context

  let get_from_file
    ?charset:(charset="")
    ?reparse:(reparse=false)
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    ctx
    filename : AnalysisUnit.t =

    ${ocaml_api.wrap_value(
      "AnalysisContextStruct.get_analysis_unit_from_file ctx.c_value"
      + " filename charset reparse grammar_rule", T.AnalysisUnit, "ctx")}

  let get_from_buffer
    ?charset:(charset="")
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    ctx
    filename
    buffer : AnalysisUnit.t =

    ${ocaml_api.wrap_value(
      "AnalysisContextStruct.get_analysis_unit_from_buffer ctx.c_value"
      + " filename charset buffer"
      + " (Unsigned.Size_t.of_int (String.length buffer)) grammar_rule"
      , T.AnalysisUnit, "ctx")}
end

% for struct_type in ctx.struct_types:
   % if not struct_type.is_entity_type:
      % if struct_type is T.entity_info:
   ${struct_types.decl_wrapper(struct_type)}
      % elif struct_type is T.env_md:
   ${struct_types.decl_wrapper(struct_type)}
      % elif struct_type.exposed:
   ${struct_types.decl_wrapper(struct_type)}
      % endif
   % endif
% endfor

% for array_type in ctx.array_types:
    % if array_type.exposed:
${array_types.decl_wrapper(array_type)}
    % endif
% endfor

let context node =
  (* Given any node, extract the context field *)
  match (node :> ${root_entity_type}) with
   % for astnode in ctx.astnode_types:
      % if not astnode.abstract:
  | ${ocaml_api.polymorphic_variant_name(astnode)} fields -> fields.context
      % endif
   % endfor

type _ node =
% for astnode in ctx.astnode_types:
  | ${ocaml_api.node_name(astnode)} :
      ${ocaml_api.type_public_name(astnode)} node
% endfor

% for astnode in reversed(ctx.astnode_types):
module ${ocaml_api.node_name(astnode)} = struct
  type t =
   % if astnode.abstract:
    [
      % for child in astnode.subclasses:
      | ${ocaml_api.node_name(child)}.t
      % endfor
    ]
   % else:
    [
      % for child in astnode.concrete_subclasses:
      | ${ocaml_api.polymorphic_variant_name(child)} of
          ${ocaml_api.fields_name(child)}
      % endfor
    ]
   % endif

   % if not astnode.abstract:
  type fields = ${ocaml_api.fields_name(astnode)} =
    ${astnode_types.field_type(astnode)}
   % endif

  let equal node1 node2 =
    Entity.equal
      (${ocaml_api.unwrap_value('(node1 :> {})'.format(root_entity_type),
                                astnode.entity, None)})
      (${ocaml_api.unwrap_value('(node2 :> {})'.format(root_entity_type),
                                astnode.entity, None)})

  let compare node1 node2 =
    Entity.compare
      (${ocaml_api.unwrap_value('(node1 :> {})'.format(root_entity_type),
                                astnode.entity, None)})
      (${ocaml_api.unwrap_value('(node2 :> {})'.format(root_entity_type),
                                astnode.entity, None)})

  let hash node =
    Entity.hash
      (${ocaml_api.unwrap_value('(node :> {})'.format(root_entity_type),
                                astnode.entity, None)})

   % for field in ocaml_api.get_properties(astnode):
let ${field.name.lower}
      % for arg in field.arguments:
         % if arg.default_value is not None:
            % if arg.public_type.is_entity_type:
            ## For entity type, use a default argument which is expanded to an
            ## option type in ocaml. This allows user to pass the node without
            ## having to write (~node=(Some node)), they can simply write
            ## ~node.
    ?${arg.name.lower}
            % else:
    ?(${arg.name.lower}=${arg.default_value.render_ocaml_constant()})
            % endif
         % endif
      % endfor
    (node)
      % for arg in field.arguments:
         % if arg.default_value is None:
    (${arg.name.lower})
         % endif
      % endfor
    =
      let result_ptr =
        allocate_n ${ocaml_api.c_type(field.public_type)} ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.${field.accessor_basename.lower}
          (addr (${ocaml_api.unwrap_value('node', astnode.entity, None)}))
      % for arg in field.arguments:
         % if arg.default_value is not None and arg.public_type.is_entity_type:
         ## for entity default argument with use the optional label
         ## construction that hides the fact that an ommited parameter is the
         ## value None.
           (match ${arg.name.lower} with
           | Some node ->
               addr (${ocaml_api.unwrap_value('node', arg.public_type,
                                              '(context node).c_value')})
           | None -> allocate_n ${ocaml_api.c_type(root_entity)} ~count:1)
         % else:
            <%
               value = ocaml_api.unwrap_value(arg.name.lower, arg.public_type,
                                              '(context node).c_value')
               if arg.public_type.is_ada_record:
                  if ocaml_api.is_struct(arg.public_type):
                     value = 'addr ({})'.format(value)
                  else:
                     value = 'allocate {} ({})'.format(
                        ocaml_api.c_type(arg.public_type), value)
            %>
          (${value})
         % endif
      % endfor
          (result_ptr)
      in
      % if field.public_type.is_entity_type:
      ## For entity types, we return an optional argument instead of calling
      ## the wrapper, that would raise an exception.
      ${ocaml_api.check_for_null('!@ result_ptr', field.public_type,
                                 '(context node)')}
      % else:
      ${ocaml_api.wrap_value('!@ result_ptr', field.public_type,
                             '(context node)')}
      % endif

   % endfor

   % for field in ocaml_api.get_parse_fields(astnode):
  let ${field.name.lower} node =
    match (node :> ${ocaml_api.type_public_name(astnode)}) with
      % for concrete in astnode.concrete_subclasses:
    | ${ocaml_api.polymorphic_variant_name(concrete)} fields ->
        Lazy.force fields.${field.name.lower}
      % endfor
   % endfor

   % if astnode.is_list:
  let f_list node =
    match (node :> ${ocaml_api.type_public_name(astnode)}) with
      % for concrete in astnode.concrete_subclasses:
    | ${ocaml_api.polymorphic_variant_name(concrete)} fields ->
        Lazy.force fields.list
      % endfor

   % endif

   % if astnode == T.root_node:

  let kind_name = function
      % for astnode in reversed(ctx.astnode_types):
         ## We walk in reverse because some concrete types have subclasses,
         ## but we want the subclasses to appear first.
         % if not astnode.abstract:
    | #${ocaml_api.type_public_name(astnode)} ->
        "${ocaml_api.node_name(astnode)}"
         % endif
      % endfor

  let text node =
    Token.text_range (token_start node) (token_end node)

  let short_image node =
    CFunctions.short_image
      (addr (${ocaml_api.unwrap_value('node', root_entity, 'context node')}))

  let entity_image node =
    let node_c_value = ${ocaml_api.unwrap_value('node', root_entity, None)} in
    CFunctions.entity_image (addr node_c_value)

  let is_token_node node =
    let node_c_value = ${ocaml_api.unwrap_value('node', root_entity, None)} in
    CFunctions.node_is_token_node (addr node_c_value)

  let sloc_range node =
    let c_result_ptr = allocate_n SlocRange.c_type ~count:1 in
    CFunctions.node_sloc_range
      (addr (${ocaml_api.unwrap_value('node', root_entity, 'context node')}))
      c_result_ptr;
    !@ c_result_ptr

  ${token_iterator.struct("token_start", "token_end")}

  let lookup node sloc =
    let node_c_value = ${ocaml_api.unwrap_value('node', root_entity, None)} in
    let sloc_ptr = allocate Sloc.c_type sloc in
    let result_ptr = allocate_n ${ocaml_api.c_type(root_entity)} ~count:1 in
    CFunctions.lookup_in_node
      (addr node_c_value) sloc_ptr result_ptr;
    ${ocaml_api.check_for_null('!@ result_ptr', root_entity, '(context node)')}

  let entity_image node =
    let node_c_value = ${ocaml_api.unwrap_value('node', root_entity, None)} in
    CFunctions.entity_image (addr node_c_value)

  let children_opt node =
    let node_c_value = ${ocaml_api.unwrap_value('node', root_entity, None)} in
    let context = context node in
    let c_value_ptr = allocate_n ${ocaml_api.c_type(T.entity.array)} ~count:1 in
    let _ : int =
      CFunctions.${c_api.get_name('node_children')}
        (addr node_c_value)
        (c_value_ptr)
    in
    let c_value = !@(!@(c_value_ptr)) in
    let length = getf c_value ${ocaml_api.struct_name(T.entity.array)}.n in
    let items = c_value @. ${ocaml_api.struct_name(T.entity.array)}.items in
    let f i =
      let fresh = allocate EntityStruct.c_type !@(items +@ i) in
      ${ocaml_api.check_for_null('!@ fresh', root_entity, 'context')}
    in
    let result = List.init length f in
    ${ocaml_api.struct_name(T.entity.array)}.dec_ref (!@ c_value_ptr);
    result

  ## TODO: check if it is better to implement this forcing the ocaml fields
  let iter_fields f node =
    children_opt (node :> ${root_entity_type})
    |> List.iter (function None -> () | Some node -> f node)

  let fold_fields f acc node =
    children_opt (node :> ${root_entity_type})
    |> List.fold_left (fun x -> function None -> x | Some node -> f x node) acc

  let exists_fields p node =
    children_opt (node :> ${root_entity_type})
    |> List.exists (function | None -> false | Some node -> p node)

  let for_all_fields p node =
    children_opt (node :> ${root_entity_type})
    |> List.for_all (function | None -> true | Some node -> p node)

  let fold f acc node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux acc node = fold_fields aux (f acc node) node in
    aux acc (node :> ${root_entity_type})

  let iter f node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node = f node; iter_fields aux node in
    aux (node :> ${root_entity_type})

  let filter p node =
    fold (fun acc node -> if p node then node :: acc else acc) [] node
    |> List.rev

  let exists p node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node =
      p node || exists_fields aux node in aux (node :> ${root_entity_type})

  let for_all p node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node = p node && for_all_fields aux node in
    aux (node :> ${root_entity_type})

  let as_a : type a. a node -> [< ${root_entity_type} ] -> a option =
   fun node_type node ->
    match node_type, (node :> ${root_entity_type}) with
   % for astnode in ctx.astnode_types:
    | ${ocaml_api.node_name(astnode)}
      , (#${ocaml_api.type_public_name(astnode)} as node) ->
        Some node
   % endfor
    | _ ->
        None

  let find : type a. a node ->  [< ${root_entity_type} ] -> a =
    fun node_type node ->
      let exception Found of a in
      let aux node =
        match node_type, node with
      % for astnode in ctx.astnode_types:
        | ${ocaml_api.node_name(astnode)}
          , (#${ocaml_api.type_public_name(astnode)} as node) ->
            raise (Found node)
      % endfor
        | _ ->
          ()
      in
      try
        iter aux node;
        raise Not_found
      with (Found node) -> node



  let findall : type a. a node ->  [< ${root_entity_type} ] -> a list =
    fun node_type node ->
      let aux : a list -> [< ${root_entity_type} ] -> a list =
       fun acc node ->
        match node_type, node with
      % for astnode in ctx.astnode_types:
        | ${ocaml_api.node_name(astnode)}
          , (#${ocaml_api.type_public_name(astnode)} as node) ->
            node :: acc
      % endfor
        | _ ->
          acc
      in
      List.rev (fold aux [] node)

  let fields_with_names node =
    let aux i x =
      (Format.sprintf "item_%d" i), x
    in
    match (node :> ${root_entity_type}) with
      % for astnode in reversed(ctx.astnode_types):
         % if not astnode.abstract:
    | ${ocaml_api.polymorphic_variant_name(astnode)} value ->
            % if astnode.is_list:
        List.mapi aux (children_opt node)
            % else:
        [
               % for field in ocaml_api.get_parse_fields(astnode):
                  % if field.is_optional:
        ("${field.name.lower[2:]}"
        , (Lazy.force value.${field.name.lower}
           :> ${root_entity_type} option));
                  % else:
        (try
           ("${field.name.lower[2:]}"
           , Some (Lazy.force value.${field.name.lower}
                    :> ${root_entity_type}))
        with SyntaxError ->
          ("${field.name.lower[2:]}", None) );
                  % endif
               % endfor
        ]
            % endif
         % endif
      % endfor

  let rec pp_tree fmt node =
    let rec pp_node_field fmt (name, node) =
      match node with
      | Some node ->
          Format.fprintf fmt "@[<v 2>%s:@ %a@]" name pp_node node
      | None ->
          Format.fprintf fmt "@[<v 2>%s: None@]" name
    and pp_node_fields fmt node =
      let name_field_list = fields_with_names node in
      match name_field_list with
      | [] ->
          ()
      | l ->
          Format.fprintf fmt "@ @[<v>%a@]"
            (Format.pp_print_list pp_node_field) l
    and pp_node fmt node =
      let repr = entity_image node in
      let len = String.length repr in
      let erepr = String.sub repr 1 (len - 2) in
      Format.fprintf fmt "@[<v 2>%s%s%a@]"
        erepr
        (if is_token_node node then (": " ^ (text node)) else "")
        pp_node_fields node
    in
    let default = Format.pp_get_formatter_out_functions fmt () in
    let out_indent n =
      let the_end = n in
      let rec make n =
        if n = the_end then ""
        else (if n mod 4 = 2 then "|" else " ") ^ make (n + 1)
      in
      default.out_string (make 0) 0 n
    in
    Format.pp_set_formatter_out_functions fmt {default with out_indent} ;
    Format.fprintf fmt "%a%!" pp_node (node :> ${root_entity_type});
    Format.pp_set_formatter_out_functions fmt default

   % endif

end

% endfor
