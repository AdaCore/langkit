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

(* Under Linux, disable GNAT's handling of SIGSEGV, which is incompatible with
   what the OCaml runtime is already doing. *)
let () =
  if Sys.unix then
    ignore
      (Dl.dlopen
        ~filename:"liblangkit_sigsegv_handler.so"
        ~flags:[Dl.RTLD_NOW]
        : Dl.library)

let so_ext = if Sys.unix then "so" else "dll"
let c_lib_name = Format.sprintf "lib${c_api.shared_object_basename}.%s" so_ext
let c_lib = Dl.dlopen ~filename:c_lib_name ~flags:[Dl.RTLD_NOW]

let add_gc_link ~from ~to_ =
  let r = ref (Some (Obj.repr to_)) in
  let finaliser _ = r := None in
  Gc.finalise finaliser from

% for e in ctx.sorted_exception_types:
exception ${e.name} of string

% endfor

${exts.include_extension(
   ctx.ext('ocaml_api', 'exceptions')
)}

let char_ptr_of_string str =
  coerce string (ptr char) str

let string_of_char_ptr str =
  coerce (ptr char) string str

module Exception = struct

  type t = {
    kind : int;
    information : string;
  }

  let c_struct : t structure typ = structure "exception"
  let kind = field c_struct "kind" int
  let information = field c_struct "information" (ptr char)
  let () = seal c_struct

  let wrap c_value_ptr =
    if is_null c_value_ptr then
      None
    else
      let c_value = !@ c_value_ptr in
      let c_information = getf c_value information in
      (* eng/codepeer/infer#220: Avoid a crash when the "information" field is
         null. This is never supposed to happen, but has been observed to
         happen randomly. For now, consider that there is no exception when
         this is the case. *)
      let information =
        if is_null c_information then
          ""
        else
          string_of_char_ptr c_information
      in
      Some {
        kind = getf c_value kind;
        information
      }

  let unwrap value =
    match value with
    | None ->
        from_voidp c_struct null
    | Some value ->
        let c_value = make c_struct in
        let c_information = char_ptr_of_string value.information in
        setf c_value kind value.kind;
        add_gc_link ~from:c_value ~to_:c_information;
        setf c_value information c_information;
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
   % for i, e in enumerate(ctx.sorted_exception_types):
         | ${i} ->
             raise (${e.name} exc.information)
   % endfor
         | _ -> assert false)
  in
  let write value = value in
  let new_typ = view typ ~read ~write in
  returning new_typ

(* Convert a char ptr encoding an utf8 string to an ocaml String (cannot use
   char_ptr_of_string here as bytes could contain null characters in the
   middle) *)
let string_of_bytes bytes length =
  String.init (Unsigned.Size_t.to_int (!@ length)) (fun i -> !@ (!@ bytes +@ i))

(* Convert an OCaml String encoded in utf8, to a char* and its length. The
   returned char* is not null terminated *)
let bytes_of_string str =
  let length = String.length str in
  let bytes = allocate_n char ~count:length in
  String.iteri (fun i c ->
    bytes +@ i <-@ c
  ) str ;
  (Unsigned.Size_t.of_int length), bytes

let free = foreign ~from:c_lib "${capi.get_name('free')}"
 (ptr char @-> returning void)

module Text = struct
  type t = string

  let c_struct : t structure typ = structure "text"

  let chars = field c_struct "chars" (ptr uint32_t)

  let length = field c_struct "length" size_t

  let is_allocated = field c_struct "is_allocated" bool

  let () = seal c_struct

  let destroy_text = foreign ~from:c_lib "${capi.get_name('destroy_text')}"
    (ptr c_struct @-> raisable void)

  let text_to_utf8 = foreign ~from:c_lib "${capi.get_name('text_to_utf8')}"
    (ptr c_struct @-> ptr (ptr char) @-> ptr size_t @-> raisable void)

  let text_from_utf8 = foreign ~from:c_lib "${capi.get_name('text_from_utf8')}"
    (ptr char @-> size_t @-> ptr c_struct @-> raisable void)

  let wrap (c_value : t structure) : t =
     let bytes = allocate (ptr char) (from_voidp char null) in
     let length = allocate (size_t) (Unsigned.Size_t.of_int 0) in
     text_to_utf8 (addr c_value) bytes length ;
     let r = string_of_bytes bytes length in
     free (!@ bytes);
     destroy_text (addr c_value);
     r

  let unwrap (value : t) : t structure =
     let length, bytes = bytes_of_string value in
     let c_value = allocate c_struct (make c_struct) in
     text_from_utf8 bytes length c_value;
     !@ c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module Character = struct
  (* Characters are encoded as strings because ocaml char are not unicode
   characters *)
  type t = string

  let char_to_utf8 = foreign ~from:c_lib "${capi.get_name('char_to_utf8')}"
    (uint32_t @-> ptr (ptr char) @-> ptr size_t @-> raisable void)

  let char_from_utf8 = foreign ~from:c_lib "${capi.get_name('char_from_utf8')}"
    (ptr char @-> size_t @-> ptr uint32_t @-> raisable void)

  let wrap (c_value : Unsigned.UInt32.t) : t =
     let bytes = allocate (ptr char) (from_voidp char null) in
     let length = allocate (size_t) (Unsigned.Size_t.of_int 0) in
     char_to_utf8 c_value bytes length ;
     let r = string_of_bytes bytes length in
     free (!@ bytes);
     r

  let chr i =
    wrap (Unsigned.UInt32.of_int i)

  let unwrap (value : string) : Unsigned.UInt32.t =
     let length, bytes = bytes_of_string value in
     let c_value = allocate uint32_t (Unsigned.UInt32.of_int 0) in
     char_from_utf8 bytes length c_value;
     !@ c_value

  let code i =
    Unsigned.UInt32.to_int (unwrap i)

  let c_type = view uint32_t ~read:wrap ~write:unwrap
end

module StringType = struct
  type t = string

  let c_struct : t structure typ = structure "string"
  let length_field = field c_struct "length" int
  let _ = field c_struct "ref_count" int
  (* Langkit strings are encoded in UTF-32 (native endianity). *)
  let content_field = field c_struct "content" uint32_t
  let () = seal c_struct

  let c_type = ptr c_struct

  let string_to_utf8 = foreign ~from:c_lib "${capi.get_name('string_to_utf8')}"
    (c_type @-> ptr (ptr char) @-> ptr size_t @-> raisable void)

  let string_from_utf8 =
    foreign ~from:c_lib "${capi.get_name('string_from_utf8')}"
      (ptr char @-> size_t @-> ptr c_type @-> raisable void)

  let dec_ref = foreign ~from:c_lib "${c_api.get_name('string_dec_ref')}"
    (c_type @-> raisable void)

  let wrap c_value_ptr =
    let bytes = allocate (ptr char) (from_voidp char null) in
    let length = allocate (size_t) (Unsigned.Size_t.of_int 0) in
    string_to_utf8 c_value_ptr bytes length ;
    let r = string_of_bytes bytes length in
    free (!@ bytes);
    dec_ref c_value_ptr;
    r

  let unwrap value =
    let length, bytes = bytes_of_string value in
    let c_value = allocate c_type (from_voidp c_struct null) in
    string_from_utf8 bytes length c_value;
    !@ c_value
end

module BigInteger = struct

  type t = Z.t

  let c_type = ptr void

  let create = foreign ~from:c_lib "${capi.get_name('create_big_integer')}"
    (ptr Text.c_type @-> raisable c_type)

  let text = foreign ~from:c_lib "${capi.get_name('big_integer_text')}"
    (c_type @-> ptr Text.c_type @-> raisable void)

  let decref = foreign ~from:c_lib "${capi.get_name('big_integer_decref')}"
    (c_type @-> raisable void)

  let wrap (c_value : unit ptr) : t =
    let c_text_ptr = allocate_n Text.c_type ~count:1 in
    text c_value c_text_ptr;
    decref c_value;
    Z.of_string (!@ c_text_ptr)

  let unwrap (value : t) : unit ptr =
    create (allocate Text.c_type (Z.to_string value))
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

let free = foreign ~from:c_lib
  "${capi.get_name('free')}"
  (ptr void @-> returning void)

(** Assuming char_ptr is a valid char*, convert it to a native Ocaml
  * string and free the C pointer.
  *)
let unwrap_str char_ptr =
  let str = Ctypes.coerce (ptr char) string char_ptr in
  free (Ctypes.coerce (ptr char) (ptr void) char_ptr);
  str


let default_grammar_rule = GrammarRule.${ctx.main_rule_api_name.camel}

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
  let message = field c_struct "message" Text.c_struct
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    sloc_range = getf c_value sloc_range;
    message = Text.wrap (getf c_value message);
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    (* sloc_range is not a pointer, thus we have a copy here which is safe. *)
    setf c_value sloc_range value.sloc_range;
    (* message is not a pointer, thus we have a copy here which is safe.
       HOWEVER, there is a link from value.message to another pointer which can
       be freed by the GC if we don't propagate this link. *)
    let c_value_message = Text.unwrap value.message in
    add_gc_link ~from:c_value ~to_:c_value_message;
    setf c_value message c_value_message;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module TokenData = struct
  type t = unit ptr
end

module Token = struct
  (* We don't have access to AnalysisContextStruct at this point. We don't need
     to do anything with the context value except pass it around, so map it as
     an opaque pointer instead. *)
  type dummy_context = unit ptr

  type t = {
    context : dummy_context;
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  let c_type : t structure typ = structure "token"
  let context = field c_type "context" (ptr void)
  let token_data = field c_type "token_data" (ptr void)
  let token_index = field c_type "token_index" int
  let trivia_index = field c_type "trivia_index" int
  let () = seal c_type

  let _token_get_kind = foreign ~from:c_lib
    "${capi.get_name('token_get_kind')}"
    (ptr c_type @-> raisable int)

  let _token_kind_name = foreign ~from:c_lib
    "${capi.get_name('token_kind_name')}"
    (int @-> raisable (ptr char))

  let _token_sloc_range = foreign ~from:c_lib
    "${capi.get_name('token_sloc_range')}"
    (ptr c_type @-> ptr SlocRange.c_type @-> raisable void)

  let token_kind_name kind =
    unwrap_str (_token_kind_name kind)

  let token_range_text = foreign ~from:c_lib
    "${capi.get_name('token_range_text')}"
    (ptr c_type @-> ptr c_type @-> ptr Text.c_type @-> raisable int)

  let wrap (c_value : t structure) : t option =
  let token_data = getf c_value token_data in
  if is_null token_data then
    None
  else
    Some {
      context = getf c_value context;
      token_data;
      token_index = getf c_value token_index;
      trivia_index = getf c_value trivia_index;
      kind = _token_get_kind (addr c_value);
      text =
        (let c_result_ptr = allocate_n Text.c_type ~count:1 in
         let _ = token_range_text (addr c_value) (addr c_value) c_result_ptr in
         !@ c_result_ptr);
      sloc_range =
        (let c_result_ptr = allocate_n SlocRange.c_type ~count:1 in
         let _ = _token_sloc_range (addr c_value) c_result_ptr in
         !@ c_result_ptr);
    }

  let unwrap (value : t) : t structure =
    let c_value = make c_type in
    setf c_value context value.context;
    setf c_value token_data value.token_data;
    setf c_value token_index value.token_index;
    setf c_value trivia_index value.trivia_index;
    c_value

  let kind_name token = token_kind_name (_token_get_kind (addr (unwrap token)))

  let sloc_range token =
    let c_result_ptr = allocate_n SlocRange.c_type ~count:1 in
    let _ = _token_sloc_range (addr (unwrap token)) c_result_ptr in
    !@ c_result_ptr

  let token_next = foreign ~from:c_lib
    "${capi.get_name('token_next')}"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let token_previous = foreign ~from:c_lib
    "${capi.get_name('token_previous')}"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let is_equivalent = foreign ~from:c_lib
    "${capi.get_name('token_is_equivalent')}"
    (ptr c_type @-> ptr c_type @-> raisable bool)

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
        (addr (unwrap token_first))
        (addr (unwrap token_last))
        c_result_ptr
    in
    if res = 0 then
      raise (Invalid_argument
        (Format.asprintf "%a and %a come from different units"
          pp token_first
          pp token_last));
    !@ c_result_ptr

  let text token = text_range token token

  let next token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_next (addr (unwrap token)) c_next_token_ptr ;
    wrap (!@ c_next_token_ptr)

  let previous token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_previous (addr (unwrap token)) c_next_token_ptr ;
    wrap (!@ c_next_token_ptr)

  let is_trivia token =
    token.trivia_index != 0

  let index token =
    match token.trivia_index with
    | 0 ->
        token.token_index - 1
    | _ ->
        token.trivia_index - 1

  let compare one other =
    let open Stdlib in
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

  let is_equivalent one other =
    is_equivalent (addr (unwrap one)) (addr (unwrap other))

end

module BareNode = struct
  type t = unit ptr
end

module Rebindings = struct
  type t = unit ptr
end

module FileReader = struct
  (* Use a pointer to pointer to register a finaliser when creating a value of
     this type. *)
  type t = unit ptr ptr

  let dec_ref =
    foreign ~from:c_lib "${capi.get_name('dec_ref_file_reader')}"
      (ptr void @-> raisable void)

  let read v =
    let finalise arg =
      dec_ref (!@ arg)
    in
    allocate (ptr void) ~finalise v

  let c_type = view (ptr void) ~read ~write:(!@)
end

module UnitProvider = struct
  (* The real C type of a context is a void*. But we use a pointer to this
     type, to be able to allocate a value of t and attach a finalizer to it. *)
  type t = unit ptr ptr

  let c_type = ptr void

  let null = allocate c_type null

  ${exts.include_extension(
     ctx.ext('ocaml_api', 'unit_providers', 'module_struct')
  )}
end

<%
   ## Register array and struct types to generate them in topogical ordering
   for struct_type in ctx.struct_types:
      ocaml_api.register_struct_type(struct_type)

   for array_type in ctx.array_types:
      ocaml_api.register_array_type(array_type)
%>

% for typ in ocaml_api.ordered_types():
   % if typ is T.AnalysisUnit:
      ${struct_types.analysis_unit()}
   % elif typ is ocaml_api.AnalysisContext:
      ${struct_types.analysis_context()}
   % elif typ is T.Symbol:
      ${struct_types.symbol()}
   % elif typ.is_entity_type:
      % if typ == root_entity:
         ## We want only one module defining the entity c structure
         ${struct_types.decl_struct(typ)}
      % endif
   % elif typ is T.entity_info:
      ${struct_types.decl_struct(typ)}
   % elif typ is T.env_md:
      ${struct_types.decl_struct(typ)}
   % elif typ.is_struct_type and typ.exposed:
      ${struct_types.decl_struct(typ)}
   % elif typ.is_array_type:
      % if typ.exposed and typ.emit_c_type:
         ${array_types.decl_struct(typ)}
      % endif
   % endif
% endfor

module CFunctions = struct
  let node_kind = foreign ~from:c_lib "${capi.get_name('node_kind')}"
    (ptr ${ocaml_api.c_type(root_entity)} @-> raisable int)

  let image = foreign ~from:c_lib "${capi.get_name('node_image')}"
    (ptr ${ocaml_api.c_type(root_entity)}
     @-> ptr Text.c_type
     @-> raisable void)

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

  let node_is_token_node = foreign ~from:c_lib
    "${capi.get_name('node_is_token_node')}"
    (ptr ${ocaml_api.c_type(root_entity)}
     @-> raisable bool)

% for astnode in ctx.astnode_types:
   % for field in astnode.fields_with_accessors():
     % if not field.type.is_iterator_type:
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
    % endif

   % endfor
% endfor

end

type analysis_context = {
  c_value : AnalysisContextStruct.t;
  unit_provider : UnitProvider.t
}

and ${ocaml_api.type_public_name(T.AnalysisUnit)} = {
  c_value : AnalysisUnitStruct.t;
  context : analysis_context
}

and entity = ${ocaml_api.c_value_type(root_entity)}

${struct_types.ocaml_fields(T.entity_info, rec=True)}

${struct_types.ocaml_fields(T.env_md, rec=True)}

% for astnode in ctx.astnode_types:
  ${astnode_types.sig(astnode)}
% endfor

${exts.include_extension(
   ctx.ext('ocaml_api', 'module_struct')
)}

let rec ${ocaml_api.unwrap_function_name(T.root_node)} value =
  (* This is the unique unwrap function that can be called for any node. *)
  match (value :> ${root_entity_type}) with
% for subclass in ctx.astnode_types:
   % if not subclass.abstract:
  | ${ocaml_api.polymorphic_variant_name(subclass)} fields -> fields.c_value
   % endif
% endfor

${struct_types.unwrap_struct(T.entity_info, rec=True)}

${struct_types.unwrap_struct(T.env_md, rec=True)}

and ${ocaml_api.unwrap_function_name(T.AnalysisUnit)}
  (unit : ${ocaml_api.type_public_name(T.AnalysisUnit)}) = unit.c_value

let rec ${ocaml_api.wrap_function_name(T.root_node)} context c_value =
  (* Top level wrap function that dispatch to wrap function of concrete types
     depending on the node kind *)
  if is_null (getf c_value ${ocaml_api.struct_name(root_entity)}.node) then
    raise (SyntaxError "null node")
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

% for astnode in ctx.astnode_types:
   % if astnode != T.root_node:
      ## root node is defined separately
      ${astnode_types.struct(astnode)}
   % endif
% endfor

${struct_types.wrap_struct(T.entity_info, rec=True)}

${struct_types.wrap_struct(T.env_md, rec=True)}

and ${ocaml_api.wrap_function_name(T.AnalysisUnit)} context c_value
   : ${ocaml_api.type_public_name(T.AnalysisUnit)} = {
 c_value=c_value;
 context=context;
}

module Entity = struct
  type t = entity

  let info value =
    ${ocaml_api.wrap_value("getf value {}.info".format(
         ocaml_api.struct_name(root_entity)), T.entity_info, None)}

  let compare e1 e2 =
    let open Stdlib in
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

module AnalysisUnit = struct
  type t = ${ocaml_api.type_public_name(T.AnalysisUnit)}

  let root (unit : t) =
    let c_value = make ${ocaml_api.c_type(root_entity)} in
    AnalysisUnitStruct.unit_root
      (${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)})
      (addr c_value);
    ${ocaml_api.wrap_value('c_value', root_entity, "unit.context",
         check_for_null=True)}

  let diagnostics (unit : t) =
    let c_unit = ${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)} in
    let length = AnalysisUnitStruct.unit_diagnostic_count c_unit in
    let f i =
      let diag = allocate_n Diagnostic.c_type ~count:1 in
      let _ : int = AnalysisUnitStruct.unit_diagnostic c_unit i diag in
      !@ diag
    in
    List.init length f

  let filename (unit : t) =
    unwrap_str( AnalysisUnitStruct.unit_filename
      (${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)}))

  let reparse ?charset:(charset="") ?buffer (unit : t) =
    match buffer with
    | None ->
        ignore
          (AnalysisUnitStruct.unit_reparse_from_file unit.c_value charset)
    | Some buffer ->
        ignore (AnalysisUnitStruct.unit_reparse_from_buffer unit.c_value
          charset buffer (Unsigned.Size_t.of_int (String.length buffer)))

  let first_token (unit : t) =
    let c_unit = ${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)} in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_first_token c_unit result_ptr ;
    Token.wrap (!@ result_ptr)

  let last_token (unit : t) =
    let c_unit = ${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)} in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_last_token c_unit result_ptr ;
    Token.wrap (!@ result_ptr)

  let token_count (unit : t) =
    AnalysisUnitStruct.unit_token_count
      (${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)})

  let trivia_count (unit : t) =
    AnalysisUnitStruct.unit_trivia_count
      (${ocaml_api.unwrap_value("unit", T.AnalysisUnit, None)})

  ${token_iterator.struct("first_token", "last_token")}
end

module AnalysisContext = struct
  type t = analysis_context

  let create
    ?charset:(charset="")
    ?with_trivia:(with_trivia=true)
    ?tab_stop:(tab_stop=${ctx.default_tab_stop})
    ?unit_provider:(unit_provider=UnitProvider.null)
    ?file_reader () : t =
    if tab_stop < 1 then
      raise (Invalid_argument "Invalid tab_stop (positive integer expected)") ;
    let c_context = AnalysisContextStruct.allocate_analysis_context () in
    AnalysisContextStruct.initialize_analysis_context
      c_context
      charset
      (match file_reader with Some v -> !@v | None -> null)
      (!@unit_provider)
      Ctypes.null (* TODO: bind the event handlers API to OCaml *)
      with_trivia
      tab_stop ;
    { c_value= c_context
      ; unit_provider= unit_provider }

  let get_from_file
    ?charset:(charset="")
    ?reparse:(reparse=false)
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    (ctx : t)
    filename : AnalysisUnit.t =

    ${ocaml_api.wrap_value(
      "AnalysisContextStruct.get_analysis_unit_from_file ctx.c_value"
      + " filename charset reparse grammar_rule", T.AnalysisUnit, "ctx")}

  let get_from_buffer
    ?charset:(charset="")
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    (ctx : t)
    filename
    buffer : AnalysisUnit.t =

    ${ocaml_api.wrap_value(
      "AnalysisContextStruct.get_analysis_unit_from_buffer ctx.c_value"
      + " filename charset buffer"
      + " (Unsigned.Size_t.of_int (String.length buffer)) grammar_rule"
      , T.AnalysisUnit, "ctx")}
end

% for typ in ocaml_api.ordered_types():
   % if typ not in [T.AnalysisUnit, ocaml_api.AnalysisContext, T.Symbol,\
                    T.entity_info, T.env_md]:
      % if not typ.is_entity_type:
         % if typ.is_struct_type and typ.exposed:
   ${struct_types.decl_wrapper(typ)}
         % elif typ.is_array_type and typ.exposed:
   ${array_types.decl_wrapper(typ)}
         % endif
      % endif
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
let ${ocaml_api.field_name(field)}
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
    % if field.type.is_iterator_type:
      raise (Failure "iterators not implemented yet")
    % else:
      let result_ptr =
        allocate_n ${ocaml_api.c_type(field.public_type)} ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      % for arg in field.arguments:
      let c_${arg.name.lower} =
         % if arg.default_value is not None and arg.public_type.is_entity_type:
         ## for entity default argument with use the optional label
         ## construction that hides the fact that an ommited parameter is the
         ## value None.
           match ${arg.name.lower} with
           | Some node ->
               addr (${ocaml_api.unwrap_value('node', arg.public_type,
                                              '(context node).c_value')})
           | None -> allocate_n ${ocaml_api.c_type(root_entity)} ~count:1
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
        ${value}
         % endif
      in
      % endfor
      let _ : int =
        CFunctions.${field.accessor_basename.lower}
          (addr (${ocaml_api.unwrap_value('node', astnode.entity, None)}))
      % for arg in field.arguments:
          c_${arg.name.lower}
      % endfor
          (result_ptr)
      in
      % for arg in field.arguments:
         <%
            finalize_name = ocaml_api.finalize_function(arg.public_type)
         %>
         % if finalize_name is not None:
            % if arg.public_type.is_ada_record:
               ${finalize_name} (!@ c_${arg.name.lower}) ;
            % else:
               ${finalize_name} c_${arg.name.lower} ;
            % endif
         % endif
      % endfor
      ## For entity types, we return an optional argument instead of calling
      ## the wrapper, that would raise an exception. This is why we set
      ## check_for_null to true.
      ${ocaml_api.wrap_value('!@ result_ptr', field.public_type,
         '(context node)', check_for_null=True)}
    % endif

   % endfor

   % for field in ocaml_api.get_parse_fields(astnode):
  let ${ocaml_api.field_name(field)} node =
    match (node :> ${ocaml_api.type_public_name(astnode)}) with
      % for concrete in astnode.concrete_subclasses:
    | ${ocaml_api.polymorphic_variant_name(concrete)} fields ->
        Lazy.force fields.${ocaml_api.field_name(field)}
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
    match token_start node, token_end node with
    | Some tok_start, Some tok_end ->
        Token.text_range tok_start tok_end
    | _ ->
        ""

  let image node =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    CFunctions.image
      (addr (${ocaml_api.unwrap_value('node', root_entity, 'context node')}))
      c_result_ptr;
    !@ c_result_ptr

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
    ${ocaml_api.wrap_value('!@ result_ptr', root_entity, '(context node)',
         check_for_null=True)}

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
      ${ocaml_api.wrap_value('!@ fresh', root_entity, 'context',
         check_for_null=True)}
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

  let lookup_with_kind :
    type a. a node -> [< ${root_entity_type}] -> Sloc.t -> a option =
    fun node_type node sloc ->
      let lookup_res = lookup node sloc in
      let rec aux : a node -> [< ${root_entity_type}] -> a option =
        fun node_type node ->
        match node_type, node with
      %for astnode in ctx.astnode_types:
        | ${ocaml_api.node_name(astnode)}
          , (#${ocaml_api.type_public_name(astnode)} as node) ->
          Some node
      %endfor
        | _ -> (match parent node with
                | Some parent_node -> aux node_type parent_node
                | _ -> None) in
    match lookup_res with
      | Some node -> aux node_type node
      | _ -> None

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
                  % if field.nullable:
        ("${ocaml_api.field_name(field)[2:]}"
        , (Lazy.force value.${ocaml_api.field_name(field)}
           :> ${root_entity_type} option));
                  % else:
        (try
           ("${ocaml_api.field_name(field)[2:]}"
           , Some (Lazy.force value.${ocaml_api.field_name(field)}
                    :> ${root_entity_type}))
        with SyntaxError _ ->
          ("${ocaml_api.field_name(field)[2:]}", None) );
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
      let repr = image node in
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
