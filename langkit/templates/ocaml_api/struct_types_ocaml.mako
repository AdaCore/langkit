<%def name="ctype_fields(cls)">
  let c_type : t structure typ = structure "${cls.api_name.lower}"
   % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
  let ${ocaml_api.field_name(f)} =
    field c_type "${ocaml_api.field_name(f)}" ${ocaml_api.c_type(f.type, cls)}
   % endfor
  let () = seal c_type
</%def>

<%def name="decl_struct(cls)">
   % if not ocaml_api.is_empty_type(cls):
module ${ocaml_api.struct_name(cls)} = struct
  type t

   ${ctype_fields(cls)}
end
   % endif
</%def>

<%def name="ocaml_fields(cls)">
  type t = {
   % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
      % if f.type.is_ast_node:
    ${ocaml_api.field_name(f)} : ${ocaml_api.c_value_type(f.type, cls)};
      % else:
    ${ocaml_api.field_name(f)} : ${ocaml_api.type_public_name(f.type, cls)};
      % endif
   % endfor
  }
</%def>

<%def name="sig_wrapper(cls)">
  ${ocaml_fields(cls)}

   % if ocaml_api.wrap_requires_context(cls):
  val wrap : AnalysisContext.t -> ${ocaml_api.c_value_type(cls)} -> t
   % else:
  val wrap : ${ocaml_api.c_value_type(cls)} -> t
   % endif

   % if cls.conversion_requires_context:
  val unwrap : AnalysisContext.t -> t -> ${ocaml_api.c_value_type(cls)}
   % else:
  val unwrap : t -> ${ocaml_api.c_value_type(cls)}
   % endif

</%def>

<%def name="struct_wrapper(cls)">
  ${ocaml_fields(cls)}

   % if ocaml_api.wrap_requires_context(cls):
  let wrap context c_value = {
   % else:
  let wrap c_value = {
   % endif
      % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
    ${ocaml_api.field_name(f)} = ${ocaml_api.wrap_value(
      'getf c_value {}.{}'.format(ocaml_api.struct_name(cls),
                                  ocaml_api.field_name(f)),
      f.type, 'context')};
      % endfor
  }

   % if cls.conversion_requires_context:
  let unwrap context value =
   % else:
  let unwrap value =
   % endif
    let c_value = make ${ocaml_api.c_type(cls)} in
      % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
    setf c_value
      ${ocaml_api.struct_name(cls)}.${ocaml_api.field_name(f)}
      (${ocaml_api.unwrap_value('value.{}'.format(ocaml_api.field_name(f)),
                                f.type, 'context')});
      % endfor
    c_value

</%def>

<%def name="decl_wrapper(cls, rec=False)">
   % if not ocaml_api.is_empty_type(cls):
      % if rec:
and ${ocaml_api.module_name(cls)} : sig
      % else:
module ${ocaml_api.module_name(cls)} : sig
      % endif
   ${sig_wrapper(cls)}
end = struct
   ${struct_wrapper(cls)}
end
   % endif
</%def>

<%def name="public_sig(cls)">
   % if not ocaml_api.is_empty_type(cls):
and ${ocaml_api.module_name(cls)} : sig
  ${ocaml_doc(cls, 1)}

  ${ocaml_fields(cls)}
end
   % endif
</%def>

<%def name="analysis_unit()">
module AnalysisUnitStruct : sig
  type t = unit ptr

  val c_type : t typ

  val unit_root : t -> ${ocaml_api.c_value_type(root_entity)} ptr -> unit

  val unit_diagnostic_count : t -> int

  val unit_diagnostic : t -> int -> Diagnostic.t ptr -> int

  val unit_reparse_from_file : t -> string -> int

  val unit_reparse_from_buffer :
    t -> string -> string -> Unsigned.size_t -> int

  val unit_first_token : t -> Token.t ptr -> unit

  val unit_last_token : t -> Token.t ptr -> unit

  val unit_token_count : t -> int

  val unit_trivia_count : t -> int
end = struct
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

</%def>

<%def name="symbol()">
module Symbol : sig
  type t = string

  val c_type : t structure typ

  val wrap : (t structure) -> t

  val unwrap : AnalysisContextStruct.t -> t -> (t structure)

  val symbol_text : t structure ptr -> string ptr -> unit

  val context_symbol :
    AnalysisContextStruct.t -> string ptr -> t structure ptr -> int
end = struct
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
</%def>

<%def name="analysis_context()">
module AnalysisContextStruct : sig
  type t = unit ptr

  val c_type : t typ

  val create_analysis_context : string -> UnitProvider.t -> bool -> int -> t

  val context_decref : t -> unit

  val get_analysis_unit_from_file :
    t
    -> string
    -> string
    -> bool
    -> GrammarRule.t
    -> AnalysisUnitStruct.t

  val get_analysis_unit_from_buffer :
    t
    -> string (* Filename *)
    -> string (* Charset *)
    -> string (* Buffer *)
    -> Unsigned.size_t (* Buffer size *)
    -> GrammarRule.t
    -> AnalysisUnitStruct.t
end = struct
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
</%def>
