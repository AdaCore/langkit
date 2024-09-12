<%def name="ctype_fields(cls)">
  let c_type : t structure typ = structure "${cls.api_name.lower}"
   % if not cls.is_empty:
   % for f in cls.get_fields():
  let ${ocaml_api.field_name(f)} =
    field c_type "${ocaml_api.field_name(f)}" ${ocaml_api.c_type(f.type, cls)}
   % endfor
   % else:
   let dummy = field c_type "dummy" char
   % endif
  let () = seal c_type
</%def>

<%def name="decl_struct(cls)">
module ${ocaml_api.struct_name(cls)} = struct
  type t

   ${ctype_fields(cls)}
end
</%def>

<%def name="ocaml_fields(cls, rec=False)">
   % if not ocaml_api.is_empty_type(cls):
  ${("and" if rec else "type")} ${ocaml_api.type_public_name(cls, cls)} = {
      % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
      ${ocaml_api.field_name(f)} :
         % if f.type.is_ast_node:
         ${ocaml_api.c_value_type(f.type, cls)};
         % elif f.type.is_entity_type:
         ## If the type is an entity type, we want an optional type instead.
         ## This is because we have no way to know if the node will be null
         ## or not.
         ${ocaml_api.type_public_name(f.type, cls)} option;
         % else:
         ${ocaml_api.type_public_name(f.type, cls)};
         % endif
      % endfor
  }
   % endif
</%def>

<%def name="sig_wrapper(cls)">
  ${ocaml_fields(cls)}

   % if ocaml_api.wrap_requires_context(cls):
  val wrap :
       ?dec_ref:bool
    -> analysis_context
    -> ${ocaml_api.c_value_type(cls)}
    -> t
   % else:
  val wrap : ?dec_ref:bool -> ${ocaml_api.c_value_type(cls)} -> t
   % endif

   % if cls.conversion_requires_context:
  val unwrap : analysis_context -> t -> ${ocaml_api.c_value_type(cls)}
   % else:
  val unwrap : t -> ${ocaml_api.c_value_type(cls)}
   % endif

</%def>

<%def name="wrap_struct(cls, rec=False)">
   <%
      let = "and" if rec else "let"
   %>
   % if not ocaml_api.is_empty_type(cls):
      % if ocaml_api.wrap_requires_context(cls):
  ${let} ${ocaml_api.wrap_function_name(cls, cls)}
    ?(dec_ref=true)
    context
    c_value = {
      % else:
  ${let} ${ocaml_api.wrap_function_name(cls, cls)} ?(dec_ref=true) c_value = {
      % endif
      % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
    ${ocaml_api.field_name(f)} = ${ocaml_api.wrap_value(
      '(getf c_value {}.{})'.format(ocaml_api.struct_name(cls),
                                    ocaml_api.field_name(f)),
      f.type, 'context', check_for_null=True, dec_ref= "dec_ref")};
      % endfor
  }
  % endif
</%def>

<%def name="unwrap_struct(cls, rec=False)">
   <%
      let = "and" if rec else "let"
   %>

   % if not ocaml_api.is_empty_type(cls):
      % if cls.conversion_requires_context:
  ${let} ${ocaml_api.unwrap_function_name(cls, cls)} context value =
      % else:
  ${let} ${ocaml_api.unwrap_function_name(cls, cls)} value =
      % endif
    let c_value = make ${ocaml_api.c_type(cls)} in
      % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
    let field_c_value =
      ${ocaml_api.unwrap_value('value.{}'.format(ocaml_api.field_name(f)),
                               f.type, 'context', check_for_none=True)}
    in
      ## We don't know if the actual unwrapped value needs a gc link. To be
      ## safe, always add one.
    add_gc_link ~from:c_value ~to_:field_c_value;
    setf c_value
      ${ocaml_api.struct_name(cls)}.${ocaml_api.field_name(f)}
      field_c_value;
      % endfor
    c_value
   % endif

</%def>

<%def name="struct_wrapper(cls)">
  ${ocaml_fields(cls)}

  ${wrap_struct(cls)}

  ${unwrap_struct(cls)}
</%def>

<%def name="decl_wrapper(cls)">
   % if not ocaml_api.is_empty_type(cls):
module ${ocaml_api.module_name(cls)} : sig
   ${sig_wrapper(cls)}
end = struct
   ${struct_wrapper(cls)}
end
   % endif
</%def>

<%def name="public_sig(cls)">
   % if not ocaml_api.is_empty_type(cls):
module ${ocaml_api.module_name(cls)} : sig
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

  val unit_filename : t -> char ptr

  val unit_reparse_from_file : t -> string -> int

  val unit_reparse_from_buffer :
    t -> string -> string -> Unsigned.size_t -> int

  val unit_first_token : t -> Token.t structure ptr -> unit

  val unit_last_token : t -> Token.t structure ptr -> unit

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

  let unit_filename = foreign ~from:c_lib
    "${capi.get_name('unit_filename')}"
    (c_type @-> raisable (ptr char))

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

  val symbol_text : t structure ptr -> Text.t structure ptr -> unit

  val context_symbol :
    AnalysisContextStruct.t -> Text.t structure ptr -> t structure ptr -> int
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
    Text.wrap (!@ c_result_ptr)

  let context_symbol = foreign ~from:c_lib "${capi.get_name('context_symbol')}"
    (AnalysisContextStruct.c_type
     @-> ptr Text.c_type
     @-> ptr c_type
     @-> raisable int)

  let unwrap (ctx : AnalysisContextStruct.t) (value : t) : t structure =
    let result = make c_type in
    let c_text = Text.unwrap value in
    let code =
      context_symbol ctx (addr c_text) (addr result)
    in
    Text.destroy_text (addr c_text);
    if code = 0 then
      raise (InvalidSymbolError value) ;
    result
end
</%def>

<%def name="analysis_context()">
module AnalysisContextStruct : sig
  type t

  val c_type : t typ

  val allocate_analysis_context : ?keep:'a -> unit -> t
  val initialize_analysis_context :
    t -> string -> unit ptr -> unit ptr -> unit ptr -> bool -> int -> unit

  val get_analysis_unit_from_file :
    t -> string -> string -> bool -> GrammarRule.t -> AnalysisUnitStruct.t

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

  (* The real C type of a context is a void*. But we use a pointer to this
     type, to be able to allocate a value of t and attach a finalizer to it.
     See wrap function *)
  type t = unit ptr ptr

  let unwrap (value : t) : unit ptr = !@value

  (* The read part is not required as the only function returning a c_type is
     allocate_analysis_context which is manually written to take an object
     to keep alive as argument *)
  let c_type = view (ptr void) ~read:(fun _ -> assert false) ~write:unwrap

  let context_decref =
    foreign ~from:c_lib "${capi.get_name('context_decref')}"
      (c_type @-> raisable void)

  let c_allocate_analysis_context =
    foreign ~from:c_lib "${capi.get_name('allocate_analysis_context')}"
      ( void @-> raisable (ptr void) )

  let allocate_analysis_context ?keep () =
    (* To deallocate cleanly the context, we need to call context_decref.
       Allocate a value and attach a finalizer to it. Use the keep option
       to keep an object alive while the analysis context is. *)
    let ref_keep = ref keep in
    let finalise arg =
      ref_keep := None;
      context_decref arg
    in
    let c_value = c_allocate_analysis_context () in
    allocate ~finalise (ptr void) c_value

  let initialize_analysis_context =
    foreign ~from:c_lib "${capi.get_name('initialize_analysis_context')}"
      ( c_type @-> string @-> ptr void @-> UnitProvider.c_type @-> ptr void
      @-> bool @-> int @-> raisable void )

  let get_analysis_unit_from_file =
    foreign ~from:c_lib "${capi.get_name('get_analysis_unit_from_file')}"
      ( c_type @-> string @-> string @-> bool @-> GrammarRule.c_type
      @-> raisable AnalysisUnitStruct.c_type )

  let get_analysis_unit_from_buffer =
    foreign ~from:c_lib "${capi.get_name('get_analysis_unit_from_buffer')}"
      ( c_type @-> string (* Filename *) @-> string (* Charset *)
      @-> string (* Buffer *) @-> size_t (* Buffer size *)
      @-> GrammarRule.c_type
      @-> raisable AnalysisUnitStruct.c_type )

end
</%def>
