<%def name="struct(cls)">
   ## When called, generate the values for the module representing the c value
   ## of the given type.

  type t

  let c_struct : t structure typ = structure "${cls.api_name.lower}"
  let n = field c_struct "n" int
  let _ = field c_struct "ref_count" int
   % if cls.is_string_type:
   ## For a string types, the type of items is uint32_t, since they are utf32
   ## strings.
  let items = field c_struct "items" uint32_t
   % else:
  let items = field c_struct "items"
    ${ocaml_api.c_type(cls.element_type, cls)}
   % endif
  let () = seal c_struct

  let c_type = ptr c_struct

  let create = foreign ~from:c_lib "${cls.c_create(capi)}"
    (int @-> raisable c_type)
  let dec_ref = foreign ~from:c_lib "${cls.c_dec_ref(capi)}"
    (c_type @-> raisable void)
</%def>

<%def name="decl_struct(cls)">
module ${ocaml_api.struct_name(cls)} = struct
  ${struct(cls)}
end
</%def>

<%def name="sig_wrapper(cls)">
   % if cls.is_string_type:
  type t = string
   % else:
  type t = ${ocaml_api.type_public_name(cls)}
   % endif

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
   % if cls.is_string_type:
  type t = string
   % else:
  type t = ${ocaml_api.type_public_name(cls)}
   % endif

   % if ocaml_api.wrap_requires_context(cls):
  let wrap context c_value_ptr =
   % else:
  let wrap c_value_ptr =
   % endif
   % if cls.is_string_type:
    let open Text in
    let open Camomile in
   % endif
    let c_value = !@ c_value_ptr in
    let length = getf c_value ${ocaml_api.struct_name(cls)}.n in
    let items = c_value @. ${ocaml_api.struct_name(cls)}.items in
   % if cls.is_string_type:
    (* We use Camomile to encode utf32 strings to an ocaml string *)
    let f i = UChar.chr_of_uint (Unsigned.UInt32.to_int !@(items +@ i)) in
    let result =
      UCS4Encoding.encode CharEncoding.utf8 (UCS4.init length f)
    in
   % else:
    let f i =
      % if cls.element_type.is_ada_record:
      (* we want to allocate a fresh value for a record, otherwize, the c value
       * will still point to the memory at array location *)
      let fresh =
        allocate ${ocaml_api.c_type(cls.element_type)} (!@ (items +@ i))
      in
      ${ocaml_api.wrap_value('!@ fresh', cls.element_type, "context")}
      % else:
      ${ocaml_api.wrap_value('!@ (items +@ i)', cls.element_type, "context")}
      % endif
    in
    let result = List.init length f in
   % endif
    ${ocaml_api.struct_name(cls)}.dec_ref c_value_ptr;
    result

   % if cls.conversion_requires_context:
  let unwrap context value =
   % else:
  let unwrap value =
   % endif
   % if cls.is_string_type:
    let open Text in
    let open Camomile in
    let text = UCS4Encoding.decode CharEncoding.utf8 value in
    let result = ${ocaml_api.struct_name(cls)}.create (UCS4.length text) in
   % else:
    let result = ${ocaml_api.struct_name(cls)}.create (List.length value) in
   % endif
    let items = result |-> ${ocaml_api.struct_name(cls)}.items in
   % if cls.conversion_requires_context:
    let c_context = context.AnalysisContext.c_value in
   % endif
   % if cls.is_string_type:
    let i = ref 0 in
    let f c =
      items +@ !i <-@ Unsigned.UInt32.of_int (UChar.code c);
      i := !i + 1
    in
    UCS4.iter f text;
   % else:
    let f i v =
      items +@ i <-@
        ${ocaml_api.unwrap_value('v', cls.element_type, 'c_context')}
    in
    List.iteri f value;
   % endif
    result

</%def>

<%def name="decl_wrapper(cls)">
module ${ocaml_api.module_name(cls)} : sig
   ${sig_wrapper(cls)}
end = struct
   ${struct_wrapper(cls)}
end
</%def>
