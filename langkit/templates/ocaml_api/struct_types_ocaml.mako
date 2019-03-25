<%def name="ctype_fields(cls)">
  let c_type : t structure typ = structure "${cls.api_name.lower}"
   % for f in cls.get_fields(lambda t: not ocaml_api.is_empty_type(t.type)):
  let ${f.name.lower} =
    field c_type "${f.name.lower}" ${ocaml_api.c_type(f.type, cls)}
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
    ${f.name.lower} : ${ocaml_api.c_value_type(f.type, cls)};
      % else:
    ${f.name.lower} : ${ocaml_api.type_public_name(f.type, cls)};
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
    ${f.name.lower} = ${ocaml_api.wrap_value('getf c_value {}.{}'.format(
                              ocaml_api.struct_name(cls), f.name.lower),
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
      ${ocaml_api.struct_name(cls)}.${f.name.lower}
      (${ocaml_api.unwrap_value('value.{}'.format(f.name.lower),
                                f.type, 'context')});
      % endfor
    c_value

</%def>

<%def name="decl_wrapper(cls)">
   % if not ocaml_api.is_empty_type(cls):
and ${ocaml_api.module_name(cls)} : sig
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
