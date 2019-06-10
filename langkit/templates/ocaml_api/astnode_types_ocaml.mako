<%def name="field_type(astnode)">
  {
      % for field in ocaml_api.get_parse_fields(astnode):
         <%
            precise_types = ocaml_api.get_field_type(field)
         %>
         % if len(precise_types) == 1:
    ${field.name.lower}: ${ocaml_api.type_public_name(precise_types[0],
                                                      astnode)}
            % if field.is_optional:
    option
            % endif
    Lazy.t;
         % else:
    ${field.name.lower}: [
            % for tpe in precise_types:
      | ${ocaml_api.polymorphic_variant_name(tpe)}
          of ${ocaml_api.fields_name(tpe, astnode)}
            % endfor
    ]
            % if field.is_optional:
    option
            % endif
    Lazy.t;
         % endif
      % endfor
      % if astnode.is_list:
    list : ${ocaml_api.type_public_name(astnode.element_type)} list Lazy.t;
      % endif
    c_value : Entity.t;
    context : AnalysisContext.t
  }
</%def>

<%def name="ast_type(astnode)">
   ## Keep direct subclass type information, at least as a comment.
   ## Do not print it, if it's redundant with the concrete_subclasses.
   <%
     subclasses_len = len(astnode.subclasses)
     concrete_len = len(astnode.concrete_subclasses)
   %>
   % if astnode.abstract and subclasses_len != concrete_len:
  (**
      % for subclass in astnode.subclasses:
    * ${ocaml_api.type_public_name(subclass)}
      % endfor
    *)
   % endif
  type t = [
   % for subclass in astnode.concrete_subclasses:
    | ${ocaml_api.polymorphic_variant_name(subclass)}
        of ${ocaml_api.fields_name(subclass, astnode)}
   % endfor
  ]
   % if not astnode.abstract:
  and fields = ${field_type(astnode)}
   % endif
</%def>

<%def name='sig(astnode)'>
  ${ast_type(astnode)}

  val wrap :
    AnalysisContext.t
    -> ${ocaml_api.c_value_type(root_entity)}
    -> ${ocaml_api.type_public_name(astnode, astnode)}
</%def>

<%def name='struct(astnode)'>
  ${ast_type(astnode)}

   % if astnode.abstract:
  let wrap context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match ${ocaml_api.wrap_value('c_value', root_entity, "context")} with
      % for tpe in astnode.concrete_subclasses:
      | ${ocaml_api.polymorphic_variant_name(tpe)} _
      % endfor
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false
   % else:

  let wrap context c_value =
      % for field in ocaml_api.get_parse_fields(astnode):
    let ${field.name.lower} () =
      let field_c_value = make ${ocaml_api.c_type(field.public_type)} in
      let _ : int = CFunctions.${field.accessor_basename.lower}
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         % if field.is_optional:
         ${ocaml_api.check_for_null('field_c_value', field.public_type,
                                    'context')}
         % else:
         ${ocaml_api.wrap_value('field_c_value', field.public_type, 'context')}
         % endif
      in
         <%
            precise_types = ocaml_api.get_field_type(field)
         %>

         % if len(precise_types) == 1:
      node
         % else:
      match node with
            <% some_or_nothing = 'Some ' if field.is_optional else '' %>
            % for tpe in precise_types:
      | ${some_or_nothing}${ocaml_api.polymorphic_variant_name(tpe)} _
            %  endfor
      ${'| None ' if field.is_optional else ''}as e -> e
      | _ -> assert false
         % endif
    in
      % endfor
      % if astnode.is_list:
    let list () =
      let c_value_ptr =
        allocate_n ${ocaml_api.c_type(T.entity.array)} ~count:1
      in
      let _ : int =
        CFunctions.${c_api.get_name('node_children')}
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value ${ocaml_api.struct_name(T.entity.array)}.n in
      let items = c_value @. ${ocaml_api.struct_name(T.entity.array)}.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        ${ocaml_api.wrap_value('(!@ fresh)', astnode.element_type.entity,
                               'context')}
      in
      let result = List.init length f in
      ${ocaml_api.struct_name(T.entity.array)}.dec_ref (!@ c_value_ptr);
      result
    in
      % endif
    if is_null (getf c_value ${ocaml_api.struct_name(root_entity)}.node) then
      raise SyntaxError
    else
      ${ocaml_api.polymorphic_variant_name(astnode)} {
      % for field in ocaml_api.get_parse_fields(astnode):
        ${field.name.lower} = Lazy.from_fun ${field.name.lower};
      % endfor
      % if astnode.is_list:
        list = Lazy.from_fun list;
      % endif
        c_value = c_value;
        context = context
      }
   % endif
</%def>

<%def name='decl_type(astnode)'>
   % if not ocaml_api.is_empty_type(astnode):
      ## We don't generate the module if the type does not have any concrete
      ## subclass since it will cause an empty type.
and ${ocaml_api.module_name(astnode)} : sig
   ${sig(astnode)}
end = struct
   ${struct(astnode)}
end
   % endif
</%def>
