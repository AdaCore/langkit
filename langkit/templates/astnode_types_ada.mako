## vim: filetype=makoada

<%namespace name="env_specs"        file="env_specs_ada.mako" />
<%namespace name="exts"             file="extensions.mako" />
<%namespace name="prop_helpers"     file="properties/helpers.mako" />
<%namespace name="logic_predicates" file="logic_predicates_ada.mako" />
<%namespace name="logic_functors"   file="logic_functors_ada.mako" />


<%def name="logic_helpers()">
   ## Generate predicate and converter functors, which wrap properties to be
   ## used in logic equations.
   ##
   ## Note that we need to generate them before the properties bodies, because
   ## they'll be used in the bodies.
   % for cls in ctx.node_types:
      % for prop in cls.get_properties(include_inherited=False):
         % for pred in prop.logic_predicates:
            ${logic_predicates.decl(pred)}
            ${logic_predicates.body(pred)}
         % endfor

         % for functor in prop.logic_functors:
            ${logic_functors.decl(functor)}
            ${logic_functors.body(functor)}
         % endfor
      % endfor
   % endfor
</%def>


## Generate code to initialize user fields in the node returned by "node_expr"
## (of type "node_type"). This does not take care of inherited fields.
##
## As we manually allocate memory for nodes, they don't benefit from the
## automatic initialization for access fields, for instance. We rely on this
## initialization for logic vars, so manually initialize them.

<%def name="init_user_fields(node_type, node_expr)">
   % for f in node_type.get_fields(predicate=lambda f: f.is_user_field, \
                                   include_inherited=False):
      ${node_expr}.${f.names.codegen} := ${f.ada_default_value};
   % endfor
</%def>


<%def name="bare_field_decl(field)">
   function ${field.names.codegen}
     (Node : ${field.owner.name}) return ${field.type.name};
</%def>


<%def name="bare_field_body(field)">
   function ${field.names.codegen}
     (Node : ${field.owner.name}) return ${field.type.name}
   is
      <%def name="return_value(cf, node_expr)">
         return ${field.type.extract_from_storage_expr(
                     node_expr=node_expr,
                     base_expr='{}.{}'.format(node_expr, cf.names.codegen)
                )};
      </%def>

      % if field.abstract:
         Kind : constant ${field.owner.ada_kind_range_name} := Node.Kind;
      % endif
   begin
      % if field.abstract:
         case Kind is
            % for cf in field.concrete_overridings:
               when ${' | '.join(n.ada_kind_name
                                 for n in cf.owner.concrete_subclasses)} =>
                  % if cf.null:
                     return ${cf.type.nullexpr};
                  % else:
                     ${return_value(cf, 'Node')}
                  % endif
            % endfor
         end case;
      % else:
         ${return_value(field, 'Node')}
      % endif
   end;
</%def>


<%def name="field_decl(field)">
   <%
      type_name = field.owner.entity.api_name
      ret_type = field.type.entity if field.type.is_ast_node else field.type
      doc = ada_doc(field, 3)
   %>

   function ${field.api_name}
     (Node : ${type_name}'Class) return ${ret_type.api_name};
   % if doc:
   ${doc}
   --% belongs-to: ${field.owner.entity.api_name}
   % else:
   --% belongs-to: ${field.owner.entity.api_name}
   % endif

   ## If this field return an enum node, generate a shortcut to get the
   ## symbolic value.
   % if field.type.is_bool_node:
      function ${field.api_name} (Node : ${type_name}'Class) return Boolean;
      --% belongs-to: ${field.owner.entity.api_name}

   % elif field.type.is_enum_node:
      function ${field.api_name}
        (Node : ${type_name}'Class) return ${field.type.ada_kind_name};
      --% belongs-to: ${field.owner.entity.api_name}
   % endif
</%def>


<%def name="field_body(field)">
   <%
      type_name = field.owner.entity.api_name
      ret_type = field.type.entity if field.type.is_ast_node else field.type
   %>

   function ${field.api_name}
     (Node : ${type_name}'Class) return ${ret_type.api_name}
   is
      Result : ${field.type.name};
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.${field.names.codegen} (Node.Internal.Node);
      % if field.type.is_ast_node:
         if Result = null then
            return No_${ret_type.api_name};
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
      % else:
         return Result;
      % endif
   end ${field.api_name};

   % if field.type.is_ast_node:
      % if field.type.is_bool_node:
         function ${field.api_name} (Node : ${type_name}'Class) return Boolean
         is (${ret_type.api_name}'(Node.${field.api_name}).Kind
             = ${field.type._alternatives[0].ada_kind_name});

      % elif field.type.is_enum_node:
         function ${field.api_name}
           (Node : ${type_name}'Class) return ${field.type.ada_kind_name}
         is (${ret_type.api_name}'(Node.${field.api_name}).Kind);
      % endif
   % endif
</%def>

<%def name="private_decl(cls)">

   <% ext = ctx.ext('nodes', cls.raw_name, 'public_decls') %>

   ## Fields initialization helper
   % if cls.has_fields_initializer:
      <%
         fields = cls.fields_to_initialize(include_inherited=True)
         parse_fields = [f for f in fields if not f.is_user_field]
      %>
      procedure Initialize_Fields_For_${cls.kwless_raw_name}
        (Self : ${cls.name}
         % for f in parse_fields:
         ; ${f.names.codegen} : ${f.type.name}
         % endfor
        );
   % endif

   ## Field getters
   % for field in cls.get_parse_fields( \
      include_inherited=False, \
      predicate=lambda f: f.abstract or not f.is_overriding, \
   ):
      ${bare_field_decl(field)}
   % endfor

   ## Properties
   % for prop in cls.get_properties(include_inherited=False):
      % if not prop.user_external:
         ${prop.prop_decl}
      % endif
   % endfor

   ${exts.include_extension(ext)}

   % if cls.env_spec:
   ${env_specs.decl(cls.env_spec)}
   % endif

</%def>


<%def name="body_decl(cls)">

   <%
      untyped_wrappers = cls.get_properties(
         include_inherited=False,
         predicate=lambda f: f.requires_untyped_wrapper
      )
   %>

   % if untyped_wrappers:
      --
      --  Untyped wrappers for ${cls.name}
      --

      % for prop in untyped_wrappers:
         ${prop.untyped_wrapper_decl}
      % endfor
   % endif

</%def>


<%def name="body(cls)">

   --
   --  Primitives for ${cls.name}
   --

   <%
   # Keep a list of ASTNode fields
   astnode_fields = cls.get_parse_fields(lambda f: f.type.is_ast_node)

   # Keep a list of user fields
   user_fields = cls.get_user_fields()

   # Keep a list of fields that are annotated with repr
   repr_fields = cls.get_parse_fields(lambda f: f.repr)

   ext = ctx.ext('nodes', cls.raw_name, 'bodies')
   %>

   % if cls.env_spec and cls.env_spec.actions:
   ${env_specs.body(cls.env_spec)}
   % endif

   ## Fields initialization helper
   % if cls.has_fields_initializer:
      <%
         filter_parse_fields = (
            lambda fields: [f for f in fields if not f.is_user_field]
         )

         # All fields to initialize
         all_fields = cls.fields_to_initialize(include_inherited=True)
         all_parse_fields = filter_parse_fields(all_fields)

         # Fields unique to this node (not inherited)
         self_fields = cls.fields_to_initialize(include_inherited=False)
         self_parse_fields = filter_parse_fields(self_fields)

         # Fields that are only inherited
         parent_fields = [f for f in all_fields if f not in self_fields]
         parent_parse_fields = filter_parse_fields(parent_fields)
      %>
      procedure Initialize_Fields_For_${cls.kwless_raw_name}
        (Self : ${cls.name}
         % for f in all_parse_fields:
         ; ${f.names.codegen} : ${f.type.name}
         % endfor
        ) is
      begin
         ## Re-use the parent node's fields initializer, if any. No need to
         ## call the root node's initializer, as it must be called before any
         ## kind-specific initializer.
         % if parent_fields and not cls.base.is_root_node:
            Initialize_Fields_For_${cls.base.kwless_raw_name}
              (Self${''.join(
                    ', {}'.format(f.names.codegen) for f in parent_parse_fields
                 )});
         % endif

         ## Then initialize fields unique to this node
         % for f in self_parse_fields:
            Self.${f.names.codegen} := ${f.names.codegen};
         % endfor
         ${init_user_fields(cls, 'Self')}
      end Initialize_Fields_For_${cls.kwless_raw_name};
   % endif

   ## Field getters
   % for field in cls.get_parse_fields( \
      include_inherited=False, \
      predicate=lambda f: f.abstract or not f.is_overriding, \
   ):
      ${bare_field_body(field)}
   % endfor

   ## Generate the bodies of properties
   % for prop in cls.get_properties(predicate=lambda p: not p.external, \
                                    include_inherited=False):
   ${prop.prop_def}
   % endfor

   ## Generate bodies of untyped wrappers
   % for prop in cls.get_properties(include_inherited=False, \
                                    predicate=lambda f: \
                                              f.requires_untyped_wrapper):
   ${prop.untyped_wrapper_def}
   % endfor

   ${exts.include_extension(ext)}
</%def>
