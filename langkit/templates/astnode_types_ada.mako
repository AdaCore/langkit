## vim: filetype=makoada

<%namespace name="exts"         file="extensions.mako" />
<%namespace name="prop_helpers" file="properties/helpers.mako" />


<%def name="logic_helpers()">

   pragma Warnings (Off, "referenced");
   type Logic_Converter_Default is null record;
   No_Logic_Converter_Default : constant Logic_Converter_Default :=
     (null record);

   function Convert
     (Self : Logic_Converter_Default;
      From : ${T.entity.name}) return ${T.entity.name}
   is
      pragma Unreferenced (Self);
   begin
      return From;
   end Convert;

   type Equals_Data_Default is null record;
   No_Equals_Data_Default : constant Equals_Data_Default := (null record);

   function Eq_Default
     (Data : Equals_Data_Default; L, R : ${T.entity.name}) return Boolean
   is (Equivalent (L, R))
      with Inline;
   pragma Warnings (On, "referenced");

   ## Generate logic/predicate binders for the properties which require it.
   ## Note that we need to generate them before the properties bodies, because
   ## they'll be used in the bodies.

   % for cls in ctx.astnode_types:
      % for prop in cls.get_properties(include_inherited=False):
         ${prop_helpers.logic_predicates(prop)}
      % endfor
   % endfor

   <% emitted_eq_props = set() %>
   ## Generate logic converters, equality predicates, and binders
   % for conv_prop, eq_prop in ctx.sorted_logic_binders:

      % if conv_prop:
         ${prop_helpers.logic_converter(conv_prop)}
      % endif
      % if eq_prop and eq_prop.uid not in emitted_eq_props:
         <% emitted_eq_props.add(eq_prop.uid) %>
         ${prop_helpers.logic_equal(eq_prop)}
      % endif

      ${prop_helpers.logic_binder(conv_prop, eq_prop)}
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
      ${node_expr}.${f.name} := ${f.type.storage_nullexpr};
   % endfor
</%def>


<%def name="bare_field_decl(field)">
   function ${field.name}
     (Node : ${field.struct.name}) return ${field.type.name};
</%def>


<%def name="bare_field_body(field)">
   function ${field.name}
     (Node : ${field.struct.name}) return ${field.type.name}
   is
      <%def name="return_value(cf, node_expr)">
         return ${field.type.extract_from_storage_expr(
                     node_expr=node_expr,
                     base_expr='{}.{}'.format(node_expr, cf.name)
                )};
      </%def>

      % if field.abstract:
         Kind : constant ${field.struct.ada_kind_range_name} := Node.Kind;
      % endif
   begin
      % if field.abstract:
         case Kind is
            % for cf in field.concrete_fields:
               when ${' | '.join(n.ada_kind_name
                                 for n in cf.struct.concrete_subclasses)} =>
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
      type_name = field.struct.entity.api_name
      ret_type = field.type.entity if field.type.is_ast_node else field.type
   %>

   function ${field.api_name}
     (Node : ${type_name}'Class) return ${ret_type.api_name};
   ${ada_doc(field, 3)}

   ## If this field return an enum node, generate a shortcut to get the
   ## symbolic value.
   % if field.type.is_bool_node:
      function ${field.api_name} (Node : ${type_name}'Class) return Boolean;

   % elif field.type.is_enum_node:
      function ${field.api_name}
        (Node : ${type_name}'Class) return ${field.type.ada_kind_name};
   % endif
</%def>


<%def name="field_body(field)">
   <%
      type_name = field.struct.entity.api_name
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

      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.${field.name} (Node.Internal.Node);
      % if field.type.is_ast_node:
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
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
         ; ${f.name} : ${f.type.name}
         % endfor
        );
   % endif

   ## Field getters
   % for field in cls.get_parse_fields( \
      include_inherited=False, \
      predicate=lambda f: f.abstract or not f.overriding, \
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

      % if cls.env_spec.pre_actions:
         procedure ${cls.raw_name}_Pre_Env_Actions
           (Self            : ${cls.name};
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);
      % endif

      % if cls.env_spec.post_actions:
         procedure ${cls.raw_name}_Post_Env_Actions
           (Self : ${cls.name}; State : in out PLE_Node_State);
      % endif

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

   ###################################
   ## Lexical Environments Handling ##
   ###################################

   % if cls.env_spec and cls.env_spec.actions:

   <%
      call_prop = cls.env_spec._render_field_access

      # Whether the initial env for this node is dynamic
      has_dyn_env = cls.env_spec.initial_env is not None

      # Name of the function to call in order to get the initial lexical env
      # for this node. Useful only when the initial env is dynamic.
      env_getter = "{}_Initial_Env_Getter_Fn".format(cls.name)
   %>

   <%def name="emit_set_initial_env(sie)">
      declare
         Name_Result : ${T.Symbol.name} :=
            ${(cls.env_spec.initial_env_name_expr
               if sie.name_prop
               else 'null')};
      begin
         Set_Initial_Env (Self, State, Name_Result, ${env_getter}'Access);
      end;
   </%def>

   <%def name="emit_add_to_env(exprs)">
      ## If we have an add_to_env specification, generate code to add elements
      ## to the lexical environment.

      declare
         ## There are two modes: either the mappings expression returns an
         ## array, in which case we must process all its elements, either it's
         ## just one mapping.
         <% is_array = exprs.mappings_prop.type.is_array %>

         % if is_array:
         Mappings : ${exprs.mappings_prop.type.name} :=
            ${call_prop(exprs.mappings_prop)};
         % else:
         Mapping : ${exprs.mappings_prop.type.name} :=
            ${call_prop(exprs.mappings_prop)};
         % endif

         Resolver : constant Entity_Resolver :=
            ${("{}'Access".format(exprs.resolver.name)
               if exprs.resolver else 'null')};
      begin
         % if is_array:
         for Mapping of Mappings.Items loop
         % endif

         Add_To_Env
           (Self, Mapping, State, Resolver,
            DSL_Location => ${('""'
                               if exprs.unsound else
                               string_repr(exprs.str_location))});
         % if not is_array:
         Dec_Ref (Mapping.Dest_Env);
         % endif

         % if is_array:
         end loop;
         Dec_Ref (Mappings);
         % endif
      end;
   </%def>

   <%def name="emit_ref_env(ref_env)">
      % if ref_env.cond_prop:
      if ${call_prop(ref_env.cond_prop)} then
      % endif

         declare
            Ref_Env_Nodes : ${ref_env.nodes_property.type.name} :=
               ${call_prop(ref_env.nodes_property)};

            Env : Lexical_Env :=
              ${(call_prop(ref_env.dest_env_prop)
                 if ref_env.dest_env_prop else "Self.Self_Env")};
         begin
            % if ref_env.dest_env_prop and not ref_env.unsound:
               ## Adding a reference on a foreign environment is unsound, but
               ## it's fine for the empty/root environments, as they don't
               ## trigger relocations.
               if Is_Foreign_Strict (Env, Self) then
                  raise Property_Error with
                     "unsound foreign environment in RefEnvs ("
                     & "${ref_env.str_location})";
               end if;
            % endif

            Ref_Env
              (Self,
               Env,
               Ref_Env_Nodes,
               ${ref_env.resolver.name}'Access,
               ${ref_env.kind.value},
               ${("({} => True, others => False)"
                  .format(ref_env.category.camel_with_underscores)
                  if ref_env.category else "All_Cats")},
               ${ref_env.shed_rebindings});
            Dec_Ref (Ref_Env_Nodes);
         end;

      % if ref_env.cond_prop:
      end if;
      % endif
   </%def>

   <%def name="emit_add_env(add_env)">
      if Add_To_Env_Only then
         return;
      end if;

      declare
         No_Parent         : constant Boolean :=
            ${'True' if add_env.no_parent else 'False'};
         Transitive_Parent : constant Boolean :=
            ${call_prop(add_env.transitive_parent_prop)};
         Resolver          : constant Lexical_Env_Resolver :=
            ${"{}'Access".format(env_getter) if has_dyn_env else 'null'};
         Names             : ${T.Symbol.array.name} :=
            ${call_prop(add_env.names_prop) if add_env.names_prop else 'null'};
      begin
         Add_Env (Self, State, No_Parent, Transitive_Parent, Resolver, Names);
      end;
   </%def>

   <%def name="emit_do(do_action)">
      declare
         Dummy : ${do_action.do_property.type.name};
      begin
         Dummy := ${call_prop(do_action.do_property)};
         % if do_action.do_property.type.is_refcounted:
         Dec_Ref (Dummy);
         % endif
      end;
   </%def>

   <%def name="emit_env_action(env_action)">

   ## This function is an explicit dispatch table on action's class, calling
   ## the proper emit function for a given action. It is quite ugly but is
   ## still the best way I found that did not imply making a lot of round trips
   ## between Mako and regular python.

   ${{
      "SetInitialEnv": emit_set_initial_env,
      "AddEnv":        emit_add_env,
      "AddToEnv":      emit_add_to_env,
      "RefEnvs":       emit_ref_env,
      "Do":            emit_do}[env_action.__class__.__name__](env_action)}
   </%def>

   % if has_dyn_env:
   ---------------------------
   -- Initial_Env_Getter_Fn --
   ---------------------------

   function ${env_getter} (E : Entity) return Lexical_Env is
      Self : constant ${cls.name} := E.Node;

      ## Define this constant so that the expressions below, which are expanded
      ## into property calls, can reference it as the currently bound
      ## environment.
      Bound_Env : constant Lexical_Env :=
        (if Self.Parent /= null
         then Self.Parent.Self_Env
         else Self.Self_Env);

      Initial_Env : Lexical_Env := Bound_Env;
   begin
      Initial_Env := ${cls.env_spec.initial_env_expr};

      if Initial_Env.Kind /= Static_Primary then
         raise Property_Error with
            "Cannot set an env that is not static-primary as the initial env";
      end if;

      % if not cls.env_spec.initial_env.unsound:
         ## Having a parent environment that is foreign is unsound, except for
         ## the empty/root environments, as no relocation is needed for them.
         if Is_Foreign_Strict (Initial_Env, Self) then
            raise Property_Error with
               "unsound foreign environment in SetInitialEnv ("
               & "${cls.env_spec.initial_env.str_location}" & ")";
         end if;
      % endif

      return Initial_Env;
   end ${env_getter};

   % endif

   ## Emit procedures for pre/post actions when needed

   % if cls.env_spec.pre_actions:
      procedure ${cls.raw_name}_Pre_Env_Actions
        (Self            : ${cls.name};
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False) is
      begin
         % for action in cls.env_spec.pre_actions:
            ${emit_env_action(action)}
         % endfor
      end;
   % endif

   % if cls.env_spec.post_actions:
      procedure ${cls.raw_name}_Post_Env_Actions
        (Self : ${cls.name}; State : in out PLE_Node_State) is
      begin
         % for action in cls.env_spec.post_actions:
            ${emit_env_action (action)}
         % endfor
      end;
   % endif

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
         ; ${f.name} : ${f.type.name}
         % endfor
        ) is
      begin
         ## Re-use the parent node's fields initializer, if any. No need to
         ## call the root node's initializer, as it must be called before any
         ## kind-specific initializer.
         % if parent_fields and not cls.base.is_root_node:
            Initialize_Fields_For_${cls.base.kwless_raw_name}
              (Self${''.join(
                    ', {}'.format(f.name) for f in parent_parse_fields
                 )});
         % endif

         ## Then initialize fields unique to this node
         % for f in self_parse_fields:
            Self.${f.name} := ${f.name};
         % endfor
         ${init_user_fields(cls, 'Self')}
      end Initialize_Fields_For_${cls.kwless_raw_name};
   % endif

   ## Field getters
   % for field in cls.get_parse_fields( \
      include_inherited=False, \
      predicate=lambda f: f.abstract or not f.overriding, \
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
