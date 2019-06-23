## vim: filetype=makoada

<%namespace name="exts"         file="extensions.mako" />
<%namespace name="prop_helpers" file="properties/helpers.mako" />


<%def name="bare_node_converters(cls)">
   function ${cls.internal_converter(T.root_node)} is
      new Ada.Unchecked_Conversion (${root_node_type_name}, ${cls.name});
   function ${T.root_node.internal_converter(cls)} is
      new Ada.Unchecked_Conversion (${cls.name}, ${root_node_type_name});
</%def>

<%def name="public_incomplete_decl(cls)">
   type ${cls.value_type_name()};
   type ${cls.name} is access all ${cls.value_type_name()}'Class;

   ${cls.null_constant} : constant ${cls.name} := null;

   % if not cls.is_root_node:
      ${bare_node_converters(cls)}
   % endif
</%def>

<%def name="logic_helpers()">

   ## Generate logic/predicate binders for the properties which require it.
   ## Note that we need to generate them before the properties bodies, because
   ## they'll be used in the bodies.

   % for cls in no_builtins(ctx.astnode_types):
      % for prop in cls.get_properties(include_inherited=False):
         ${prop_helpers.logic_predicates(prop)}
      % endfor
   % endfor

   ## Generate logic converters, equality predicates, and binders
   % for conv_prop in ctx.sorted_logic_converters:
      ${prop_helpers.logic_converter(conv_prop)}
   % endfor
   % for eq_prop in ctx.sorted_logic_comparers:
   ${prop_helpers.logic_equal(eq_prop)}
   % endfor
</%def>


<%def name="bare_field_decl(field)">
   <% type_name = field.struct.value_type_name() %>

   function ${field.name}
     (Node : access ${type_name}'Class) return ${field.type.name};
</%def>


<%def name="bare_field_body(field)">
   <% type_name = field.struct.value_type_name() %>

   function ${field.name}
     (Node : access ${type_name}'Class) return ${field.type.name}
   is
      <%def name="return_value(node_expr)">
         return ${field.type.extract_from_storage_expr(
                     node_expr=node_expr,
                     base_expr='{}.{}'.format(node_expr, field.name)
         )};
      </%def>

      % if field.abstract:
         Kind : constant ${field.struct.ada_kind_range_name} :=
            ${T.root_node.internal_conversion(field.struct, 'Node')}.Kind;
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
                     declare
                        N : constant ${cf.struct.name} :=
                           ${cf.struct.internal_conversion(field.struct,
                                                           'Node')};
                     begin
                        ${return_value('N')}
                     end;
                  % endif
            % endfor
         end case;
      % else:
         ${return_value('Node')}
      % endif
   end;
</%def>


<%def name="field_decl(field)">
   <%
      type_name = field.struct.entity.api_name
      ret_type = field.type.entity if field.type.is_ast_node else field.type
   %>

   function ${field.name}
     (Node : ${type_name}'Class) return ${ret_type.api_name};
   ${ada_doc(field, 3)}

   ## If this field return an enum node, generate a shortcut to get the
   ## symbolic value.
   % if field.type.is_bool_node:
      function ${field.name} (Node : ${type_name}'Class) return Boolean;

   % elif field.type.is_enum_node:
      function ${field.name}
        (Node : ${type_name}'Class) return ${field.type.ada_kind_name};
   % endif
</%def>


<%def name="field_body(field)">
   <%
      type_name = field.struct.entity.api_name
      ret_type = field.type.entity if field.type.is_ast_node else field.type
      bare_type = field.struct
   %>

   function ${field.name}
     (Node : ${type_name}'Class) return ${ret_type.api_name}
   is
      Self   : constant ${bare_type.name} := ${bare_type.internal_conversion(
         T.root_node, 'Node.Internal.Node')};
      Result : constant ${field.type.name} := ${(
          field.type.extract_from_storage_expr(
              node_expr='Self',
              base_expr='Self.{}'.format(field.name)
          )
      )};
   begin
      Check_Safety_Net (Node.Safety_Net);
      % if field.type.is_ast_node:
         return (Internal   => (${T.root_node.internal_conversion(
                                     field.type, 'Result')},
                                Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
      % else:
         return Result;
      % endif
   end ${field.name};

   % if field.type.is_ast_node:
      <%
         node_expr = field.struct.internal_conversion(
            T.root_node, 'Node.Internal.Node')
         field_expr = '{}.{}'.format(node_expr, field.name)
         root_field_expr = T.root_node.internal_conversion(field.type,
                                                           field_expr)
      %>

      % if field.type.is_bool_node:
         function ${field.name} (Node : ${type_name}'Class) return Boolean is
           (${root_field_expr}.Kind
            = ${field.type.alternatives[0].type.ada_kind_name});

      % elif field.type.is_enum_node:
         function ${field.name}
           (Node : ${type_name}'Class) return ${field.type.ada_kind_name}
         is (${root_field_expr}.Kind);
      % endif
   % endif
</%def>

<%def name="node_fields(cls, emit_null=True)">
   <%
      fields = cls.get_fields(
         include_inherited=False,
         predicate=lambda f: (f.should_emit and
                              not f.abstract and
                              not f.null))
      ext = ctx.ext('nodes', cls.raw_name, 'components')
   %>
   % if fields:
      % for f in fields:
         ${f.name} : aliased ${f.type.storage_type_name}
            := ${f.type.storage_nullexpr};
         ${ada_doc(f, 9)}
      % endfor

      % if cls == ctx.ple_unit_root:
         Is_Env_Populated : Boolean := False;
         --  Whether this PLE unit root was processed by Populate_Lexical_Env
      % endif

      ${exts.include_extension(ext)}
   % elif emit_null:
      null;
   % endif
</%def>

<%def name="private_decl(cls)">

   <%
      type_name = cls.value_type_name()
      base_name = cls.base.name
      ext = ctx.ext('nodes', cls.raw_name, 'public_decls')
   %>

   type ${type_name} is ${"abstract" if cls.abstract else ""}
      new ${cls.base.value_type_name()} with record
      ${node_fields(cls)}
   end record;

   ## Fields initialization helper
   <% fields = cls.get_parse_fields(
         predicate = lambda f: not f.abstract and not f.null) %>
   % if fields:
      procedure Initialize_Fields_For_${cls.kwless_raw_name}
        (Self : access ${cls.value_type_name()}'Class
         % for f in fields:
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
      ${prop.prop_decl}
   % endfor

   ${exts.include_extension(ext)}

   % if cls.env_spec:

      function ${cls.name}_Pre_Env_Actions
        (Self                : access ${type_name}'Class;
         Bound_Env, Root_Env : AST_Envs.Lexical_Env;
         Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

      % if cls.env_spec.post_actions:
         procedure ${cls.name}_Post_Env_Actions
           (Self                : access ${type_name}'Class;
            Bound_Env, Root_Env : AST_Envs.Lexical_Env);
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

   type_name = cls.value_type_name()

   ext = ctx.ext('nodes', cls.raw_name, 'bodies')
   %>

   ###################################
   ## Lexical Environments Handling ##
   ###################################

   % if cls.env_spec:

   <% call_prop = cls.env_spec._render_field_access %>

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

         Add_To_Env (${root_node_type_name} (Self),
                     Mapping, Initial_Env, Resolver);
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
            Ref_Env
              (${root_node_type_name} (Self),
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
         return Initial_Env;
      end if;

      G := Simple_Env_Getter (Initial_Env);
      % if has_dyn_env:
      if Initial_Env not in Root_Env | Empty_Env
         and then Initial_Env.Env.Node.Unit /= Self.Unit
      then
         G := Dyn_Env_Getter (${env_getter}'Access, Self);
      end if;
      % endif

      Self.Self_Env := AST_Envs.Create_Lexical_Env
        (Parent            => ${"No_Env_Getter" if add_env.no_parent else "G"},
         Node              => Self,
         Transitive_Parent => ${call_prop(add_env.transitive_parent_prop)},
         Owner             => Self.Unit);

      Initial_Env := Self.Self_Env;

      Register_Destroyable (Self.Unit, Self.Self_Env.Env);

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

   ${{"AddEnv":     emit_add_env,
      "AddToEnv":   emit_add_to_env,
      "RefEnvs":    emit_ref_env,
      "Do":         emit_do}[env_action.__class__.__name__](env_action)}
   </%def>

   <%
   env_getter = "{}_Initial_Env_Getter_Fn".format(cls.name)
   has_dyn_env = cls.env_spec.initial_env or cls.env_spec.env_hook_enabled
   %>

   % if has_dyn_env:
   ---------------------------
   -- Initial_Env_Getter_Fn --
   ---------------------------

   function ${env_getter} (E : Entity) return AST_Envs.Lexical_Env is
      Self_As_Root : constant ${root_node_type_name} := E.Node;
      Self : constant ${cls.name} := ${cls.name} (Self_As_Root);

      ## Define this constant so that the expressions below, which are expanded
      ## into property calls, can reference it as the currently bound
      ## environment.
      Bound_Env : constant Lexical_Env :=
        (if Self_As_Root.Parent /= null
         then Self_As_Root.Parent.Self_Env
         else Self_As_Root.Self_Env);

      Initial_Env : Lexical_Env := Bound_Env;
   begin
      % if cls.env_spec.env_hook_enabled:
         ${ctx.env_hook_subprogram.fqn}
           (Self_As_Root.Unit, ${cls.env_spec.env_hook_arg_expr});
      % endif
      % if cls.env_spec.initial_env:
      Initial_Env := ${cls.env_spec.initial_env_expr};
      % endif
      return Initial_Env;
   end ${env_getter};

   % endif

   function ${cls.name}_Pre_Env_Actions
     (Self                : access ${type_name}'Class;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env
   is
      use AST_Envs;

      Self_As_Root : constant ${root_node_type_name} :=
         ${T.root_node.internal_conversion(cls, 'Self')};
      Initial_Env  : Lexical_Env := Bound_Env;

      % if cls.env_spec.adds_env:
      G : Env_Getter;
      % endif
   begin
      % if has_dyn_env:
         Initial_Env := ${env_getter}
           ((Node => Self_As_Root,
             Info => ${T.entity_info.nullexpr}));
      % endif

      % for action in cls.env_spec.pre_actions:
      ${emit_env_action(action)}
      % endfor

      return Initial_Env;
   end;

   ## If the node class adds an env, then the environement in which node is is
   ## the parent env.

   ## Emit Post_Env_Actions only if needed

   % if cls.env_spec.post_actions:
      procedure ${cls.name}_Post_Env_Actions
        (Self                : access ${type_name}'Class;
         Bound_Env, Root_Env : AST_Envs.Lexical_Env)
      is
         use AST_Envs;
         Initial_Env : Lexical_Env := Bound_Env;
      begin
         % for action in cls.env_spec.post_actions:
         ${emit_env_action (action)}
         % endfor
      end;
   % endif

   % endif

   ## Fields initialization helper
   <%
      predicate = lambda f: not f.abstract and not f.null

      # All fields to initialize
      all_fields = cls.get_parse_fields(include_inherited=True,
                                        predicate=predicate)

      # Fields unique to this node (not inherited)
      self_fields = cls.get_parse_fields(include_inherited=False,
                                         predicate=predicate)

      # Fields that are only inherited
      parent_fields = all_fields[:len(all_fields) - len(self_fields)]
   %>
   % if all_fields:
      procedure Initialize_Fields_For_${cls.kwless_raw_name}
        (Self : access ${cls.value_type_name()}'Class
         % for f in all_fields:
         ; ${f.name} : ${f.type.name}
         % endfor
        ) is
      begin
         ## Re-use the parent node's fields initializer, if any
         % if parent_fields:
            Initialize_Fields_For_${cls.base.kwless_raw_name}
              (${cls.base.internal_conversion(cls, 'Self')},
               ${', '.join(str(f.name) for f in parent_fields)});
         % endif

         ## Then initialize fields unique to this node
         % for f in self_fields:
            Self.${f.name} := ${f.name};
         % endfor
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
