## vim: filetype=makoada

<%namespace name="exts"         file="extensions.mako" />
<%namespace name="prop_helpers" file="properties/helpers.mako" />

<%def name="public_incomplete_decl(cls)">
   type ${cls.value_type_name()};
   type ${cls.name} is access all ${cls.value_type_name()}'Class;
</%def>

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

   % for cls in no_builtins(ctx.astnode_types):
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


<%def name="bare_field_decl(field)">
   <% type_name = field.struct.value_type_name() %>

   function ${field.name}
     (Node : access ${type_name}'Class) return ${field.type.name};
</%def>


<%def name="bare_field_body(field)">
   <% type_name = field.struct.value_type_name() %>

   function ${field.name}
     (Node : access ${type_name}'Class) return ${field.type.name}
   is (${field.type.extract_from_storage_expr(
             node_expr='Node',
             base_expr='Node.{}'.format(field.name))});
</%def>


<%def name="field_decl(field)">
   <%
      type_name = field.struct.entity.api_name
      ret_type = field.type.entity if field.type.is_ast_node else field.type
   %>

   function ${field.name}
     (Node : ${type_name}'Class) return ${ret_type.api_name};
   ${ada_doc(field, 6)}

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
      bare_type = field.struct.name
   %>

   function ${field.name}
     (Node : ${type_name}'Class) return ${ret_type.api_name}
   is
      Self   : constant ${bare_type} := ${bare_type} (Node.Internal.El);
      Result : constant ${field.type.name} := ${(
          field.type.extract_from_storage_expr(
              node_expr='Self',
              base_expr='Self.{}'.format(field.name)
          )
      )};
   begin
      % if field.type.is_ast_node:
         return (Internal => (${root_node_type_name} (Result),
                              Node.Internal.Info));
      % else:
         return Result;
      % endif
   end ${field.name};

   % if field.type.is_ast_node:
      <% field_expr = ('{} (Node.Internal.El).{}'
                       .format(bare_type, field.name)) %>

      % if field.type.is_bool_node:
         function ${field.name} (Node : ${type_name}'Class) return Boolean
         is (${field_expr}.all in
                ${field.type.alternatives[0].type.name}_Type'Class);

      % elif field.type.is_enum_node:
         function ${field.name}
           (Node : ${type_name}'Class) return ${field.type.ada_kind_name}
         is (${field_expr}.Kind);
      % endif
   % endif
</%def>

<%def name="node_fields(cls, emit_null=True)">
   <%
      fields = cls.get_fields(include_inherited=False,
                              predicate=lambda f: f.should_emit)
      ext = ctx.ext('nodes', cls.raw_name, 'components')
   %>
   % if fields:
       % for f in fields:
            ${f.name} : aliased ${f.type.storage_type_name}
               := ${f.type.storage_nullexpr};
            ${ada_doc(f, 12)}
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

   ## Field getters
   % for field in cls.get_parse_fields(include_inherited=False):
      ${bare_field_decl(field)}
   % endfor

   ## Properties
   % for prop in cls.get_properties(include_inherited=False):
      ${prop.prop_decl}
   % endfor

   ${exts.include_extension(ext)}

   % if not cls.is_env_spec_inherited:

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

   % if not cls.is_env_spec_inherited:

   <% call_prop = cls.env_spec._render_field_access %>

   <%def name="emit_add_to_env(exprs)">
      ## If we have an add_to_env specification, generate code to add elements
      ## to the lexical environment.

      declare
         Env : Lexical_Env :=
           ${call_prop(exprs.dest_env_prop) \
             if exprs.dest_env_prop else "Initial_Env"};

         MD  : constant ${T.env_md.name} :=
            ${(call_prop(exprs.metadata_prop)
               if exprs.metadata else 'No_Metadata')};

         Mappings : ${exprs.mappings_prop.type.name} :=
            ${call_prop(exprs.mappings_prop)};

         Resolver : constant Entity_Resolver :=
            ${("{}'Access".format(exprs.resolver.name)
               if exprs.resolver else 'null')};

      begin
         Add_To_Env (${root_node_type_name} (Self),
                     Env, Mappings, MD, Resolver);
         Dec_Ref (Env);
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
               ${ref_env.kind.value});
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

      Self.Self_Env := AST_Envs.Create
        (Parent            => ${"No_Env_Getter" if add_env.no_parent else "G"},
         Node              => Self,
         Transitive_Parent => ${add_env.transitive_parent},
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
      Self : constant ${cls.name} := ${cls.name} (E.El);

      ## Define this constant so that the expressions below, which are expanded
      ## into property calls, can reference it as the currently bound
      ## environment.
      Bound_Env : constant Lexical_Env :=
        (if Self.Parent /= null
         then Self.Parent.Self_Env
         else Self.Self_Env);

      Initial_Env : Lexical_Env := Bound_Env;
   begin
      % if cls.env_spec.env_hook_enabled:
         ${ctx.env_hook_subprogram.fqn}
           (Self.Unit, ${cls.env_spec.env_hook_arg_expr});
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

      Initial_Env : Lexical_Env := Bound_Env;

      % if cls.env_spec.adds_env:
      G : Env_Getter;
      % endif
   begin
      % if has_dyn_env:
         Initial_Env := ${env_getter}
           ((El => ${root_node_type_name} (Self), Info => No_Entity_Info));
      % endif

      % for action in cls.env_spec.pre_actions:
      ${emit_env_action (action)}
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

   ## Field getters
   % for field in cls.get_parse_fields(include_inherited=False):
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
