## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />
<%namespace name="prop_helpers" file="properties/helpers.mako" />

<%def name="public_incomplete_decl(cls)">

   type ${cls.value_type_name()};
   type ${cls.name()} is access all ${cls.value_type_name()}'Class;
   ${ada_doc(cls, 3)}

</%def>

<%def name="logic_helpers()">

   pragma Warnings (Off, "referenced");
   function Eq_Default (L, R : ${T.entity.name()}) return Boolean is
     (Is_Equivalent (L, R))
      with Inline;

   type Logic_Converter_Default is null record;
   No_Logic_Converter_Default : constant Logic_Converter_Default :=
     (null record);

   function Convert
     (Self : Logic_Converter_Default;
      From : ${T.entity.name()}) return ${T.entity.name()}
   is
      pragma Unreferenced (Self);
   begin
      Inc_Ref (From);
      return From;
   end Convert;
   pragma Warnings (On, "referenced");

   ## Generate logic/predicate binders for the properties who require it. Note
   ## that we need to generate them before the properties bodies, because
   ## they'll be used in the bodies.
   ## TODO: Filtering logic duplicated with pkg_ast_types_body_ada.mako. See if
   ## we can share in helpers.

   % for cls in filter(lambda t: not t.is_builtin(), ctx.astnode_types):
   % for prop in cls.get_properties(include_inherited=False):
   ${prop_helpers.generate_logic_predicates(prop)}
   % endfor
   % endfor

   ## Generate logic converters
   % for el in set(p for (p, _) in ctx.logic_binders if p):
   ${prop_helpers.generate_logic_converter(el)}
   % endfor

   ## Generate logic equal function wrappers
   % for el in set(p for (_, p) in ctx.logic_binders if p):
   ${prop_helpers.generate_logic_equal(el)}
   % endfor

   ## Generate logic binders
   % for conv_prop, eq_prop in ctx.logic_binders:
   ${prop_helpers.generate_logic_binder(conv_prop, eq_prop)}
   % endfor
</%def>


<%def name="field_decl(field)">
   <% type_name = field.struct.value_type_name() %>

   function ${field.name}
     (Node : access ${type_name}) return ${field.type.name()};
   ${ada_doc(field, 6)}

   ## If this field return an enum node, generate a shortcut to get the
   ## symbolic value.
   % if is_ast_node(field.type):
      % if field.type.is_bool_node:
         function ${field.name}
           (Node : access ${type_name}'Class)
            return Boolean
            with Inline => True;

      % elif field.type.is_enum_node:
         function ${field.name}
           (Node : access ${type_name}'Class)
            return ${field.type.ada_kind_name()}
            with Inline => True;
      % endif
   % endif
</%def>


<%def name="public_decl(cls)">

   <%
      type_name = cls.value_type_name()
      base_name = cls.base().name()
      ext = ctx.ext("nodes", cls.name(), "public_decls")
      memoized_properties = cls.get_memoized_properties(include_inherited=True)

      logic_vars = cls.get_fields(
         include_inherited=True,
         predicate=lambda f: is_logic_var(f.type)
      )
   %>

   --
   --  ${cls.name()}
   --

   type ${type_name} is ${"abstract" if cls.abstract else "" }
      new ${cls.base().value_type_name()} with private;

   % if not cls.abstract:

      overriding function Kind
        (Node : access ${type_name}) return ${root_node_kind_name};
      overriding function Kind_Name
        (Node : access ${type_name}) return String;

      ## No need to regenerate these primitives for list types as the
      ## inheritted one already fit.
      % if not cls.is_list_type:
         overriding function Image
           (Node : access ${type_name}) return String;

         overriding function Child_Count
           (Node : access ${type_name}) return Natural;
         overriding procedure Get_Child
           (Node            : access ${type_name};
            Index           : Positive;
            Index_In_Bounds : out Boolean;
            Result          : out ${root_node_type_name});

         overriding procedure Print
           (Node   : access ${type_name};
            Prefix : String := "");

      % endif
   % endif

   % if logic_vars:
   procedure Assign_Names_To_Logic_Vars_Impl
     (Node : access ${type_name});
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.
   % endif

   ## Public field getters

   % if not cls.is_env_spec_inherited and cls.env_spec._add_env:
      overriding function Node_Env
        (Node : access ${type_name}) return AST_Envs.Lexical_Env;
   % endif

   % for field in cls.get_fields(include_inherited=False, \
                                 predicate=library_public_field):
      ${field_decl(field)}
   % endfor

   % for prop in cls.get_properties(include_inherited=False, \
                                    predicate=library_public_field):
      ${prop.prop_decl}
   % endfor

   ${exts.include_extension(ext)}
</%def>

<%def name="node_fields(cls, emit_null=True)">
   <%
      fields = cls.get_fields(include_inherited=False,
                              predicate=lambda f: f.should_emit)
      ext = ctx.ext("nodes", cls.name(), "components")
      memoized_properties = cls.get_memoized_properties()
   %>
   % if fields or ext or memoized_properties:
       % for f in fields:
            ${f.name} : aliased ${f.type.storage_type_name()}
               := ${f.type.storage_nullexpr()};
            ${ada_doc(f, 12)}
       % endfor
       ${exts.include_extension(ext)}
   % elif emit_null:
      null;
   % endif

   ## Put all state flags first, and only then cached values not to wast too
   ## much space with alignment. TODO: put all the flags in a packed array.
   % for p in memoized_properties:
      ${p.memoization_state_field_name} : Memoization_State := Not_Computed;
   % endfor
   % for p in memoized_properties:
      ${p.memoization_value_field_name} : ${p.type.name()};
   % endfor
</%def>

<%def name="private_decl(cls)">

   <%
      type_name = cls.value_type_name()
      base_name = cls.base().name()
      memoized_properties = cls.get_memoized_properties(include_inherited=True)
   %>

   type ${type_name} is ${"abstract" if cls.abstract else ""}
      new ${cls.base().value_type_name()} with record
      ${node_fields(cls)}
   end record;

   % if not cls.abstract and not cls.is_list_type:

      % if memoized_properties:
         overriding procedure Reset_Property_Caches
           (Node : access ${type_name});
      % endif

      overriding procedure Destroy_Node
        (Node : access ${cls.value_type_name()});

   % endif

   ## Private field getters

   <% library_private_field = lambda f: not library_public_field(f) %>

   % for field in cls.get_fields(include_inherited=False, \
                                 predicate=library_private_field):
      ${field_decl(field)}
   % endfor

   % for prop in cls.get_properties(include_inherited=False, \
                                    predicate=library_private_field):
      % if prop.dispatching:
         ${prop.prop_decl}
      % endif
   % endfor

   % if not cls.is_env_spec_inherited:

      overriding function Pre_Env_Actions
        (Self                  : access ${type_name};
         Current_Env, Root_Env : AST_Envs.Lexical_Env;
         Add_To_Env_Only       : Boolean := False)
         return AST_Envs.Lexical_Env;

   % if cls.env_spec.has_post_actions:
      overriding procedure Post_Env_Actions
        (Self                  : access ${type_name};
         Current_Env, Root_Env : AST_Envs.Lexical_Env);
   % endif

   % endif

</%def>


<%def name="body_decl(cls)">

   <%
      library_private_field = lambda f: not library_public_field(f)
      props = cls.get_properties(
         include_inherited=False,
         predicate=lambda f: not library_public_field(f) and not f.dispatching
      )
      untyped_wrappers = cls.get_properties(
         include_inherited=False,
         predicate=lambda f: f.requires_untyped_wrapper
      )
   %>

   % if props:
      --
      --  Private and non-dispatching primitives for ${cls.name()}
      --

      % for prop in props:
         ${prop.prop_decl}
      % endfor
   % endif

   % if untyped_wrappers:
      --
      --  Untyped wrappers for ${cls.name()}
      --

      % for prop in props:
         ${prop.untyped_wrapper_decl}
      % endfor
   % endif

</%def>


<%def name="body(cls)">

   --
   --  Primitives for ${cls.name()}
   --

   <%
   # Keep a list of ASTNode fields
   astnode_fields = cls.get_parse_fields(lambda f: is_ast_node(f.type))

   # Keep a list of user fields
   user_fields = cls.get_user_fields()

   # Keep a list of fields that are annotated with repr
   repr_fields = cls.get_parse_fields(lambda f: f.repr)

   type_name = cls.value_type_name()

   ext = ctx.ext("nodes", cls.name(), "bodies")

   memoized_properties = cls.get_memoized_properties(include_inherited=True)

   logic_vars = cls.get_fields(
      include_inherited=True,
      predicate=lambda f: is_logic_var(f.type)
   )
   %>

   % if not cls.abstract:

      ----------
      -- Kind --
      ----------

      overriding function Kind
        (Node : access ${type_name}) return ${root_node_kind_name}
      is
         pragma Unreferenced (Node);
      begin
         return ${cls.ada_kind_name()};
      end Kind;

      ---------------
      -- Kind_Name --
      ---------------

      overriding function Kind_Name
        (Node : access ${type_name}) return String
      is
         pragma Unreferenced (Node);
      begin
         return "${cls.repr_name()}";
      end Kind_Name;

      ## No need to regenerate these primitives for list types as the
      ## inheritted one already fit.
      % if not cls.is_list_type:

      -----------
      -- Image --
      -----------

      overriding function Image
        (Node : access ${type_name}) return String
      is
         Class_Wide_Node : constant ${cls.name()} := ${cls.name()} (Node);
         Result          : Unbounded_String;
      begin
         Append (Result, Class_Wide_Node.Kind_Name);
         Append (Result, '[');
         Append (Result, Image (Node.Sloc_Range));
         Append (Result, "](");

         % for i, field in enumerate(repr_fields):
             % if i > 0:
                 Append (Result, ", ");
             % endif

             % if field.type.is_ptr:
                 if Node.${field.name} /= null then
             % endif

             % if is_ast_node(field.type):
                Append (Result,
                        Image (${root_node_type_name} (Node.${field.name})));
             % elif is_token_type(field.type):
                Append (Result, Image (Token (Node, Node.${field.name})));
             % else:
                Append (Result, Image (Node.${field.name}));
             % endif

             % if field.type.is_ptr:
                 else
                    Append (Result, "None");
                 end if;
             % endif
         % endfor

         Append (Result, ')');
         return To_String (Result);
      end Image;

      -----------------
      -- Child_Count --
      -----------------

      overriding function Child_Count
        (Node : access ${type_name}) return Natural
      is
         pragma Unreferenced (Node);
      begin
         return ${len(astnode_fields)};
      end Child_Count;

      ---------------
      -- Get_Child --
      ---------------

      overriding procedure Get_Child
        (Node            : access ${type_name};
         Index           : Positive;
         Index_In_Bounds : out Boolean;
         Result          : out ${root_node_type_name})
      is
         ## Some ASTnodes have no ASTNode child: avoid the "unused parameter"
         ## compilation warning for them.
         % if not astnode_fields:
             pragma Unreferenced (Node);
             pragma Unreferenced (Result);
         % endif
      begin
         case Index is
             % for i, field in enumerate(astnode_fields, 1):
                 when ${i} =>
                     Result := ${root_node_type_name} (Node.${field.name});
                     Index_In_Bounds := True;
             % endfor
             when others =>
                Index_In_Bounds := False;
                Result := null;
         end case;
      end Get_Child;

      -----------
      -- Print --
      -----------

      overriding procedure Print
        (Node   : access ${type_name};
         Prefix : String := "")
      is
         Class_Wide_Node : constant ${cls.name()} := ${cls.name()} (Node);
         Attr_Prefix     : constant String := Prefix & "|";
         Children_Prefix : constant String := Prefix & "|  ";
      begin
         Put_Line
           (Prefix & Class_Wide_Node.Kind_Name
            & "[" & Image (Node.Sloc_Range) & "]");

         % for i, field in enumerate(repr_fields):
            % if field.type.is_ptr:
               Put (Attr_Prefix & "${field._name.lower}:");
               if Node.${field.name} /= null then
                  New_Line;
                  Node.${field.name}.Print (Children_Prefix);
               else
                  Put_Line (" <null>");
               end if;
            % elif is_token_type(field.type):
               Put_Line
                 (Attr_Prefix & "${field._name.lower}: "
                  & Text (${field.name} (Node)));
            % else:
               Put_Line (Attr_Prefix & "${field._name.lower}: "
                         & Image (Node.${field.name}));
            % endif
         % endfor

      end Print;

      % if memoized_properties:
         overriding procedure Reset_Property_Caches
           (Node : access ${cls.value_type_name()})
         is
         begin
            % for p in memoized_properties:
               % if p.type.is_refcounted():
                  if Node.${p.memoization_state_field_name} = Computed then
                     Dec_Ref (Node.${p.memoization_value_field_name});
                  end if;
               % endif
               Node.${p.memoization_state_field_name} := Not_Computed;
            % endfor
         end Reset_Property_Caches;
      % endif

      ------------------
      -- Destroy_Node --
      ------------------

      overriding procedure Destroy_Node
        (Node : access ${cls.value_type_name()})
      is
      begin
         ## When no extension is registered, we don't need to recurse on the
         ## tree at all.
         if Langkit_Support.Extensions.Has_Extensions then
            Node.Free_Extensions;
         end if;
         % if memoized_properties:
            Node.Reset_Property_Caches;
         % endif
         % for field in user_fields:
            % if is_logic_var(field.type):
               Eq_Node.Refs.Destroy (Node.${field.name});
            % endif
         % endfor
      end Destroy_Node;

      % endif

   % endif

   % if logic_vars:
   procedure Assign_Names_To_Logic_Vars_Impl
     (Node : access ${type_name})
   is
   begin
      % for f in logic_vars:
      Node.${f.name}.Dbg_Name :=
        new String'(Image (Node.Short_Image) & ".${f.name}");
      % endfor
   end Assign_Names_To_Logic_Vars_Impl;
   % endif

   ###################################
   ## Lexical Environments Handling ##
   ###################################

   % if not cls.is_env_spec_inherited:

   <% call_prop = cls.env_spec._render_field_access %>

   <%def name="emit_add_to_env(exprs)">
      ## If we have an _add_to_env specification, we generate code to
      ## add elements to the lexical environment.

      <% md = call_prop(exprs.metadata) if exprs.metadata else "No_Metadata" %>

      declare
         Env : Lexical_Env :=
           ${call_prop(exprs.dest_env) if exprs.dest_env else "Initial_Env"};

         ${"Mappings" if is_array_type(exprs.mappings.type) else "B"} :
           ${exprs.mappings.type.name()} := ${call_prop(exprs.mappings)};
      begin
         % if is_array_type(exprs.mappings.type):
         for B of mappings.Items loop
         % endif
            ## Add the element to the environment
            Add (Self  => Env,
                 Key   => B.F_Key,
                 Value => ${root_node_type_name} (B.F_Val),
                 MD    => ${md}

                 % if exprs.resolver:
                 , Resolver => ${exprs.resolver.name}'Access
                 % endif
            );

            ## If we're adding the element to an env that belongs to a different
            ## unit, then:
            if Env /= Empty_Env
               and then (Env = Root_Env or else Env.Node.Unit /= Self.Unit)
            then
               ## Add the env, the key, and the value to the list of entries
               ## contained in other units, so we can remove them when reparsing
               ## val's unit.
               Get_Lex_Env_Data (B.F_Val).Is_Contained_By.Append
                 ((Env, B.F_Key, ${root_node_type_name} (B.F_Val)));

               if Env /= Root_Env then
                  ## Add Val to the list of entries that env's unit contains, so
                  ## that when the unit is reparsed, we can call add_to_env
                  ## again on those nodes.
                  Get_Lex_Env_Data (Env.Node).Contains.Append
                    (${root_node_type_name} (B.F_Val));
               end if;
            end if;
         
         % if is_array_type(exprs.mappings.type):
         end loop;
         Dec_Ref (Mappings);
         % endif
      end;
   </%def>

   <%
   env_getter = "{}_Initial_Env_Getter_Fn".format(cls.name())
   has_dyn_env = cls.env_spec.initial_env or cls.env_spec.env_hook_enabled
   %>

   % if has_dyn_env:
   ---------------------------
   -- Initial_Env_Getter_Fn --
   ---------------------------

   function ${env_getter}
     (State : Env_Getter_State_T) return AST_Envs.Lexical_Env
   is
      Self        : ${cls.name()} := ${cls.name()} (State.Node);
      Current_Env : Lexical_Env :=
        (if State.Node.Parent /= null
         then State.Node.Parent.Self_Env
         else State.Node.Self_Env);
      Initial_Env : Lexical_Env := Current_Env;
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
   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   overriding function Pre_Env_Actions
     (Self                  : access ${type_name};
      Current_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only       : Boolean := False) return AST_Envs.Lexical_Env
   is
      use AST_Envs;

      Initial_Env : Lexical_Env := Current_Env;
      G_State     : Env_Getter_State_T :=
        (Node => ${root_node_type_name} (Self));

      % if cls.env_spec._add_env:
      G           : Env_Getter;
      % endif
   begin
      ## Super call

      % if cls.base().env_spec and cls.env_spec.call_parents:
         Initial_Env := Pre_Env_Actions
           (${cls.base().value_type_name()} (Self.all)'Access,
            Current_Env, Root_Env, Add_To_Env_Only);
      % endif

      ## initial_env

      % if has_dyn_env:
         Initial_Env := ${env_getter} (G_State);
      % endif

      ## add_to_env

      % for exprs in cls.env_spec.envs_expressions:
      % if not exprs.is_post:
      ${emit_add_to_env(exprs)}
      % endif
      % endfor

      --  Return early if we only want to run add_to_env actions
      if Add_To_Env_Only then
         return Initial_Env;
      end if;

      ## ref_envs

      % if cls.env_spec.ref_envs:
      declare
         To_Ref_Envs : ${cls.env_spec.ref_envs.type.name()}
           := ${call_prop(cls.env_spec.ref_envs)};
      begin
         for To_Ref_Env of To_Ref_Envs.Items loop
            Reference (Current_Env, To_Ref_Env, ${root_node_type_name} (Self));
         end loop;
         Dec_Ref (To_Ref_Envs);
      end;
      % endif

      ## add_env

      % if cls.env_spec._add_env:
         G := Simple_Env_Getter (Initial_Env);
         % if has_dyn_env:
         if Initial_Env not in Root_Env | Empty_Env 
            and then Initial_Env.Node.Unit /= Self.Unit
         then
            G := Dyn_Env_Getter (${env_getter}'Access, G_State);
         end if;
         % endif

         Self.Self_Env := AST_Envs.Create
           (Parent        => G,
            Node          => Self,
            Is_Refcounted => False);

         Register_Destroyable (Self.Unit, Self.Self_Env);
      % endif

      return Initial_Env;
   end Pre_Env_Actions;

   ## If the node class adds an env, then the environement in which node is is
   ## the parent env.

   % if cls.env_spec._add_env:

   --------------
   -- Node_Env --
   --------------

   overriding function Node_Env
     (Node : access ${type_name}) return AST_Envs.Lexical_Env
   is (AST_Envs.Get_Env (Node.Self_Env.Parent));
   % endif

   ## Emit Post_Env_Actions only if needed

   % if cls.env_spec.has_post_actions:
   ----------------------
   -- Post_Env_Actions --
   ----------------------

   overriding procedure Post_Env_Actions
     (Self                  : access ${type_name};
      Current_Env, Root_Env : AST_Envs.Lexical_Env)
   is
      use AST_Envs;
      Initial_Env : Lexical_Env := Current_Env;
   begin
      % if cls.base().env_spec and cls.env_spec.call_parents:
         Post_Env_Actions
           (${cls.base().value_type_name()} (Self.all)'Access,
            Current_Env, Root_Env);
      % endif

      #############################
      ## Post add_to_env actions ##
      #############################

      % for exprs in cls.env_spec.envs_expressions:
      % if exprs.is_post:
      ${emit_add_to_env(exprs)}
      % endif
      % endfor
   end Post_Env_Actions;
   % endif

   % endif

   ## Body of attribute getters

   % for field in cls.get_fields(include_inherited=False):
      function ${field.name}
        (Node : access ${type_name}) return ${field.type.name()}
      is
      begin
         return ${field.type.extract_from_storage_expr(
                      node_expr='Node',
                      base_expr='Node.{}'.format(field.name)
                  )};
      end ${field.name};

      % if is_ast_node(field.type):
         % if field.type.is_bool_node:
            function ${field.name}
              (Node : access ${type_name}'Class)
               return Boolean
            is (Node.${field.name}.all
                in ${field.type.alt_present.value_type_name()}'Class);

         % elif field.type.is_enum_node:
            function ${field.name}
              (Node : access ${type_name}'Class)
               return ${field.type.ada_kind_name()}
            is (Node.${field.name}.Kind);
         % endif
      % endif
   % endfor

   ## Generate the bodies of properties
   % for prop in cls.get_properties(predicate=lambda p: not p.external, \
                                    include_inherited=False):
   ${prop.prop_def}
   % endfor

   ${exts.include_extension(ext)}
</%def>
