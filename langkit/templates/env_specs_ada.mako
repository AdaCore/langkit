## vim: filetype=makoada

<%def name="decl(env_spec)">
   % if env_spec.pre_actions:
      procedure ${env_spec.owner.raw_name}_Pre_Env_Actions
        (Self            : ${env_spec.owner.name};
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);
   % endif

   % if env_spec.post_actions:
      procedure ${env_spec.owner.raw_name}_Post_Env_Actions
        (Self : ${env_spec.owner.name}; State : in out PLE_Node_State);
   % endif
</%def>

<%def name="body(env_spec)">

   ###################################
   ## Lexical Environments Handling ##
   ###################################

   <% call_prop = env_spec._render_field_access %>

   <%def name="emit_set_initial_env(sie)">
      declare
         Env : constant ${T.DesignatedEnv.name} :=
           ${env_spec.initial_env_expr};
      begin
         Set_Initial_Env
           (Self,
            State,
            Env,
            DSL_Location => ${ascii_repr(sie.str_location)});
      end;
   </%def>

   <%def name="emit_add_to_env(exprs)">
      ## If we have an add_to_env specification, generate code to add elements
      ## to the lexical environment.

      declare
         Resolver : constant Entity_Resolver :=
            ${("{}'Access".format(exprs.resolver.names.codegen)
               if exprs.resolver else 'null')};

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
      begin
         % if is_array:
         for Mapping of Mappings.Items loop
         % endif

         Add_To_Env
           (Self,
            State,
            Mapping.Key,
            Mapping.Value,
            Mapping.Metadata,
            Resolver,
            Mapping.Dest_Env,
            DSL_Location => ${ascii_repr(exprs.str_location)});
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
            % if ref_env.dest_env_prop:
               ## Adding a reference on a foreign environment is unsound, but
               ## it's fine for the empty/root environments, as they don't
               ## trigger relocations.
               if AST_Envs.Is_Foreign_Strict (Env, Self) then
                  Raise_Property_Exception
                    (Self,
                     Property_Error'Identity,
                     "unsound foreign environment in RefEnvs ("
                     & "${ref_env.str_location})");
               end if;
            % endif

            Ref_Env
              (Self,
               Env,
               Ref_Env_Nodes,
               ${ref_env.resolver.names.codegen}'Access,
               ${ref_env.kind.value},
               ${("({} => True, others => False)"
                  .format(ref_env.category.camel_with_underscores)
                  if ref_env.category else "AST_Envs.All_Cats")},
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
            ${(
               call_prop(add_env.transitive_parent_prop)
               if add_env.transitive_parent_prop else
               'False'
            )};
         Names             : ${T.Symbol.array.name} :=
            ${call_prop(add_env.names_prop) if add_env.names_prop else 'null'};
      begin
         Add_Env (Self, State, No_Parent, Transitive_Parent, Names);
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

   ## Emit procedures for pre/post actions when needed

   % if env_spec.pre_actions:
      procedure ${env_spec.owner.raw_name}_Pre_Env_Actions
        (Self            : ${env_spec.owner.name};
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False) is
      begin
         % for action in env_spec.pre_actions:
            ${emit_env_action(action)}
         % endfor
      end;
   % endif

   % if env_spec.post_actions:
      procedure ${env_spec.owner.raw_name}_Post_Env_Actions
        (Self : ${env_spec.owner.name}; State : in out PLE_Node_State) is
      begin
         % for action in env_spec.post_actions:
            ${emit_env_action (action)}
         % endfor
      end;
   % endif

</%def>
