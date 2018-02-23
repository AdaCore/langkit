## vim: filetype=makoada

<%namespace name="scopes"  file="scopes_ada.mako" />
<%namespace name="helpers" file="helpers.mako" />

## Regular property function


% if property.abstract_runtime_check:

${"overriding" if property.overriding else ""} function ${property.name}
  ${helpers.argument_list(property, property.dispatching)}
   return ${property.type.name}
is (raise Property_Error
    with "Property ${property.original_name} not implemented on type "
    & Kind_Name (${Self.type.name} (${property.self_arg_name})));

% elif not property.abstract and not property.external:
${gdb_property_start(property)}
pragma Warnings (Off, "is not referenced");
${"overriding" if property.overriding else ""} function ${property.name}
  ${helpers.argument_list(property, property.dispatching)}
   return ${property.type.name}
is
   ## We declare a variable Self, that has the named class wide access type
   ## that we can use to dispatch on other properties and all.
   Self : ${Self.type.name} := ${Self.type.name}
     (${property.self_arg_name});
   ${gdb_bind('self', 'Self')}

   % if property._has_self_entity:
   Ent : ${Self.type.entity.name} :=
     ${Self.type.entity.name}'(Info => E_Info, El => Self);
   % endif

   % for arg in property.arguments:
   ${gdb_bind(arg.name.lower, arg.name.camel_with_underscores)}
   % endfor

   Property_Result : ${property.type.name} := ${property.type.nullexpr};

   ## For each scope, there is one of the following subprograms that finalizes
   ## all the ref-counted local variables it contains, excluding variables from
   ## children scopes.
   <% all_scopes = property.vars.all_scopes %>
   % for scope in all_scopes:
      % if scope.has_refcounted_vars():
         procedure ${scope.finalizer_name};
      % endif
   % endfor

   ${property.vars.render()}

   % for scope in all_scopes:
      % if scope.has_refcounted_vars():
         procedure ${scope.finalizer_name} is
         begin
            ## Finalize the local variable for this scope
            % for var in scope.variables:
               % if var.type.is_refcounted:
                  Dec_Ref (${var.name});
               % endif
            % endfor
         end ${scope.finalizer_name};
      % endif
   % endfor

   % if property.memoized:
         <%
            key_length = 1 + len(property.arguments)
            if property.uses_entity_info:
               key_length += 1
         %>
         use Memoization_Maps;
         Mmz_Map : Map renames Node.Unit.Memoization_Map;
         Mmz_Cur : Cursor;
         Mmz_K   : Mmz_Key;
         Mmz_Val : Mmz_Value;
   % endif

begin
   ${gdb_property_body_start()}

   % if ctx.properties_logging:
      Properties_Traces.Trace
        ("${property.qualname} ("
         & Trace_Image (Self)
         % for arg in property.arguments:
            & ", " & Trace_Image (${arg.name})
         % endfor
         & "):");
      Properties_Traces.Increase_Indent;
   % endif

   ## If this property uses env, we want to make sure lexical env caches are up
   ## to date.
   % if property.uses_envs:
      if Node /= null then
         Reset_Caches (Node.Unit);

         ## And if it is also public, we need to ensure that lexical
         ## environments are populated before executing the property itself.
         % if property.is_public:
            Populate_Lexical_Env (Node.Unit);
         % endif
      end if;
   % endif

   % if property.memoized:
      ## If memoization is enabled for this property, look for an already
      ## computed result for this property. See the declaration of
      ## Analysis_Context_Type.In_Populate_Lexical_Env for the rationale about
      ## the test that follows.

      % if not property.memoize_in_populate:
      if not Node.Unit.Context.In_Populate_Lexical_Env then
      % endif

         Mmz_K :=
           (Property => ${property.memoization_enum},
            Items    => new Mmz_Key_Array (1 ..  ${key_length}));
         Mmz_K.Items (1) := (Kind => ${property.struct.memoization_kind},
                             As_${property.struct.name} => Self);
         % for i, arg in enumerate(property.arguments, 2):
            Mmz_K.Items (${i}) := (Kind => ${arg.type.memoization_kind},
                                   As_${arg.type.name} => ${arg.name});
            % if arg.type.is_refcounted:
               Inc_Ref (Mmz_K.Items (${i}).As_${arg.type.name});
            % endif
         % endfor
         % if property.uses_entity_info:
            Mmz_K.Items (${key_length}) :=
              (Kind => ${T.entity_info.memoization_kind},
               As_${T.entity_info.name} => ${property.entity_info_name});
         % endif

         if not Lookup_Memoization_Map (Node.Unit, Mmz_K, Mmz_Cur) then
            ${gdb_memoization_lookup()}
            Mmz_Val := Memoization_Maps.Element (Mmz_Cur);

            if Mmz_Val.Kind = Mmz_Evaluating then
               % if ctx.properties_logging:
                  Properties_Traces.Trace
                    ("Result: infinite recursion");
               % endif
               ${gdb_memoization_return()}
               raise Property_Error with "Infinite recursion detected";

            elsif Mmz_Val.Kind = Mmz_Property_Error then
               % if ctx.properties_logging:
                  Properties_Traces.Trace
                    ("Result: Property_Error");
                  Properties_Traces.Decrease_Indent;
               % endif
               ${gdb_memoization_return()}
               raise Property_Error;

            else
               Property_Result := Mmz_Val.As_${property.type.name};
               % if property.type.is_refcounted:
                  Inc_Ref (Property_Result);
               % endif

               % if ctx.properties_logging:
                  Properties_Traces.Trace
                    ("Result: " & Trace_Image (Property_Result));
                  Properties_Traces.Decrease_Indent;
               % endif
               ${gdb_memoization_return()}
               return Property_Result;
            end if;
            ${gdb_end()}
         end if;

      % if not property.memoize_in_populate:
      end if;
      % endif
   % endif

   ${scopes.start_scope(property.vars.root_scope)}
   ${property.constructed_expr.render_pre()}

   Property_Result := ${property.constructed_expr.render_expr()};
   % if property.type.is_refcounted:
      Inc_Ref (Property_Result);
   % endif
   ${scopes.finalize_scope(property.vars.root_scope)}

   % if property.memoized:
      ## If memoization is enabled for this property, save the result for later
      ## re-use.
      % if not property.memoize_in_populate:
      if not Node.Unit.Context.In_Populate_Lexical_Env then
      % endif

         Mmz_Val := (Kind => ${property.type.memoization_kind},
                       As_${property.type.name} => Property_Result);
         Mmz_Map.Replace_Element (Mmz_Cur, Mmz_Val);
         % if property.type.is_refcounted:
            Inc_Ref (Property_Result);
         % endif
      % if not property.memoize_in_populate:
      end if;
      % endif
   % endif

   % if ctx.properties_logging:
      Properties_Traces.Trace ("Result: " & Trace_Image (Property_Result));
      Properties_Traces.Decrease_Indent;
   % endif

   return Property_Result;

% if property.vars.root_scope.has_refcounted_vars(True) or \
     ctx.properties_logging:
   exception
      when Property_Error =>
         % for scope in all_scopes:
            % if scope.has_refcounted_vars():
               ${scope.finalizer_name};
            % endif
         % endfor

         % if property.memoized:
            % if not property.memoize_in_populate:
            if not Node.Unit.Context.In_Populate_Lexical_Env then
            % endif

               Mmz_Map.Replace_Element (Mmz_Cur, Mmz_Val);

            % if not property.memoize_in_populate:
            end if;
            % endif
         % endif

         % if ctx.properties_logging:
            Properties_Traces.Trace ("Result: Properties_Error");
            Properties_Traces.Decrease_Indent;
         % endif

         raise;
% endif
end ${property.name};
${gdb_end()}
% endif
