## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} : ${Self.type.name}
   % for arg in property.arguments:
      ; ${arg.name} : ${arg.type.name}
      % if arg.default_value is not None:
         := ${arg.default_value.render_private_ada_constant()}
      % endif
   % endfor
   % if property.uses_entity_info:
   ; ${property.entity_info_name} : ${T.entity_info.name} :=
      ${T.entity_info.nullexpr}
   % endif
  )
</%def>

<%def name="dynamic_vars_holder_decl(type_name, base_type_name, dynvars)">
   type ${type_name} is new ${base_type_name} with
   % if dynvars:
      record
         % for dynvar in dynvars:
            ${dynvar.argument_name} : ${dynvar.type.name};
         % endfor
      end record;
   % else:
      null record;
   % endif
</%def>

<%def name="logic_converter(conv_prop)">
   <%
   type_name = "Logic_Converter_{}".format(conv_prop.uid)
   entity = T.entity.name
   %>

   ${dynamic_vars_holder_decl(
         type_name, "Solver_Ifc.Converter_Type", conv_prop.dynamic_vars
     )}

   overriding function Convert
     (Self : ${type_name}; From : ${entity}) return ${entity}
     with Inline;

   overriding function Image (Self : ${type_name}) return String;

   -------------
   -- Convert --
   -------------

   overriding function Convert
     (Self : ${type_name}; From : ${entity}) return ${entity} 
   is
      % if not conv_prop.dynamic_vars:
         pragma Unreferenced (Self);
      % endif
      Ret : ${conv_prop.type.name};
   begin
      ## Here, we just forward the return value from conv_prop to our caller,
      ## so there is nothing to do regarding ref-counting.
      Ret := ${conv_prop.name}
        (${conv_prop.self_arg_name} => From.Node,
         % for dynvar in conv_prop.dynamic_vars:
            ${dynvar.argument_name} => Self.${dynvar.argument_name},
         % endfor
         ${conv_prop.entity_info_name} => From.Info);

      return (Node => Ret.Node, Info => Ret.Info);
   end Convert;

   -----------
   -- Image --
   -----------

   overriding function Image (Self : ${type_name}) return String is
   begin
      return ("${conv_prop.qualname}");
   end Image;
</%def>

<%def name="logic_equal(eq_prop)">
   <%
      struct = eq_prop.struct
      type_name = 'Comparer_{}'.format(eq_prop.uid)
   %>

   ${dynamic_vars_holder_decl(
         type_name, "Solver_Ifc.Comparer_Type", eq_prop.dynamic_vars
     )}

   overriding function Image (Self : ${type_name}) return String;
   overriding function Compare
     (Self : ${type_name}; L, R : ${T.entity.name}) return Boolean;

   overriding function Image (Self : ${type_name}) return String
   is
   begin
      return ("${eq_prop.qualname}");
   end Image;

   overriding function Compare
     (Self : ${type_name}; L, R : ${T.entity.name}) return Boolean is
     % if not eq_prop.dynamic_vars:
        pragma Unreferenced (Self);
     % endif
   begin
      --  If any node pointer is null, then use that for equality
      if L.Node = null or else R.Node = null then
         return L.Node = R.Node;
      end if;

      --  Check that both arguments have appropriate types for the property
      --  call.
      if L.Node.Kind not in ${struct.ada_kind_range_name} then
         raise Property_Error with
            "Wrong type for ${eq_prop.qualname}'s ""self"" argument";
      elsif R.Node.Kind not in ${struct.ada_kind_range_name} then
         raise Property_Error with
            "Wrong type for ${eq_prop.qualname}'s"
            & " ""${eq_prop.natural_arguments[0].dsl_name}"" argument";
      end if;

      --  All is good: do the call
      declare
         R_Entity : constant ${struct.entity.name} := (R.Node, R.Info);
      begin
         return ${eq_prop.name}
          (${eq_prop.self_arg_name}             => L.Node,
           ${eq_prop.natural_arguments[0].name} => R_Entity,
           % for dynvar in eq_prop.dynamic_vars:
              ${dynvar.argument_name} => Self.${dynvar.argument_name},
           % endfor
           ${eq_prop.entity_info_name}          => L.Info);
       end;
   end Compare;

</%def>

<%def name="logic_predicates(prop)">
   % for (args_types, default_passed_args, pred_id) in prop.logic_predicates:

   <%
      type_name = "{}_Predicate".format(pred_id)
      package_name = "{}_Pred".format(pred_id)
      formal_node_types = prop.get_concrete_node_types(args_types,
                                                       default_passed_args)
      arity = len(formal_node_types)
      refcounted_args_types = filter(lambda t: t.is_refcounted, args_types)
      args = list(enumerate(args_types))
   %>

   <%def name="call_profile()">
      overriding function Call
        (Self       : ${type_name};
        % if arity == 1:
        Entity  : Solver_Ifc.Value_Type
        % else:
        Entities : Solver_Ifc.Value_Array
        % endif
        ) return Boolean
   </%def>

   type ${type_name} is
   new Solver_Ifc.${"Predicate_Type" if arity == 1 else "N_Predicate_Type"}
   with record
      % for i, arg_type in args:
         Field_${i} : ${arg_type.name};
      % endfor
      % if not args:
      null;
      % endif
   end record;

   ${call_profile()};
   overriding function Image (Self : ${type_name}) return String;
   % if refcounted_args_types:
   overriding procedure Destroy (Self : in out ${type_name});
   % endif

   function Create_${pred_id}_Predicate
   % if args:
   (
      % for i, arg_type in args:
         Field_${i} : ${arg_type.name}${"" if loop.last else ";"}
      % endfor
   )
   % endif
   return ${type_name} is
   begin
      % for i, arg_type in args:
         % if arg_type.is_refcounted:
            Inc_Ref (Field_${i});
         % endif
      % endfor
      return ${type_name}'(
         % if args:
         % for i, arg_type in args:
            Field_${i} => Field_${i}${"" if loop.last else ","}
         % endfor
         % else:
         null record
         % endif
      );
   end;

   ----------
   -- Call --
   ----------

   ${call_profile()}
   is
      % if not args_types:
         pragma Unreferenced (Self);
      % endif

      % if arity > 1:
      Entity : Solver_Ifc.Value_Type := Entities (1);
      % endif
      <% node0_type = formal_node_types[0] %>
      Node : constant ${node0_type.name} := Entity.Node;
   begin
      ## Here, we'll raise a property error, but only for dispatching
      ## properties. For non dispatching properties we'll allow the user to
      ## handle null however he wants.
      % if prop.dispatching and not ctx.no_property_checks:
         if Node_0.Node = null then
            raise Property_Error
              with "In predicate, calling dispatching property on a null node";
        end if;
      % endif

      <%
         args = ['Node'] + [
            '(Node => {}, Info => Entities ({}).Info)'.format(
                'Entities ({}).Node'.format(i + 1),
                i + 1
            ) for i, formal_type in enumerate(formal_node_types[1:], 1)
      ] + [
            'Self.Field_{}'.format(i)
            for i, _ in enumerate(args_types)
         ]
         if prop.uses_entity_info:
            args.append('{} => Entity.Info'.format(prop.entity_info_name))
         args_fmt = '({})'.format(', '.join(args)) if args else ''
      %>
      return ${prop.name} ${args_fmt};
   end Call;

   -----------
   -- Image --
   -----------

   overriding function Image (Self : ${type_name}) return String is
   begin
      return "${prop.qualname}";
   end Image;

   ----------
   -- Free --
   ----------

   % if refcounted_args_types:
   overriding procedure Destroy (Self : in out ${type_name}) is
   begin
      % for i, arg_type in enumerate(refcounted_args_types):
      Dec_Ref (Self.Field_${i});
      % endfor
   end Free;
   % endif

   % endfor
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted:
      Inc_Ref (${var.name});
   % endif
</%def>
