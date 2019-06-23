## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} :
   % if Self.type.is_ast_node:
      access ${Self.type.value_type_name()}${"" if dispatching else "'Class"}
   % else:
      ${Self.type.name}
   % endif

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
   root_class = T.root_node.name
   entity = T.entity.name
   %>

   ${dynamic_vars_holder_decl(type_name, "Solver.Converter_Type", conv_prop.dynamic_vars)}

   overriding function Convert (Self : ${type_name}; From : ${entity}) return ${entity}
      with Inline;

   -------------
   -- Convert --
   -------------

   overriding function Convert (Self : ${type_name}; From : ${entity}) return ${entity} is
      % if not conv_prop.dynamic_vars:
         pragma Unreferenced (Self);
      % endif
      Ret : ${conv_prop.type.name};
   begin
      ## Here, we just forward the return value from conv_prop to our caller,
      ## so there is nothing to do regarding ref-counting.
      Ret := ${conv_prop.name}
        (${conv_prop.self_arg_name}    => ${conv_prop.struct.name} (From.Node),
         % for dynvar in conv_prop.dynamic_vars:
            ${dynvar.argument_name}    => Self.${dynvar.argument_name},
         % endfor
         ${conv_prop.entity_info_name} => From.Info);
      return (Node => ${root_class} (Ret.Node), Info => Ret.Info);
   end Convert;
</%def>

<%def name="logic_equal(eq_prop)">
   <%
      struct = eq_prop.struct.name
      struct_entity = eq_prop.struct.entity.name
      type_name = 'Comparer_{}'.format(eq_prop.uid)
   %>

   ${dynamic_vars_holder_decl(type_name, "Solver.Comparer_Type", eq_prop.dynamic_vars)}
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
      if L.Node.all not in ${struct}_Type'Class then
         raise Property_Error with
            "Wrong type for ${eq_prop.qualname}'s ""self"" argument";
      elsif R.Node.all not in ${struct}_Type'Class then
         raise Property_Error with
            "Wrong type for ${eq_prop.qualname}'s"
            & " ""${eq_prop.natural_arguments[0].dsl_name}"" argument";
      end if;

      --  All is good: do the call
      declare
         R_Entity : constant ${struct_entity} :=
           (${struct} (R.Node), R.Info);
      begin
         return ${eq_prop.name}
          (${eq_prop.self_arg_name}             => ${struct} (L.Node),
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
      type_name = "{}_Predicate_Caller".format(pred_id)
      package_name = "{}_Pred".format(pred_id)
      root_class = T.root_node.name
      formal_node_types = prop.get_concrete_node_types(args_types,
                                                       default_passed_args)
   %>

   type ${type_name} is record
      % for i, arg_type in enumerate(args_types):
         Field_${i} : ${arg_type.name};
      % endfor
      Dbg_Img : String_Access := null;
   end record;

   function Create_${pred_id}_Predicate (
      % for i, arg_type in enumerate(args_types):
         Field_${i} : ${arg_type.name};
      % endfor
      Dbg_Img : String_Access := null
   ) return ${type_name} is
   begin
      % for i, arg_type in enumerate(args_types):
         % if arg_type.is_refcounted:
            Inc_Ref (Field_${i});
         % endif
      % endfor
      return ${type_name}'(
         % for i, arg_type in enumerate(args_types):
            Field_${i} => Field_${i},
         % endfor
         Dbg_Img => Dbg_Img
      );
   end;

   ----------
   -- Call --
   ----------

   function Call
     (Self       : ${type_name}
     % for i in range(len(formal_node_types)):
     ; Node_${i} : ${T.entity.name}
     % endfor
     ) return Boolean
   is
      Node : constant ${formal_node_types[0].name} :=
         ${formal_node_types[0].name} (Node_0.Node);
   begin
      ## Here, we'll raise a property error, but only for dispatching
      ## properties. For non dispatching properties we'll allow the user to
      ## handle null however he wants.
      % if prop.dispatching and not ctx.no_property_checks:
         if Node = null then
            raise Property_Error
              with "In predicate, calling dispatching property on a null node";
        end if;
      % endif

      <%
         args = ['Node'] + [
            '(Node => {} (Node_{}.Node), Info => Node_{}.Info)'.format(
                formal_type.element_type.name, i, i
            ) for i, formal_type in enumerate(formal_node_types[1:], 1)
         ] + [
            'Self.Field_{}'.format(i)
            for i, _ in enumerate(args_types)
         ]
         if prop.uses_entity_info:
            args.append('{} => Node_0.Info'.format(prop.entity_info_name))
         args_fmt = '({})'.format(', '.join(args)) if args else ''
      %>
      return ${prop.name} ${args_fmt};
   end Call;

   -----------
   -- Image --
   -----------

   function Image (Self : ${type_name}) return String
   is (if Self.Dbg_Img /= null then Self.Dbg_Img.all else "");

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out ${type_name}) is
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
   begin
      % for i, arg_type in enumerate(args_types):
         % if arg_type.is_refcounted:
            Dec_Ref (Self.Field_${i});
         % endif
      % endfor
      Free (Self.Dbg_Img);
   end Free;

   package ${package_name} is new Predicate_${len(formal_node_types)}
     (El_Type        => ${T.entity.name},
      Var            => Eq_Node.Refs.Raw_Logic_Var,
      Predicate_Type => ${type_name},
      Free           => Free,
      Image          => Image);

   % endfor
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted:
      Inc_Ref (${var.name});
   % endif
</%def>
