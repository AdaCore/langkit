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

<%def name="dynamic_vars_holder_decl(type_name, dynvars)">
   type ${type_name} is
   % if dynvars:
      record
         % for dynvar in dynvars:
            ${dynvar.argument_name} : ${dynvar.type.name};
         % endfor
      end record;
   % else:
      null record;
   % endif

   No_${type_name} : constant ${type_name} := (
      % if dynvars:
         <%
            items = [
               '{} => {}'.format(dynvar.argument_name, dynvar.type.nullexpr)
               for dynvar in dynvars
            ]
         %>
         ${', '.join(items)}
      % else:
         null record
      % endif
   );
</%def>

<%def name="logic_converter(conv_prop)">
   <%
   type_name = "Logic_Converter_{}".format(conv_prop.uid)
   entity = T.entity.name
   %>

   ${dynamic_vars_holder_decl(type_name, conv_prop.dynamic_vars)}

   function Convert (Self : ${type_name}; From : ${entity}) return ${entity}
      with Inline;

   -------------
   -- Convert --
   -------------

   function Convert (Self : ${type_name}; From : ${entity}) return ${entity} is
      % if not conv_prop.dynamic_vars:
         pragma Unreferenced (Self);
      % endif
      Ret : ${conv_prop.type.name};
   begin
      ## From can contain any node type: make sure it has a correct type for
      ## the conversion property before doing the call.
      ##
      ## No need to perform the check if there is only one concrete node in the
      ## whole language, or we get a compilation warning.
      % if len(T.root_node.concrete_subclasses) > 1:
         if From.Node /= null
            and then From.Node.Kind not in
                     ${conv_prop.struct.ada_kind_range_name}
         then
            raise Property_Error with "mismatching node type for conv_prop";
         end if;
      % endif

      ## Here, we just forward the return value from conv_prop to our caller,
      ## so there is nothing to do regarding ref-counting.
      <%
         args = [f"{conv_prop.self_arg_name} => From.Node"]
         for dynvar in conv_prop.dynamic_vars:
            args.append(
               f"{dynvar.argument_name} => Self.{dynvar.argument_name}"
            )
         if conv_prop.uses_entity_info:
            args.append(f"{conv_prop.entity_info_name} => From.Info")
      %>
      Ret := ${conv_prop.name} (${", ".join(args)});

      return (Node => Ret.Node, Info => Ret.Info);
   end Convert;
</%def>

<%def name="logic_equal(eq_prop)">
   <%
      struct = eq_prop.struct
      type_name = 'Equals_Data_{}'.format(eq_prop.uid)
   %>

   ${dynamic_vars_holder_decl(type_name, eq_prop.dynamic_vars)}

   function Eq_${eq_prop.uid}
     (Data : ${type_name}; L, R : ${T.entity.name}) return Boolean is
     % if not eq_prop.dynamic_vars:
        pragma Unreferenced (Data);
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
              ${dynvar.argument_name} => Data.${dynvar.argument_name},
           % endfor
           ${eq_prop.entity_info_name}          => L.Info);
       end;
   end Eq_${eq_prop.uid};

</%def>

<%def name="logic_binder(conv_prop, eq_prop)">
   <%
   cprop_uid = conv_prop.uid if conv_prop else "Default"
   eprop_uid = eq_prop.uid if eq_prop else "Default"
   package_name = "Bind_{}_{}".format(cprop_uid, eprop_uid)
   converter_type_name = "Logic_Converter_{}".format(cprop_uid)
   equals_type_name = 'Equals_Data_{}'.format(eprop_uid)
   %>
   ## This package contains the necessary Adalog instantiations, so that we can
   ## create an equation that will bind two logic variables A and B so that::
   ##    B = PropertyCall (A.Value)
   ##
   ## Which is expressed as Bind (A, B, Property) in the DSL.
   package ${package_name} is new Eq_Node.Raw_Custom_Bind
     (Converter        => ${converter_type_name},
      No_Data          => No_${converter_type_name},
      Equals_Data      => ${equals_type_name},
      No_Equals_Data   => No_${equals_type_name},
      Convert          => Convert,
      Equals           => Eq_${eprop_uid},
      Convert_Image    =>
        ${ascii_repr(conv_prop.qualname if conv_prop else '')},
      Equals_Image     => ${ascii_repr(eq_prop.qualname if eq_prop else '')},

      ## We don't support passing converters that works both ways (from left to
      ## right and from right to left value) because it is confusing, so when a
      ## converter is passed, we forbid two sided conversions.
      One_Side_Convert => ${"True" if conv_prop else "False"}
      );
</%def>

<%def name="logic_predicates(prop)">
   % for (args_types, default_passed_args, pred_id) in prop.logic_predicates:

   <%
      type_name = "{}_Predicate_Caller".format(pred_id)
      package_name = "{}_Pred".format(pred_id)
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
      % if not args_types:
         pragma Unreferenced (Self);
      % endif
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
         args = ['Node_0.Node'] + [
            '(Node => Node_{i}.Node, Info => Node_{i}.Info)'.format(i=i)
            for i, formal_type in enumerate(formal_node_types[1:], 1)
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
