## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} :
   access ${Self.type.value_type_name()}${"" if dispatching else "'Class"}

   % for arg in property.arguments:
      ; ${arg.name} : ${arg.type.name}
      % if arg.is_optional:
         := ${arg.type.nullexpr}
      % endif
   % endfor
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
   root_class = T.root_node.name
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
      ## Here, we just forward the return value from conv_prop to our caller,
      ## so there is nothing to do regarding ref-counting.
      Ret := ${conv_prop.name}
        (${conv_prop.self_arg_name}    => ${conv_prop.struct.name} (From.El),
         % for dynvar in conv_prop.dynamic_vars:
            ${dynvar.argument_name}    => Self.${dynvar.argument_name},
         % endfor
         ${conv_prop.entity_info_name} => From.Info);
      return (El => ${root_class} (Ret.El), Info => Ret.Info);
   end Convert;
</%def>

<%def name="logic_equal(eq_prop)">
   <%
      struct = eq_prop.struct.name
      struct_entity = eq_prop.struct.entity.name
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
      if L.El = null or else R.El = null then
         return L.El = R.El;
      end if;

      --  If both args are of the proper AST node type
      if L.El.all in ${struct}_Type'Class
         and then R.El.all in ${struct}_Type'Class
      then
         --  Then call the equality property on it
         declare
            R_Entity : constant ${struct_entity} := (${struct} (R.El), R.Info);
         begin
            return ${eq_prop.name}
             (${eq_prop.self_arg_name}             => ${struct} (L.El),
              ${eq_prop.natural_arguments[0].name} => R_Entity,
              % for dynvar in eq_prop.dynamic_vars:
                 ${dynvar.argument_name} => Data.${dynvar.argument_name},
              % endfor
              ${eq_prop.entity_info_name}          => L.Info);
          end;
      end if;

      --  Else raise an error
      raise Constraint_Error with "Wrong type for Eq_${eq_prop.uid} arguments";
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
     (Converter      => ${converter_type_name},
      No_Data        => No_${converter_type_name},
      Equals_Data    => ${equals_type_name},
      No_Equals_Data => No_${equals_type_name},
      Convert        => Convert,
      Equals         => Eq_${eprop_uid},
      Convert_Image  => ${string_repr(conv_prop.qualname if conv_prop else '')},
      Equals_Image   => ${string_repr(eq_prop.qualname if eq_prop else '')});
</%def>

<%def name="logic_predicates(prop)">
   % for (args_types, pred_id) in prop.logic_predicates:

   <%
      type_name = "{}_Predicate_Caller".format(pred_id)
      package_name = "{}_Pred".format(pred_id)
      root_class = T.root_node.name
      formal_node_types = prop.get_concrete_node_types(args_types)
   %>

   type ${type_name} is record
      % for i, arg_type in enumerate(args_types):
         Field_${i} : ${arg_type.name};
      % endfor
      Dbg_Img : String_Access := null;
   end record;

   ------------
   -- Create --
   ------------

   function Create (
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
   end Create;

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
      pragma Unreferenced (Self);
   begin
      <%
         args = [
            '{} (Node_{}.El)'.format(formal_type.name, i)
            for i, formal_type in enumerate(formal_node_types)
         ] + [
            'Self.Field_{}'.format(i)
            for i, _ in enumerate(args_types)
         ]
         if prop.uses_entity_info:
            args.append('Node_0.Info')
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
