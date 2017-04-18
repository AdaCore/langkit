## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} :
   access ${Self.type.value_type_name()}${"" if dispatching else "'Class"}

   % for arg in property.arguments:
      ; ${arg.name} : ${arg.type.name()}
      % if not arg.is_explicit:
         := ${arg.type.nullexpr()}
      % endif
   % endfor
  )
</%def>

<%def name="generate_logic_converter(conv_prop)">
   <%
   type_name = "Logic_Converter_{}".format(conv_prop.uid)
   root_class = T.root_node.name()
   entity = T.entity.name()
   %>

   ## We generate a custom type which is a functor in the C++ term, eg just a
   ## function with state. The state it needs to keep is the lexical env at the
   ## site where the logic binder is generated.
   type ${type_name} is null record;

   No_${type_name} : constant ${type_name} := (null record);

   function Convert (Self : ${type_name}; From : ${entity}) return ${entity}
      with Inline;

   -------------
   -- Convert --
   -------------

   function Convert (Self : ${type_name}; From : ${entity}) return ${entity} is
      pragma Unreferenced (Self);
      Ret : ${conv_prop.type.name()};
   begin
      ## Here, we just forward the return value from conv_prop to our caller,
      ## so there is nothing to do regarding ref-counting.
      Ret := ${conv_prop.name}
        (${conv_prop.struct.name()} (From.El), From.Info);
      return (El => ${root_class} (Ret.El), Info => Ret.Info);
   end Convert;
</%def>

<%def name="generate_logic_equal(eq_prop)">
   <% struct = eq_prop.struct.name() %>

   function Eq_${eq_prop.uid} (L, R : ${T.entity.name()}) return Boolean is
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
         return ${eq_prop.name}
          (${struct} (L.El),
           (El => ${struct} (R.El), Info => R.Info),
           ${eq_prop.entity_info_name} => L.Info);
      end if;

      --  Else raise an error
      raise Constraint_Error with "Wrong type for Eq_${eq_prop.uid} arguments";
   end Eq_${eq_prop.uid};

</%def>

<%def name="generate_logic_binder(conv_prop, eq_prop)">
   <%
   cprop_uid = conv_prop.uid if conv_prop else "Default"
   eprop_uid = eq_prop.uid if eq_prop else "Default"
   package_name = "Bind_{}_{}".format(cprop_uid, eprop_uid)
   converter_type_name = "Logic_Converter_{}".format(cprop_uid)
   %>
   ## This package contains the necessary Adalog instantiations, so that we can
   ## create an equation that will bind two logic variables A and B so that::
   ##    B = PropertyCall (A.Value)
   ##
   ## Which is expressed as Bind (A, B, Property) in the DSL.
   package ${package_name} is new Eq_Node.Raw_Custom_Bind
     (Converter => ${converter_type_name},
      No_Data   => No_${converter_type_name},
      Convert   => Convert,
      Equals    => Eq_${eprop_uid});
</%def>

<%def name="generate_logic_predicates(prop)">
   % for (args_types, pred_id) in prop.logic_predicates:

   <%
      type_name = "{}_Predicate_Caller".format(pred_id)
      package_name = "{}_Pred".format(pred_id)
      root_class = T.root_node.name()
      formal_node_types = prop.get_concrete_node_types(args_types)
   %>

   type ${type_name} is record
      % for i, arg_type in enumerate(args_types):
      Field_${i} : ${arg_type.name()};
      % endfor
      Dbg_Img    : String_Access := null;
   end record;

   ------------
   -- Create --
   ------------

   function Create (
      % for i, arg_type in enumerate(args_types):
         Field_${i} : ${arg_type.name()};
      % endfor
      Dbg_Img : String_Access := null
   ) return ${type_name} is
   begin
      % for i, arg_type in enumerate(args_types):
         % if arg_type.is_refcounted():
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
     ; Node_${i} : ${T.entity.name()}
     % endfor
     ) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      <%
         args = [
            '{} (Node_{}.El)'.format(formal_type.name(), i)
            for i, formal_type in enumerate(formal_node_types)
         ] + [
            'Self.Field_{}'.format(i)
            for i, _ in enumerate(args_types)
         ]
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
         % if arg_type.is_refcounted():
            Dec_Ref (Self.Field_${i});
         % endif
      % endfor
      Free (Self.Dbg_Img);
   end Free;

   package ${package_name} is new Predicate_${len(formal_node_types)}
     (El_Type        => ${T.entity.name()},
      Var            => Eq_Node.Refs.Raw_Logic_Var,
      Predicate_Type => ${type_name},
      Free           => Free,
      Image          => Image);

   % endfor
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted():
      Inc_Ref (${var.name});
   % endif
</%def>
