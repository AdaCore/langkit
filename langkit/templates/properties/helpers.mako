## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} :
   access ${Self.type.name()}_Type${"" if dispatching else "'Class"}

   % for arg_name, arg_type, arg_dv in property.arguments:
      ; ${arg_name} : ${arg_type.name()}
      % if arg_dv:
         := ${arg_dv}
      % endif
   % endfor
  )
</%def>

<%def name="generate_logic_binder(prop)">
   % if prop.needs_logic_binder:

   <%
   type_name = "{}_{}_Logic_Binder".format(prop.struct.name(), prop.name)
   package_name = "{}_{}_Bind".format(prop.struct.name(), prop.name)
   root_class = T.root_node.name()
   %>

   ## We generate a custom type which is a functor in the C++ term, eg just a
   ## function with state. The state it needs to keep is the lexical env at the
   ## site where the logic binder is generated.
   type ${type_name} is record
      Env  : Lexical_Env;
   end record;

   function Convert
     (Self : ${type_name}; From : ${root_class}) return ${root_class} is
   begin
      return ${root_class}
        (${prop.name} (${prop.struct.name()} (From), Self.Env));
   end Convert;

   ## This package contains the necessary Adalog instantiations, so that we can
   ## create an equation that will bind two logic variables A and B so that::
   ##    B = PropertyCall (A.Value)
   ##
   ## Which is expressed as Bind (A, B, Property) in the DSL.
   package ${package_name}
   is new Eq_Node.Raw_Custom_Bind (${type_name}, Convert);

   % endif
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
      Env        : Lexical_Env;
      Dbg_Img    : access String;
   end record;

   function Call
     (Self           : ${type_name}
     % for i, _ in enumerate(formal_node_types):
     ; Node_${i} : ${root_class}
     % endfor
     ) return Boolean is
   begin
      return ${prop.name} (
         % for i, formal_type in enumerate(formal_node_types):
         ${formal_type.name()} (Node_${i}),
         % endfor
         % for i, _ in enumerate(args_types):
         Self.Field_${i},
         % endfor
         Self.Env
      );
   end Call;

   function Image (Self : ${type_name}) return String
   is (if Self.Dbg_Img /= null then Self.Dbg_Img.all else "");

   package ${package_name} is
   new Predicate_${len(formal_node_types)}
     (${root_class}, Eq_Node.Refs.Raw_Logic_Var, ${type_name});

   % endfor
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted():
      Inc_Ref (${var.name});
   % endif
</%def>
