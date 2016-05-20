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
   root_class = ctx.root_grammar_class.name()
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

<%def name="generate_logic_predicate(prop)">
   % if prop.needs_logic_predicate:

   <%
   type_name = "{}_{}_Predicate_Caller".format(prop.struct.name(), prop.name)
   package_name = "{}_{}_Pred".format(prop.struct.name(), prop.name)
   root_class = ctx.root_grammar_class.name()
   %>

   type ${type_name} is record
      Env  : Lexical_Env;
   end record;

   function Call
     (Self : ${type_name}; Node : ${root_class}) return Boolean is
   begin
      return ${prop.name} (${prop.struct.name()} (Node), Self.Env);
   end Call;

   package ${package_name} is
   new Predicate (${root_class}, Eq_Node.Refs.Raw_Logic_Var, ${type_name});

   % endif
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted():
      Inc_Ref (${var.name});
   % endif
</%def>
