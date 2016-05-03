## vim: filetype=makoada

<%def name="generate_logic_binder(prop)">
   % if prop.needs_logic_binder:

   <%
   type_name = "{}_{}_Logic_Binder".format(prop.ast_node.name(), prop.name)
   package_name = "{}_{}_Bind".format(prop.ast_node.name(), prop.name)
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
        (${prop.name} (${prop.ast_node.name()} (From), Self.Env));
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
