## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

## Regular property function

${"overriding" if property.is_overriding else ""}
function ${property.names.codegen}
   ${helpers.argument_list(property, property.dispatching)}
   return ${property.type.name}
   % if property.abstract:
   is abstract
   % endif

   <%
      aspects = []
      if property.is_dispatcher:
         aspects.append("Inline_Always")

      if (
         property.owner == T.root_node
         and property.names.spec.lower == "can_reach"
      ):
         aspects += [
            "Export",
            f"External_Name => {ascii_repr(ctx.can_reach_property_symbol)}",
         ]
   %>
   % if aspects:
      with
      % for a in aspects:
        ${a}${"" if loop.last else ","}
      % endfor
   % endif
   ;
${ada_doc(property, 0)}
