## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = ctx.root_grammar_class.array_type() %>
<% no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts) %>

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Extensions; use Langkit_Support.Extensions;
with Langkit_Support.PP_Utils;   use Langkit_Support.PP_Utils;
with Langkit_Support.Symbols;    use Langkit_Support.Symbols;
with Langkit_Support.Tokens;     use Langkit_Support.Tokens;

package body ${_self.ada_api_settings.lib_name}.AST.Types is

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.body(struct_type)}
   % endfor

   % for astnode in no_builtins(_self.astnode_types):
   ${astnode_types.body(astnode)}
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.Types;
