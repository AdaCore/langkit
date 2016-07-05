## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = ctx.root_grammar_class.array_type() %>
<% no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts) %>

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off, "referenced");
with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Operations;          use Adalog.Operations;
with Adalog.Predicates;          use Adalog.Predicates;
with Adalog.Pure_Relations;      use Adalog.Pure_Relations;
with Adalog.Variadic_Operations; use Adalog.Variadic_Operations;

with Langkit_Support.Extensions; use Langkit_Support.Extensions;
with Langkit_Support.PP_Utils;   use Langkit_Support.PP_Utils;
with Langkit_Support.Relative_Get;
with Langkit_Support.Slocs;      use Langkit_Support.Slocs;
with Langkit_Support.Symbols;    use Langkit_Support.Symbols;

with ${_self.ada_api_settings.lib_name}.Analysis.Internal;
pragma Warnings (On, "referenced");

%if _self.env_hook_subprogram:
with ${_self.env_hook_subprogram[0]};
%endif

package body ${_self.ada_api_settings.lib_name}.AST.Types is

   use Eq_Node, Eq_Node.Raw_Impl;
   ##  Make logic operations on nodes accessible

   procedure Register_Destroyable is new
      Analysis_Interfaces.Register_Destroyable
        (AST_Envs.Lexical_Env_Type, AST_Envs.Lexical_Env, AST_Envs.Destroy);

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.body(struct_type)}
   % endfor

   % for array_type in _self.sorted_types(_self.array_types):
   % if array_type.element_type().should_emit_array_type:
   ${array_types.body(array_type)}
   % endif
   % endfor

   % for astnode in no_builtins(_self.astnode_types):
   ${astnode_types.logic_helpers(astnode)}
   % endfor

   % for astnode in no_builtins(_self.astnode_types):
   ${astnode_types.body(astnode)}
   % endfor

   % for element_type in _self.sorted_types(_self.list_types):
   ${list_types.body(element_type)}
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.Types;
