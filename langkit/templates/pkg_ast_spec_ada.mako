## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = ctx.root_grammar_class.array_type() %>

with Langkit_Support.Bump_Ptr; use Langkit_Support.Bump_Ptr;
with Langkit_Support.Tokens;   use Langkit_Support.Tokens;
with Langkit_Support.Vectors;

private with ${_self.ada_api_settings.lib_name}.AST_List;
with ${_self.ada_api_settings.lib_name}.AST_Root;
use ${_self.ada_api_settings.lib_name}.AST_Root;

--  This package defines all AST node types except the root one (see the
--  AST_Root package) along with the corresponding primitives. It also defines
--  miscellanous types that appear in the AST (stored or handled in
--  properties).

package ${_self.ada_api_settings.lib_name}.AST is

   --  Possible values for ${root_node_kind_name}:

   ## Output constants so that all concrete AST_Node subclasses get their own
   ## AST_Node_Kind. Nothing can be an instance of an abstract subclass, so
   ## these do not need their own kind. Note that we start from 2 because 1 is
   ## reserved for all lists.
   List_Kind : constant ${root_node_kind_name} := 1;
   % for cls in _self.astnode_types:
      % if not cls.abstract:
         ${cls.name()}_Kind : constant ${root_node_kind_name} :=
            ${ctx.node_kind_constants[cls]};
      % endif
   % endfor

   -----------------------
   -- Enumeration types --
   -----------------------

   function Image (Value : Boolean) return String is
     (if Value then "True" else "False");

   % for cls in _self.sorted_types(_self.enum_types):
   ${enum_types.public_decl(cls)}
   % endfor

   ---------------------------
   -- ASTNode derived types --
   ---------------------------

   % for struct_type in _self.struct_types:
   ${struct_types.public_incomplete_decl(struct_type)}
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode != _self.root_grammar_class:
         ${astnode_types.public_incomplete_decl(astnode)}
      % endif
   % endfor

   % for element_type in _self.sorted_types(_self.list_types):
   ${list_types.public_decl(element_type)}
   % endfor

   % for struct_type in _self.struct_types:
   ${struct_types.public_decl(struct_type)}
   % endfor
   % for array_type in _self.sorted_types(_self.array_types):
      % if array_type != root_node_array:
   ${array_types.public_decl(array_type)}
      % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode != _self.root_grammar_class:
         ${astnode_types.public_decl(astnode)}
      % endif
   % endfor

private

   % for struct_type in _self.struct_types:
   ${struct_types.private_decl(struct_type)}
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode != _self.root_grammar_class:
         ${astnode_types.private_decl(astnode)}
      % endif
   % endfor

   % for element_type in _self.sorted_types(_self.list_types):
   ${list_types.private_decl(element_type)}
   % endfor

end ${_self.ada_api_settings.lib_name}.AST;
