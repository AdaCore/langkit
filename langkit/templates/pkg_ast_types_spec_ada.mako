## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = T.root_node.array_type() %>
<% no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts) %>

pragma Warnings (Off, "referenced");
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Vectors;
pragma Warnings (On, "referenced");

private with ${_self.ada_api_settings.lib_name}.AST.List;

--  This package defines all AST node types except the root one (see the
--  AST_Root package) along with the corresponding primitives. It also defines
--  miscellanous types that appear in the AST (stored in nodes or handled in
--  properties).

package ${_self.ada_api_settings.lib_name}.AST.Types is

   -----------------------
   -- Enumeration types --
   -----------------------

   function Image (Value : Boolean) return String is
     (if Value then "True" else "False");

   % for cls in _self.sorted_types(_self.enum_types):
   ${enum_types.public_decl(cls)}
   % endfor

   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.public_incomplete_decl(struct_type)}
   % endfor

   -----------------------------------------------------
   -- ASTNode derived types (incomplete declarations) --
   -----------------------------------------------------

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.public_incomplete_decl(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.public_incomplete_decl(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.public_incomplete_decl(astnode)}
      % endif
   % endfor

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.public_decl(struct_type)}
   % endfor

   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

   --  TODO??? This is likely to change in the near future: we would like to
   --  have here pure Ada arrays instead.

   % for array_type in _self.sorted_types(_self.array_types):
   % if array_type.element_type().should_emit_array_type:
   ${array_types.public_decl(array_type)}
   % endif
   % endfor

   -----------------------------------------------
   -- ASTNode derived types (full declarations) --
   -----------------------------------------------

   --  See ${_self.ada_api_settings.lib_name}.AST for overriden primitive
   --  operations documentations.

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.public_decl(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.public_decl(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.public_decl(astnode)}
      % endif
   % endfor

private

   % for array_type in _self.sorted_types(_self.array_types):
   % if array_type.element_type().should_emit_array_type:
   ${array_types.private_decl(array_type)}
   % endif
   % endfor

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.private_decl(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.private_decl(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.private_decl(astnode)}
      % endif
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.Types;
