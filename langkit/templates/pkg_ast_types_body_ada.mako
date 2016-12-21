## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = T.root_node.array_type() %>
<% no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts) %>

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off, "referenced");
with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Debug;               use Adalog.Debug;
with Adalog.Operations;          use Adalog.Operations;
with Adalog.Predicates;          use Adalog.Predicates;
with Adalog.Pure_Relations;      use Adalog.Pure_Relations;
with Adalog.Variadic_Operations; use Adalog.Variadic_Operations;

with Langkit_Support.Extensions; use Langkit_Support.Extensions;
with Langkit_Support.Relative_Get;
with Langkit_Support.Slocs;      use Langkit_Support.Slocs;
with Langkit_Support.Symbols;    use Langkit_Support.Symbols;

with ${_self.ada_api_settings.lib_name}.Analysis.Internal;
with ${_self.ada_api_settings.lib_name}.Analysis;
pragma Warnings (On, "referenced");

%if _self.env_hook_subprogram:
with ${_self.env_hook_subprogram.unit_fqn};
%endif

package body ${_self.ada_api_settings.lib_name}.AST.Types is

   use Eq_Node, Eq_Node.Raw_Impl;
   ##  Make logic operations on nodes accessible

   procedure Register_Destroyable is new
      Analysis_Interfaces.Register_Destroyable
        (AST_Envs.Lexical_Env_Type, AST_Envs.Lexical_Env, AST_Envs.Destroy);

   pragma Warnings (Off, "referenced");
   procedure Register_Destroyable
     (Unit : access Analysis_Unit_Interface_Type'Class;
      Node : ${root_node_type_name});
   --  Helper for synthetized nodes. We cannot used the generic
   --  Register_Destroyable because the root AST node is an abstract types, so
   --  this is implemented using the untyped (using System.Address)
   --  implementation helper.
   pragma Warnings (Off, "referenced");

   procedure Destroy_Synthetic_Node (Node : in out ${root_node_type_name});
   --  Helper for the Register_Destroyable above

   function Get_Lex_Env_Data
     (Node : access ${root_node_value_type}'Class) return Lex_Env_Data
   is (${_self.ada_api_settings.lib_name}.Analysis.Get_Lex_Env_Data
        (Analysis.Internal.Convert (Node.Unit)));

   -----------
   -- Image --
   -----------

   overriding function Image
     (Node : access ${generic_list_value_type}) return String
   is
      Result : Unbounded_String;
   begin
      Append (Result, '[');
      for El of Node.Vec loop
         if Length (Result) > 0 then
            Append (Result, ", ");
         end if;
         Append (Result, El.Image);
      end loop;

      Append (Result, ']');
      return To_String (Result);
   end Image;

   -----------------
   -- Child_Count --
   -----------------

   overriding function Child_Count
     (Node : access ${generic_list_value_type}) return Natural
   is
   begin
      return Node_Bump_Ptr_Vectors.Length (Node.Vec);
   end Child_Count;

   ---------------
   -- Get_Child --
   ---------------

   overriding procedure Get_Child
     (Node            : access ${generic_list_value_type};
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name}) is
   begin
      if Index > Node_Bump_Ptr_Vectors.Last_Index (Node.Vec) then
         Index_In_Bounds := False;
      else
         Index_In_Bounds := True;
         Result := ${root_node_type_name}
           (Node_Bump_Ptr_Vectors.Get_At_Index (Node.Vec, Index));
      end if;
   end Get_Child;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (Node : access ${generic_list_value_type}; Prefix : String := "")
   is
      Class_Wide_Node : constant ${root_node_type_name} :=
         ${root_node_type_name} (Node);
   begin
      Put
        (Prefix & Class_Wide_Node.Kind_Name
         & "[" & Image (Node.Sloc_Range) & "]");
      if Node_Bump_Ptr_Vectors.Length (Node.Vec) = 0 then
         Put_Line (": <empty list>");
         return;
      end if;

      New_Line;
      for Child of Node.Vec loop
         if Child /= null then
            Child.Print (Prefix & "|  ");
         end if;
      end loop;
   end Print;

   ------------------
   -- Destroy_Node --
   ------------------

   overriding procedure Destroy_Node
     (Node : access ${generic_list_value_type})
   is
   begin
      if Langkit_Support.Extensions.Has_Extensions then
         Node.Free_Extensions;
      end if;
   end Destroy_Node;

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.body(struct_type)}
   % endfor

   % for array_type in _self.sorted_types(_self.array_types):
   % if array_type.element_type().should_emit_array_type:
   ${array_types.body(array_type)}
   % endif
   % endfor

   ${astnode_types.logic_helpers()}

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.body(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.body(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.body(astnode)}
      % endif
   % endfor

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : access Analysis_Unit_Interface_Type'Class;
      Node : ${root_node_type_name})
   is
      procedure Helper is new
         Analysis_Interfaces.Register_Destroyable
           (${root_node_value_type}'Class,
            ${root_node_type_name},
            Destroy_Synthetic_Node);
   begin
      Helper (Unit, Node);
   end Register_Destroyable;

   ----------------------------
   -- Destroy_Synthetic_Node --
   ----------------------------

   procedure Destroy_Synthetic_Node (Node : in out ${root_node_type_name}) is
      procedure Free is new Ada.Unchecked_Deallocation
        (${root_node_value_type}'Class, ${root_node_type_name});
   begin
      Node.Destroy_Node;
      Free (Node);
   end Destroy_Synthetic_Node;

end ${_self.ada_api_settings.lib_name}.AST.Types;
