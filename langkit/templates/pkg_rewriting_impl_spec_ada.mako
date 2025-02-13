## vim: filetype=makoada

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Interfaces.C;

with System;
with System.Address_To_Access_Conversions;

with Langkit_Support.Bump_Ptr; use Langkit_Support.Bump_Ptr;
with Langkit_Support.Bump_Ptr_Vectors;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation;   use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Implementation.C; use ${ada_lib_name}.Implementation.C;

--  Internal package: low-level primitives to implement syntax-based source
--  rewriting.

private package ${ada_lib_name}.Rewriting_Implementation is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;

   type Rewriting_Handle is access Rewriting_Handle_Type
      with Convention => C;
   --  Internal handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type
      with Convention => C;
   --  Internal handle for the process of rewriting an analysis unit

   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type
      with Convention => C;
   --  Internal handle for the process of rewriting an analysis unit

   pragma No_Strict_Aliasing (Rewriting_Handle);
   pragma No_Strict_Aliasing (Unit_Rewriting_Handle);
   pragma No_Strict_Aliasing (Node_Rewriting_Handle);

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unit_Rewriting_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => ${T.root_node.name},
      Element_Type    => Node_Rewriting_Handle,
      Hash            => Named_Hash,
      Equivalent_Keys => "=");

   package Nodes_Pools is new Langkit_Support.Bump_Ptr_Vectors
     (Node_Rewriting_Handle);

   type Rewriting_Handle_Type is record
      Context : Internal_Context;
      --  Analysis context this rewriting handle relates to

      Units : Unit_Maps.Map;
      --  Keep track of rewriting handles we create for the units that Context
      --  owns.

      Pool      : Bump_Ptr_Pool;
      New_Nodes : Nodes_Pools.Vector;
      --  Keep track of all node rewriting handles that don't map to original
      --  nodes, i.e. all nodes that were created during this rewriting
      --  session.

      Stubs : Nodes_Pools.Vector;
      --  Keep track of all allocated stub rewriting nodes. These are used in
      --  ``Rotate`` as stubs for rotated ones, and are re-used each time
      --  ``Rotate`` is called.
   end record;

   type Unit_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context this relates to

      Unit : Internal_Unit;
      --  Analysis unit this relates to

      Root : Node_Rewriting_Handle;
      --  Handle for the node that will become the root node of this analysis
      --  unit.

      Nodes : Node_Maps.Map;
      --  Keep track of rewriting handles we create for base AST nodes that
      --  Unit owns.
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, Node_Rewriting_Handle);

   type Node_Children_Kind is (
      Unexpanded,
      --  Dummy node rewriting handle: children don't have their own handle yet

      Expanded_Regular,
      --  Expanded node rewriting handle: children have their own handle. Note
      --  that this is for all but token nodes.

      Expanded_List,
      --  Expanded node rewriting handle, specific for list nodes: element
      --  nodes are stored as a doubly linked list.

      Expanded_Token_Node
      --  Expanded node rewriting handle, specific for token nodes: there is no
      --  children, only some associated text.
   );

   type Node_Children (Kind : Node_Children_Kind := Unexpanded) is record
      case Kind is
         when Unexpanded =>
            null;

         when Expanded_Regular =>
            Vector : Node_Vectors.Vector;
            --  Vector of children for all non-null syntax fields

         when Expanded_List =>
            First, Last : Node_Rewriting_Handle;
            --  Doubly linked list of children

            Count : Natural;
            --  Number of children

         when Expanded_Token_Node =>
            Text : Unbounded_Wide_Wide_String;
            --  Text for this token node
      end case;
   end record;
   --  Lazily evaluated vector of children for a Node_Rewriting_Handle.
   --
   --  In order to avoid constructing the whole tree of Node_Rewriting_Handle
   --  for some analysis unit at once, we build them in a lazy fashion.

   Unexpanded_Children : constant Node_Children := (Kind => Unexpanded);

   type Node_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context that owns Node

      Node : ${T.root_node.name};
      --  Bare AST node which this rewriting handle relates to

      Parent : Node_Rewriting_Handle;
      --  Rewriting handle for Node's parent, or No_Node_Rewriting_Handle if
      --  Node is a root node.

      Previous, Next : Node_Rewriting_Handle;
      --  If ``Parent`` is a list node, ``Previous`` is the previous subling
      --  for this node in that list (``No_Node_Rewriting_Handle`` for the
      --  first sibling), and ``Next`` is the next sibling
      --  (``No_Node_Rewriting_Handle`` for the last sibling).
      --
      --  If ``Parent`` is not a list node, both are set to
      --  ``No_Node_Rewriting_Handle``).

      Kind : ${T.node_kind};
      --  Kind for the node this handle represents. When Node is not null (i.e.
      --  when this represents an already existing node, rather than a new
      --  one), this must be equal to Node.Kind.

      Tied : Boolean;
      --  Whether this node is tied to an analysis unit tree. It can be
      --  assigned as a child to another node iff it is not tied.

      Root_Of : Unit_Rewriting_Handle;
      --  If the node this handle represents is the root of a rewritten unit,
      --  this references this unit. No_Unit_Rewriting_Handle in all other
      --  cases.

      Children : Node_Children;
      --  Lazily evaluated vector of children for the rewritten node
   end record;

   type Unit_Rewriting_Handle_Array is
      array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
      array (Positive range <>) of Node_Rewriting_Handle;

   No_Rewriting_Handle      : constant Rewriting_Handle      := null;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle := null;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle := null;

   --------------------------------------------------
   -- Implementation of context rewriting routines --
   --------------------------------------------------

   function Handle (Context : Internal_Context) return Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function C_Context_To_Handle
     (Context : Internal_Context) return Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_context_to_handle')}";

   function Context (Handle : Rewriting_Handle) return Internal_Context;
   --  Implementation for Rewriting.Context

   function C_Handle_To_Context
     (Handle : Rewriting_Handle) return Internal_Context
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_handle_to_context')}";

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
      with Post => Handle (Context) /= No_Rewriting_Handle
                   and then Has_With_Trivia (Context)
                   and then Start_Rewriting'Result = Handle (Context)
                   and then ${ada_lib_name}.Rewriting_Implementation.Context
                             (Start_Rewriting'Result) = Context;

   function C_Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_start_rewriting')}";
   --  Implementation for Rewriting.Start_Rewriting

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle)
      with Post => Handle = No_Rewriting_Handle;
   --  Implementation for Rewriting.Abort_Rewriting

   procedure C_Abort_Rewriting (Handle : Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_abort_rewriting')}";

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Internal_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result
      with Post => (if Apply'Result.Success
                    then Handle = No_Rewriting_Handle
                    else Handle = Handle'Old);
   --  Implementation for Rewriting.Apply

   package C_Diagnostic_Array is new
     System.Address_To_Access_Conversions (${diagnostic_type});

   type C_Apply_Result is record
      Success           : Interfaces.C.int;
      Unit              : Internal_Unit;
      Diagnostics_Count : Interfaces.C.int;
      Diagnostics       : C_Diagnostic_Array.Object_Pointer;
   end record
      with Convention => C;

   procedure C_Apply
     (Handle : Rewriting_Handle;
      Result : access C_Apply_Result)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_apply')}";

   procedure Free_Apply_Result (Result : access C_Apply_Result)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_free_apply_result')}";

   package C_Unit_Array is new
     System.Address_To_Access_Conversions (Unit_Rewriting_Handle);

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Implementation for Rewriting.Unit_Handles

   function C_Unit_Handles
     (Handle : Rewriting_Handle) return C_Unit_Array.Object_Pointer
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_unit_handles')}";

   ---------------------------------------
   -- Implementation for unit rewriting --
   ---------------------------------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function C_Unit_To_Handle
     (Unit : Internal_Unit) return Unit_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_unit_to_handle')}";

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit;
   --  Implementation for Rewriting.Unit

   function C_Handle_To_Unit
     (Handle : Unit_Rewriting_Handle) return Internal_Unit
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_handle_to_unit')}";

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Root

   function C_Root
     (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_unit_root')}";

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Root

   procedure C_Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_unit_set_root')}";

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Implementation for Rewriting.Unparse

   procedure C_Unparse
     (Handle : Unit_Rewriting_Handle; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_unit_unparse')}";

   ---------------------------------------
   -- Implementation for node rewriting --
   ---------------------------------------

   function Handle
     (Node : ${T.root_node.name}) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function C_Node_To_Handle
     (Node : ${node_type}) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_node_to_handle')}";

   function Node
     (Handle : Node_Rewriting_Handle) return ${T.root_node.name};
   --  Implementation for Rewriting.Node

   function C_Handle_To_Node
     (Handle : Node_Rewriting_Handle) return ${node_type}
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_handle_to_node')}";

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Implementation for Rewriting.Context

   function C_Node_To_Context
     (Node : Node_Rewriting_Handle) return Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_node_to_context')}";

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Unparse

   procedure C_Unparse
     (Handle : Node_Rewriting_Handle; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_node_unparse')}";

   function Kind (Handle : Node_Rewriting_Handle) return ${T.node_kind};
   --  Implementation for Rewriting.Kind

   function C_Kind (Handle : Node_Rewriting_Handle) return ${node_kind_type}
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_kind')}";

   function Image (Handle : Node_Rewriting_Handle) return String;
   --  Implementation for Rewriting.Image

   procedure C_Image
     (Handle : Node_Rewriting_Handle; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_node_image')}";

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Implementation for Rewriting.Tied

   function C_Tied (Handle : Node_Rewriting_Handle) return Interfaces.C.int
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_tied')}";

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Parent

   function C_Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_parent')}";

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Implementation for Rewriting.Children_Count

   function C_Children_Count
     (Handle : Node_Rewriting_Handle) return Interfaces.C.int
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_children_count')}";

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Child

   function C_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Interfaces.C.int) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_child')}";

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array;
   --  Implementation for Rewriting.Children

   package C_Node_Array is new
     System.Address_To_Access_Conversions (Node_Rewriting_Handle);

   procedure C_Children
     (Handle   : Node_Rewriting_Handle;
      Children : access C_Node_Array.Object_Pointer;
      Count    : access Interfaces.C.int)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_children')}";

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Child

   procedure C_Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Interfaces.C.int;
      Child  : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_set_child')}";

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Text

   procedure C_Text
     (Handle : Node_Rewriting_Handle; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_text')}";

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Implementation for Rewriting.Set_Text

   procedure C_Set_Text
     (Handle : Node_Rewriting_Handle; Text : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_set_text')}";

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Replace

   procedure C_Replace (Handle, New_Node : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_replace')}";

   procedure Rotate (Handles : Node_Rewriting_Handle_Array);
   --  Implementation for Rewriting.Rotate

   procedure C_Rotate
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_rotate')}";

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean;
   --  Implementation for Rewriting.Is_List_Node

   --------------------------------------------
   -- Implementation for list node rewriting --
   --------------------------------------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.First_Child

   function C_First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_first_child')}";

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Last_Child

   function C_Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_last_child')}";

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Next_Child

   function C_Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_next_child')}";

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Previous_Child

   function C_Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_previous_child')}";

   procedure Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_Before

   procedure C_Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_insert_before')}";

   procedure Insert_After (Handle, New_Sibling : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_After

   procedure C_Insert_After (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_insert_after')}";

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_First

   procedure C_Insert_First (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_insert_first')}";

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_Last

   procedure C_Insert_Last (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_insert_last')}";

   procedure Remove_Child (Handle : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Remove_Child

   procedure C_Remove_Child (Handle : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_remove_child')}";

   --------------------------------------
   -- Implementation for node creation --
   --------------------------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Clone

   function C_Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_clone')}";

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind}) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Node

   function C_Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${node_kind_type}) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_create_node')}";

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind};
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Token_Node

   function C_Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${node_kind_type};
      Text   : access ${text_type}) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_create_token_node')}";

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${T.node_kind};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Regular_Node

   function C_Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${node_kind_type};
      Children : C_Node_Array.Object_Pointer;
      Count    : Interfaces.C.int) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('rewriting_create_regular_node')}";

   ----------------------------------
   -- Implementation for templates --
   ----------------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_From_Template

   function C_Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : access ${text_type};
      Arguments : C_Node_Array.Object_Pointer;
      Count     : Interfaces.C.int;
      Rule      : ${grammar_rule_type}) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name =>
             "${capi.get_name('rewriting_create_from_template')}";

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------

   ## Emit shortcuts for constructors of nodes that have fields

   % for n in ctx.node_types:
      % if not n.abstract and \
            not n.is_token_node and \
            not n.is_list_type and \
            n.get_parse_fields():

         function Create_${n.entity.api_name}
           (Handle : Rewriting_Handle
            % for f in n.get_parse_fields():
               ; ${f.names.codegen} : Node_Rewriting_Handle
            % endfor
            ) return Node_Rewriting_Handle;

      % endif
   % endfor

end ${ada_lib_name}.Rewriting_Implementation;
