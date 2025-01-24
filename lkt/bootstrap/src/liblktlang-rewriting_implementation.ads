
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Interfaces.C;

with System;
with System.Address_To_Access_Conversions;

with Liblktlang_Support.Bump_Ptr; use Liblktlang_Support.Bump_Ptr;
with Liblktlang_Support.Bump_Ptr_Vectors;
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;

with Liblktlang.Common;   use Liblktlang.Common;
with Liblktlang.Implementation;   use Liblktlang.Implementation;
with Liblktlang.Implementation.C; use Liblktlang.Implementation.C;

--  Internal package: low-level primitives to implement syntax-based source
--  rewriting.

private package Liblktlang.Rewriting_Implementation is

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
     (Key_Type        => Bare_Lkt_Node,
      Element_Type    => Node_Rewriting_Handle,
      Hash            => Named_Hash,
      Equivalent_Keys => "=");

   package Nodes_Pools is new Liblktlang_Support.Bump_Ptr_Vectors
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

      Node : Bare_Lkt_Node;
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

      Kind : Lkt_Node_Kind_Type;
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
           External_Name => "lkt_rewriting_context_to_handle";

   function Context (Handle : Rewriting_Handle) return Internal_Context;
   --  Implementation for Rewriting.Context

   function C_Handle_To_Context
     (Handle : Rewriting_Handle) return Internal_Context
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_handle_to_context";

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
      with Post => Handle (Context) /= No_Rewriting_Handle
                   and then Has_With_Trivia (Context)
                   and then Start_Rewriting'Result = Handle (Context)
                   and then Liblktlang.Rewriting_Implementation.Context
                             (Start_Rewriting'Result) = Context;

   function C_Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_start_rewriting";
   --  Implementation for Rewriting.Start_Rewriting

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle)
      with Post => Handle = No_Rewriting_Handle;
   --  Implementation for Rewriting.Abort_Rewriting

   procedure C_Abort_Rewriting (Handle : Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_abort_rewriting";

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
     System.Address_To_Access_Conversions (lkt_diagnostic);

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
           External_Name => "lkt_rewriting_apply";

   procedure Free_Apply_Result (Result : access C_Apply_Result)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_free_apply_result";

   package C_Unit_Array is new
     System.Address_To_Access_Conversions (Unit_Rewriting_Handle);

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Implementation for Rewriting.Unit_Handles

   function C_Unit_Handles
     (Handle : Rewriting_Handle) return C_Unit_Array.Object_Pointer
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_handles";

   ---------------------------------------
   -- Implementation for unit rewriting --
   ---------------------------------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function C_Unit_To_Handle
     (Unit : Internal_Unit) return Unit_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_to_handle";

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit;
   --  Implementation for Rewriting.Unit

   function C_Handle_To_Unit
     (Handle : Unit_Rewriting_Handle) return Internal_Unit
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_handle_to_unit";

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Root

   function C_Root
     (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_root";

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Root

   procedure C_Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_set_root";

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Implementation for Rewriting.Unparse

   procedure C_Unparse
     (Handle : Unit_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_unparse";

   ---------------------------------------
   -- Implementation for node rewriting --
   ---------------------------------------

   function Handle
     (Node : Bare_Lkt_Node) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function C_Node_To_Handle
     (Node : lkt_base_node) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_to_handle";

   function Node
     (Handle : Node_Rewriting_Handle) return Bare_Lkt_Node;
   --  Implementation for Rewriting.Node

   function C_Handle_To_Node
     (Handle : Node_Rewriting_Handle) return lkt_base_node
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_handle_to_node";

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Implementation for Rewriting.Context

   function C_Node_To_Context
     (Node : Node_Rewriting_Handle) return Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_to_context";

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Unparse

   procedure C_Unparse
     (Handle : Node_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_unparse";

   function Kind (Handle : Node_Rewriting_Handle) return Lkt_Node_Kind_Type;
   --  Implementation for Rewriting.Kind

   function C_Kind (Handle : Node_Rewriting_Handle) return lkt_node_kind_enum
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_kind";

   function Image (Handle : Node_Rewriting_Handle) return String;
   --  Implementation for Rewriting.Image

   procedure C_Image
     (Handle : Node_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_image";

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Implementation for Rewriting.Tied

   function C_Tied (Handle : Node_Rewriting_Handle) return Interfaces.C.int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_tied";

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Parent

   function C_Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_parent";

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Implementation for Rewriting.Children_Count

   function C_Children_Count
     (Handle : Node_Rewriting_Handle) return Interfaces.C.int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_children_count";

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Child

   function C_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Interfaces.C.int) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_child";

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
           External_Name => "lkt_rewriting_children";

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
           External_Name => "lkt_rewriting_set_child";

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Text

   procedure C_Text
     (Handle : Node_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_text";

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Implementation for Rewriting.Set_Text

   procedure C_Set_Text
     (Handle : Node_Rewriting_Handle; Text : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_set_text";

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Replace

   procedure C_Replace (Handle, New_Node : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_replace";

   procedure Rotate (Handles : Node_Rewriting_Handle_Array);
   --  Implementation for Rewriting.Rotate

   procedure C_Rotate
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_rotate";

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
           External_Name => "lkt_rewriting_first_child";

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Last_Child

   function C_Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_last_child";

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Next_Child

   function C_Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_next_child";

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Previous_Child

   function C_Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_previous_child";

   procedure Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_Before

   procedure C_Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_before";

   procedure Insert_After (Handle, New_Sibling : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_After

   procedure C_Insert_After (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_after";

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_First

   procedure C_Insert_First (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_first";

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Insert_Last

   procedure C_Insert_Last (Handle, New_Sibling : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_last";

   procedure Remove_Child (Handle : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Remove_Child

   procedure C_Remove_Child (Handle : Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_remove_child";

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
           External_Name => "lkt_rewriting_clone";

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Node

   function C_Create_Node
     (Handle : Rewriting_Handle;
      Kind   : lkt_node_kind_enum) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_create_node";

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Token_Node

   function C_Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : lkt_node_kind_enum;
      Text   : access lkt_text) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_create_token_node";

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Lkt_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Regular_Node

   function C_Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : lkt_node_kind_enum;
      Children : C_Node_Array.Object_Pointer;
      Count    : Interfaces.C.int) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_create_regular_node";

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
      Template  : access lkt_text;
      Arguments : C_Node_Array.Object_Pointer;
      Count     : Interfaces.C.int;
      Rule      : lkt_grammar_rule) return Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name =>
             "lkt_rewriting_create_from_template";

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------



         function Create_Lexer_Case_Rule_Cond_Alt
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : Node_Rewriting_Handle
               ; Lexer_Case_Rule_Cond_Alt_F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule_Default_Alt
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_Default_Alt_F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Rule_Decl
           (Handle : Rewriting_Handle
               ; Grammar_Rule_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Grammar_Rule_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Lexer_Decl
           (Handle : Rewriting_Handle
               ; Synthetic_Lexer_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Synthetic_Lexer_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Node_Decl
           (Handle : Rewriting_Handle
               ; Node_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Self_Decl
           (Handle : Rewriting_Handle
               ; Self_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Lit_Decl
           (Handle : Rewriting_Handle
               ; Enum_Lit_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Field_Decl
           (Handle : Rewriting_Handle
               ; Field_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Field_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Field_Decl_F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Fun_Arg_Decl
           (Handle : Rewriting_Handle
               ; Fun_Arg_Decl_F_Decl_Annotations : Node_Rewriting_Handle
               ; Fun_Arg_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Fun_Arg_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Fun_Arg_Decl_F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lambda_Arg_Decl
           (Handle : Rewriting_Handle
               ; Lambda_Arg_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Lambda_Arg_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Lambda_Arg_Decl_F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Dyn_Var_Decl
           (Handle : Rewriting_Handle
               ; Dyn_Var_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Dyn_Var_Decl_F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Match_Val_Decl
           (Handle : Rewriting_Handle
               ; Match_Val_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Match_Val_Decl_F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Val_Decl
           (Handle : Rewriting_Handle
               ; Val_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Val_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Val_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Fun_Decl
           (Handle : Rewriting_Handle
               ; Fun_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Fun_Decl_F_Args : Node_Rewriting_Handle
               ; Fun_Decl_F_Return_Type : Node_Rewriting_Handle
               ; Fun_Decl_F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Env_Spec_Decl
           (Handle : Rewriting_Handle
               ; Env_Spec_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Env_Spec_Decl_F_Actions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Decl
           (Handle : Rewriting_Handle
               ; Generic_Decl_F_Generic_Formal_Decls : Node_Rewriting_Handle
               ; Generic_Decl_F_Decl : Node_Rewriting_Handle
               ; Generic_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Decl
           (Handle : Rewriting_Handle
               ; Grammar_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Grammar_Decl_F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Decl
           (Handle : Rewriting_Handle
               ; Lexer_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Lexer_Decl_F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Family_Decl
           (Handle : Rewriting_Handle
               ; Lexer_Family_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Lexer_Family_Decl_F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synth_Arg_Decl
           (Handle : Rewriting_Handle
               ; Synth_Arg_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synth_Fun_Decl
           (Handle : Rewriting_Handle
               ; Synth_Fun_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Any_Type_Decl
           (Handle : Rewriting_Handle
               ; Any_Type_Decl_F_Traits : Node_Rewriting_Handle
               ; Any_Type_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Any_Type_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Class_Alt_Decl
           (Handle : Rewriting_Handle
               ; Enum_Class_Alt_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Enum_Class_Alt_Decl_F_Traits : Node_Rewriting_Handle
               ; Enum_Class_Alt_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Function_Type
           (Handle : Rewriting_Handle
               ; Function_Type_F_Syn_Name : Node_Rewriting_Handle
               ; Function_Type_F_Traits : Node_Rewriting_Handle
               ; Function_Type_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; Generic_Formal_Type_Decl_F_Has_Class : Node_Rewriting_Handle
               ; Generic_Formal_Type_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Generic_Formal_Type_Decl_F_Traits : Node_Rewriting_Handle
               ; Generic_Formal_Type_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Class_Decl
           (Handle : Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Traits : Node_Rewriting_Handle
               ; Class_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Class_Decl
           (Handle : Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Traits : Node_Rewriting_Handle
               ; Enum_Class_Decl_F_Branches : Node_Rewriting_Handle
               ; Enum_Class_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Type_Decl
           (Handle : Rewriting_Handle
               ; Enum_Type_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Traits : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Literals : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Struct_Decl
           (Handle : Rewriting_Handle
               ; Struct_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Struct_Decl_F_Traits : Node_Rewriting_Handle
               ; Struct_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Struct_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Trait_Decl
           (Handle : Rewriting_Handle
               ; Trait_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Trait_Decl_F_Traits : Node_Rewriting_Handle
               ; Trait_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Trait_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decl_Annotation
           (Handle : Rewriting_Handle
               ; Decl_Annotation_F_Name : Node_Rewriting_Handle
               ; Decl_Annotation_F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decl_Annotation_Params
           (Handle : Rewriting_Handle
               ; Decl_Annotation_Params_F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Elsif_Branch
           (Handle : Rewriting_Handle
               ; Elsif_Branch_F_Cond_Expr : Node_Rewriting_Handle
               ; Elsif_Branch_F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Class_Case
           (Handle : Rewriting_Handle
               ; Enum_Class_Case_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Any_Of
           (Handle : Rewriting_Handle
               ; Any_Of_F_Expr : Node_Rewriting_Handle
               ; Any_Of_F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Array_Literal
           (Handle : Rewriting_Handle
               ; Array_Literal_F_Exprs : Node_Rewriting_Handle
               ; Array_Literal_F_Element_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; Base_Call_Expr_F_Name : Node_Rewriting_Handle
               ; Base_Call_Expr_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Predicate
           (Handle : Rewriting_Handle
               ; Base_Call_Expr_F_Name : Node_Rewriting_Handle
               ; Base_Call_Expr_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Propagate_Call
           (Handle : Rewriting_Handle
               ; Base_Call_Expr_F_Name : Node_Rewriting_Handle
               ; Base_Call_Expr_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Dot_Expr
           (Handle : Rewriting_Handle
               ; Base_Dot_Expr_F_Prefix : Node_Rewriting_Handle
               ; Base_Dot_Expr_F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Cond_Dotted_Name
           (Handle : Rewriting_Handle
               ; Base_Dot_Expr_F_Prefix : Node_Rewriting_Handle
               ; Base_Dot_Expr_F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; Bin_Op_F_Left : Node_Rewriting_Handle
               ; Bin_Op_F_Op : Node_Rewriting_Handle
               ; Bin_Op_F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Block_Expr
           (Handle : Rewriting_Handle
               ; Block_Expr_F_Val_Defs : Node_Rewriting_Handle
               ; Block_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Cast_Expr
           (Handle : Rewriting_Handle
               ; Cast_Expr_F_Expr : Node_Rewriting_Handle
               ; Cast_Expr_F_Excludes_Null : Node_Rewriting_Handle
               ; Cast_Expr_F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Error_On_Null
           (Handle : Rewriting_Handle
               ; Error_On_Null_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Instantiation
           (Handle : Rewriting_Handle
               ; Generic_Instantiation_F_Name : Node_Rewriting_Handle
               ; Generic_Instantiation_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Discard
           (Handle : Rewriting_Handle
               ; Grammar_Discard_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Dont_Skip
           (Handle : Rewriting_Handle
               ; Grammar_Dont_Skip_F_Expr : Node_Rewriting_Handle
               ; Grammar_Dont_Skip_F_Dont_Skip : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_List
           (Handle : Rewriting_Handle
               ; Grammar_List_F_List_Type : Node_Rewriting_Handle
               ; Grammar_List_F_Kind : Node_Rewriting_Handle
               ; Grammar_List_F_Expr : Node_Rewriting_Handle
               ; Grammar_List_F_Sep : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Null
           (Handle : Rewriting_Handle
               ; Grammar_Null_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt
           (Handle : Rewriting_Handle
               ; Grammar_Opt_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt_Error
           (Handle : Rewriting_Handle
               ; Grammar_Opt_Error_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt_Error_Group
           (Handle : Rewriting_Handle
               ; Grammar_Opt_Error_Group_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt_Group
           (Handle : Rewriting_Handle
               ; Grammar_Opt_Group_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Or_Expr
           (Handle : Rewriting_Handle
               ; Grammar_Or_Expr_F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Pick
           (Handle : Rewriting_Handle
               ; Grammar_Pick_F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Implicit_Pick
           (Handle : Rewriting_Handle
               ; Grammar_Pick_F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Predicate
           (Handle : Rewriting_Handle
               ; Grammar_Predicate_F_Expr : Node_Rewriting_Handle
               ; Grammar_Predicate_F_Prop_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Rule_Ref
           (Handle : Rewriting_Handle
               ; Grammar_Rule_Ref_F_Node_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Skip
           (Handle : Rewriting_Handle
               ; Grammar_Skip_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Stop_Cut
           (Handle : Rewriting_Handle
               ; Grammar_Stop_Cut_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Parse_Node_Expr
           (Handle : Rewriting_Handle
               ; Parse_Node_Expr_F_Node_Name : Node_Rewriting_Handle
               ; Parse_Node_Expr_F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Token_No_Case_Lit
           (Handle : Rewriting_Handle
               ; Token_No_Case_Lit_F_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Token_Pattern_Concat
           (Handle : Rewriting_Handle
               ; Token_Pattern_Concat_F_Left : Node_Rewriting_Handle
               ; Token_Pattern_Concat_F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Token_Ref
           (Handle : Rewriting_Handle
               ; Token_Ref_F_Token_Name : Node_Rewriting_Handle
               ; Token_Ref_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; If_Expr_F_Cond_Expr : Node_Rewriting_Handle
               ; If_Expr_F_Then_Expr : Node_Rewriting_Handle
               ; If_Expr_F_Alternatives : Node_Rewriting_Handle
               ; If_Expr_F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Isa
           (Handle : Rewriting_Handle
               ; Isa_F_Expr : Node_Rewriting_Handle
               ; Isa_F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Keep_Expr
           (Handle : Rewriting_Handle
               ; Keep_Expr_F_Expr : Node_Rewriting_Handle
               ; Keep_Expr_F_Keep_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lambda_Expr
           (Handle : Rewriting_Handle
               ; Lambda_Expr_F_Params : Node_Rewriting_Handle
               ; Lambda_Expr_F_Return_Type : Node_Rewriting_Handle
               ; Lambda_Expr_F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Lit
           (Handle : Rewriting_Handle
               ; Null_Lit_F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Block_String_Lit
           (Handle : Rewriting_Handle
               ; Block_String_Lit_F_Lines : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Assign
           (Handle : Rewriting_Handle
               ; Logic_Assign_F_Dest_Var : Node_Rewriting_Handle
               ; Logic_Assign_F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Expr
           (Handle : Rewriting_Handle
               ; Logic_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Propagate
           (Handle : Rewriting_Handle
               ; Logic_Propagate_F_Dest_Var : Node_Rewriting_Handle
               ; Logic_Propagate_F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Unify
           (Handle : Rewriting_Handle
               ; Logic_Unify_F_Lhs : Node_Rewriting_Handle
               ; Logic_Unify_F_Rhs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Match_Expr
           (Handle : Rewriting_Handle
               ; Match_Expr_F_Match_Expr : Node_Rewriting_Handle
               ; Match_Expr_F_Branches : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Not_Expr
           (Handle : Rewriting_Handle
               ; Not_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; Paren_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; Raise_Expr_F_Dest_Type : Node_Rewriting_Handle
               ; Raise_Expr_F_Except_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subscript_Expr
           (Handle : Rewriting_Handle
               ; Subscript_Expr_F_Prefix : Node_Rewriting_Handle
               ; Subscript_Expr_F_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Cond_Subscript_Expr
           (Handle : Rewriting_Handle
               ; Subscript_Expr_F_Prefix : Node_Rewriting_Handle
               ; Subscript_Expr_F_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Try_Expr
           (Handle : Rewriting_Handle
               ; Try_Expr_F_Try_Expr : Node_Rewriting_Handle
               ; Try_Expr_F_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; Un_Op_F_Op : Node_Rewriting_Handle
               ; Un_Op_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Full_Decl
           (Handle : Rewriting_Handle
               ; Full_Decl_F_Doc : Node_Rewriting_Handle
               ; Full_Decl_F_Decl_Annotations : Node_Rewriting_Handle
               ; Full_Decl_F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_List_Sep
           (Handle : Rewriting_Handle
               ; Grammar_List_Sep_F_Token : Node_Rewriting_Handle
               ; Grammar_List_Sep_F_Extra : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Import
           (Handle : Rewriting_Handle
               ; Import_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Langkit_Root
           (Handle : Rewriting_Handle
               ; Langkit_Root_F_Imports : Node_Rewriting_Handle
               ; Langkit_Root_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_F_Expr : Node_Rewriting_Handle
               ; Lexer_Case_Rule_F_Alts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule_Send
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_Send_F_Sent : Node_Rewriting_Handle
               ; Lexer_Case_Rule_Send_F_Match_Size : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Match_Branch
           (Handle : Rewriting_Handle
               ; Match_Branch_F_Decl : Node_Rewriting_Handle
               ; Match_Branch_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Param
           (Handle : Rewriting_Handle
               ; Param_F_Name : Node_Rewriting_Handle
               ; Param_F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Function_Type_Ref
           (Handle : Rewriting_Handle
               ; Function_Type_Ref_F_Args_Types : Node_Rewriting_Handle
               ; Function_Type_Ref_F_Return_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Type_Ref
           (Handle : Rewriting_Handle
               ; Generic_Type_Ref_F_Type_Name : Node_Rewriting_Handle
               ; Generic_Type_Ref_F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Simple_Type_Ref
           (Handle : Rewriting_Handle
               ; Simple_Type_Ref_F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Var_Bind
           (Handle : Rewriting_Handle
               ; Var_Bind_F_Name : Node_Rewriting_Handle
               ; Var_Bind_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


end Liblktlang.Rewriting_Implementation;
