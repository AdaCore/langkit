## vim: filetype=makoada

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Bump_Ptr; use Langkit_Support.Bump_Ptr;
with Langkit_Support.Bump_Ptr_Vectors;

with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;

--  Internal package: low-level primitives to implement syntax-based source
--  rewriting.

private package ${ada_lib_name}.Rewriting_Implementation is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;

   type Rewriting_Handle is access Rewriting_Handle_Type;
   --  Internal handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type;
   --  Internal handle for the process of rewriting an analysis unit

   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type;
   --  Internal handle for the process of rewriting an analysis unit

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
      --  Keep track of rewriting handles we create all units that Context owns

      Pool      : Bump_Ptr_Pool;
      New_Nodes : Nodes_Pools.Vector;
      --  Keep track of all node rewriting handles that don't map to original
      --  nodes, i.e. all nodes that were created during this rewriting
      --  session.
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

      Expanded_Token_Node
      --  Expanded node rewriting handle, specific for token nodes: there is no
      --  children, only some associated text.
   );

   type Node_Children (Kind : Node_Children_Kind := Unexpanded) is record
      case Kind is
         when Unexpanded          => null;
         when Expanded_Regular    => Vector : Node_Vectors.Vector;
         when Expanded_Token_Node => Text   : Unbounded_Wide_Wide_String;
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

   function Context (Handle : Rewriting_Handle) return Internal_Context;
   --  Implementation for Rewriting.Context

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
      with Post => Handle (Context) /= No_Rewriting_Handle
                   and then Has_With_Trivia (Context)
                   and then Start_Rewriting'Result = Handle (Context)
                   and then ${ada_lib_name}.Rewriting_Implementation.Context
                             (Start_Rewriting'Result) = Context;
   --  Implementation for Rewriting.Start_Rewriting

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle)
      with Post => Handle = No_Rewriting_Handle;
   --  Implementation for Rewriting.Abort_Rewriting

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

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Implementation for Rewriting.Unit_Handles

   ---------------------------------------
   -- Implementation for unit rewriting --
   ---------------------------------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit;
   --  Implementation for Rewriting.Unit

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Root

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Root

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Implementation for Rewriting.Unparse

   ---------------------------------------
   -- Implementation for node rewriting --
   ---------------------------------------

   function Handle
     (Node : ${T.root_node.name}) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Node
     (Handle : Node_Rewriting_Handle) return ${T.root_node.name};
   --  Implementation for Rewriting.Node

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Implementation for Rewriting.Context

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Unparse

   function Kind (Handle : Node_Rewriting_Handle) return ${T.node_kind};
   --  Implementation for Rewriting.Kind

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Implementation for Rewriting.Tied

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Parent

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Implementation for Rewriting.Children_Count

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Child

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Child

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Text

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Implementation for Rewriting.Set_Text

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Replace

   --------------------------------------------
   -- Implementation for list node rewriting --
   --------------------------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
      with Post => Rewriting_Implementation.Child
                     (Handle, Index) = Child;
   --  Implementation for Rewriting.Insert_Child

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle)
      with Post => Rewriting_Implementation.Child
                     (Handle, Children_Count (Handle)) = Child;
   --  Implementation for Rewriting.Append_Child

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive);
   --  Implementation for Rewriting.Remove_Child

   --------------------------------------
   -- Implementation for node creation --
   --------------------------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Clone

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind}) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Node

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind};
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Token_Node

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${T.node_kind};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Regular_Node

   ----------------------------------
   -- Implementation for templates --
   ----------------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_From_Template

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------

   ## Emit shortcuts for constructors of nodes that have fields

   % for n in ctx.astnode_types:
      % if not n.abstract and \
            not n.is_token_node and \
            not n.is_list_type and \
            n.get_parse_fields():

         function Create_${n.entity.api_name}
           (Handle : Rewriting_Handle
            % for f in n.get_parse_fields():
               ; ${f.name} : Node_Rewriting_Handle
            % endfor
            ) return Node_Rewriting_Handle;

      % endif
   % endfor

end ${ada_lib_name}.Rewriting_Implementation;
