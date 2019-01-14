## vim: filetype=makoada

--  This package provides support for tree-based source code rewriting.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Bump_Ptr; use Langkit_Support.Bump_Ptr;
with Langkit_Support.Bump_Ptr.Vectors;

with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;

private package ${ada_lib_name}.Rewriting_Implementation is

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;

   type Rewriting_Handle is access Rewriting_Handle_Type;
   --  Handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unit_Rewriting_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => ${root_node_type_name},
      Element_Type    => Node_Rewriting_Handle,
      Hash            => Named_Hash,
      Equivalent_Keys => "=");

   package Nodes_Pools is new Langkit_Support.Bump_Ptr.Vectors
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

   type Node_Rewriting_Handle_Type is new Abstract_Node_Type with record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context that owns Node

      Node : ${root_node_type_name};
      --  Bare AST node which this rewriting handle relates to

      Parent : Node_Rewriting_Handle;
      --  Rewriting handle for Node's parent, or No_Node_Rewriting_Handle if
      --  Node is a root node.

      Kind : ${root_node_kind_name};
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

   -----------------------
   -- Context rewriting --
   -----------------------

   function Handle (Context : Internal_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Internal_Context;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle;
   --  Start a rewriting session for Context.
   --
   --  This handle will keep track of all changes to do on Context's analysis
   --  units. Once the set of changes is complete, call the Apply procedure to
   --  actually update Context. This makes it possible to inspect the "old"
   --  Context state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an Existing_Rewriting_Handle_Error exception if Context
   --  already has a living rewriting session.

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle);
   --  Discard all modifications registered in Handle and close Handle

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

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result;
   --  Apply all modifications to Handle's analysis context. If that worked,
   --  close Handle and return (Success => True). Otherwise, reparsing did not
   --  work, so keep Handle and its Context unchanged and return details about
   --  the error that happened.

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array
      with Pre => Handle /= No_Rewriting_Handle;
   --  Return the list of unit rewriting handles in the given context handle
   --  for units that the Apply primitive will modify.

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Unit

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit;
   --  Return the unit corresponding to Handle

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return the node handle corresponding to the root of the unit which
   --  Handle designates.

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Set the root node for the unit Handle to Root. This unties the previous
   --  root handle. If Root is not No_Node_Rewriting_Handle, this also ties
   --  Root to Handle.

   --------------------
   -- Node rewriting --
   --------------------

   function Handle
     (Node : ${root_node_type_name}) return Node_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Node

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_node_type_name};
   --  Return the node which the given rewriting Handle relates to. This can
   --  be the null entity if this handle designates a new node.

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Return a handle for the rewriting context to which Handle belongs

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Turn the given rewritten node Handles designates into text. This is the
   --  text that is used in Apply in order to re-create an analysis unit.

   function Kind (Handle : Node_Rewriting_Handle) return ${root_node_kind_name};
   --  Return the kind corresponding to Handle's node

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether this node handle is tied to an analysis unit. If it is
   --  not, it can be passed as the Child parameter to Set_Child.

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return a handle for the node that is the parent of Handle's node. This
   --  is No_Rewriting_Handle for a node that is not tied to any tree yet.

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Return the number of children the node represented by Handle has

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle
      with Pre => Handle /= No_Node_Rewriting_Handle
                  and then Index in 1 .. Children_Count (Handle);
   --  Return a handle corresponding to the Index'th child of the node that
   --  Handle represents. Index is 1-based.

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
      with Pre =>
         Handle /= No_Node_Rewriting_Handle
         and then Index in 1 .. Children_Count (Handle)
         and then (Child = No_Node_Rewriting_Handle or else not Tied (Child));
   --  If Child is No_Rewriting_Node, untie the Handle's Index'th child to this
   --  tree, so it can be attached to another one. Otherwise, Child must have
   --  no parent as it will be tied to Handle's tree.

   function Text (Handle : Node_Rewriting_Handle) return Text_Type
      with Pre => Handle /= No_Node_Rewriting_Handle
                  and then Is_Token_Node (Kind (Handle));
   --  Return the text associated to the given token node

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type)
      with Pre => Handle /= No_Node_Rewriting_Handle
                  and then Is_Token_Node (Kind (Handle));
   --  Override text associated to the given token node

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle)
      with Pre =>
         Handle /= No_Node_Rewriting_Handle
         and then Tied (Handle)
         and then (New_Node = No_Node_Rewriting_Handle
                   or else not Tied (New_Node));
   --  If Handle is the root of an analysis unit, untie it and set New_Node as
   --  its new root. Otherwise, replace Handle with New_Node in Handle's parent
   --  node.

   -------------------------
   -- List node rewriting --
   -------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  Assuming Handle refers to a list node, insert the given Child node to be
   --  in the children list at the given index.

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle);
   --  Assuming Handle refers to a list node, append the given Child node to
   --  the children list.

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive);
   --  Assuming Handle refers to a list node, remove the child at the given
   --  Index from the children list.

   -------------------
   -- Node creation --
   -------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Create a clone of the Handle node tree. The result is not tied to any
   --  analysis unit tree.

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${root_node_kind_name}) return Node_Rewriting_Handle;
   --  Create a new node of the given Kind, with empty text (for token nodes)
   --  or children (for regular nodes).

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${root_node_kind_name};
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Create a new token node with the given Kind and Text

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${root_node_kind_name};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Create a new regular node of the given Kind and assign it the given
   --  Children.
   --
   --  Except for lists, which can have any number of children, the
   --  size of Children must match the number of children associated to the
   --  given Kind. Besides, all given children must not be tied.

   ---------------
   -- Templates --
   ---------------

   --  Templating is a way to create trees of node rewriting handles. It is
   --  intended to be more convenient than calling node constructors for each
   --  individual node in a tree.
   --
   --  A template is text that represents source code, including zero or
   --  multiple holes (stray "{}").
   --
   --  Create a tree of new nodes from a template is called instantiating a
   --  template: just call Create_From_Template, passing to it the template
   --  itself, a sequence of nodes (the template arguments) to fill the
   --  template holes and a grammar rule to parse the resulting source code.
   --  This will unparse given nodes to replace holes in the template text, and
   --  then parse the resulting source code in order to create a tree of node
   --  rewriting handles.
   --
   --  In order not to interfer with the template DSL, stray "{" and "}"
   --  characters in the source code must be doubled: for instance "{{"
   --  represent "{" in the source code to be parsed.

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Create a tree of new nodes from the given Template string, filling holes
   --  in it with nodes in Arguments and parsed according to the given grammar
   --  Rule.

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

   overriding function Abstract_Kind
     (Node : access Node_Rewriting_Handle_Type) return ${root_node_kind_name};

   overriding function Abstract_Children_Count
     (Node : access Node_Rewriting_Handle_Type) return Natural;

   overriding function Abstract_Child
     (Node  : access Node_Rewriting_Handle_Type;
      Index : Positive) return Implementation.Abstract_Node;

   overriding function Abstract_Text
     (Node : access Node_Rewriting_Handle_Type) return Text_Type;

   overriding function Abstract_Rewritten_Node
     (Node : access Node_Rewriting_Handle_Type) return ${root_node_type_name};

end ${ada_lib_name}.Rewriting_Implementation;
