## vim: filetype=makoada

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;
private with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
private with ${ada_lib_name}.Analysis.Implementation;

package ${ada_lib_name}.Rewriting is

   type Rewriting_Handle is private;
   --  Handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is private;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   type Node_Rewriting_Handle is private;
   --  Handle for the process of rewriting an AST node. Such handles are owned
   --  by a Rewriting_Handle instance.

   No_Rewriting_Handle      : constant Rewriting_Handle;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle;

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Analysis_Context
      with Pre => Handle /= No_Rewriting_Handle;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
     with Pre  => Handle (Context) = No_Rewriting_Handle,
          Post => Handle (Context) /= No_Rewriting_Handle
                  and then Start_Rewriting'Result = Handle (Context)
                  and then ${ada_lib_name}.Rewriting.Context
                             (Start_Rewriting'Result) = Context;
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

   function Apply (Handle : in out Rewriting_Handle) return Boolean
      with Pre  => Handle /= No_Rewriting_Handle,
           Post => (if Apply'Result
                    then Handle = No_Rewriting_Handle
                    else Handle = Handle'Old);
   --  Apply all modifications to Handle's analysis context. If that worked,
   --  close Handle and return True. Otherwise, reparsing did not work, so keep
   --  Handle and its Context unchanged and return False.

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle
      with Pre => Handle (Context (Unit)) /= No_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Unit

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle
      with Pre => Handle (Context (Get_Unit (Node))) /= No_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Node

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_entity.api_name}
      with Pre => Handle /= No_Node_Rewriting_Handle;
   --  Return the node which the given rewriting Handle relates to. This can
   --  be the null entity if this handle designates a new node.

private
   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;

   use ${ada_lib_name}.Analysis.Implementation;

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;
   type Rewriting_Handle is access Rewriting_Handle_Type;
   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type;

   No_Rewriting_Handle : constant Rewriting_Handle := null;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle := null;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle := null;

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

   type Rewriting_Handle_Type is record
      Context : Analysis_Context;
      --  Analysis context this rewriting handle relates to

      Units : Unit_Maps.Map;
      --  Keep track of rewriting handles we create all units that Context owns
   end record;

   type Unit_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context this relates to

      Unit : Analysis_Unit;
      --  Analysis unit this relates to

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

      Children : Node_Children;
      --  Lazily evaluated vector of children for the rewritten node
   end record;

   overriding function Kind
     (Node : access Node_Rewriting_Handle_Type) return ${root_node_kind_name};

   overriding function Children_Count
     (Node : access Node_Rewriting_Handle_Type) return Natural;

   overriding function Abstract_Child
     (Node  : access Node_Rewriting_Handle_Type;
      Index : Positive) return Analysis.Implementation.Abstract_Node;

   overriding function Abstract_Text
     (Node : access Node_Rewriting_Handle_Type) return Text_Type;

end ${ada_lib_name}.Rewriting;
