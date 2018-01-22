## vim: filetype=makoada

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

package body ${ada_lib_name}.Rewriting is

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
     (Get_Rewriting_Handle (Context));

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
     (Handle.Context);

   function Convert is new Ada.Unchecked_Conversion
     (AST_Node_Pointer, ${root_node_type_name});
   function Convert is new Ada.Unchecked_Conversion
     (${root_node_type_name}, AST_Node_Pointer);

   function Hash (Node : AST_Node_Pointer) return Ada.Containers.Hash_Type is
     (Hash (Convert (Node)));

   function Handle
     (Node : ${root_node_type_name}) return Node_Rewriting_Handle;
   --  Helper to implement the public "Handle" function, which works on public
   --  entities type.

   function Allocate
     (Node          : ${root_node_type_name};
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;
   --  Allocate a handle for Node and register it in Unit_Handle's map

   procedure Expand_Children
     (Node        : Node_Rewriting_Handle;
      Unit_Handle : Unit_Rewriting_Handle);
   --  If Node.Children.Expanded, do nothing. Otherwise, populate Node's list
   --  of Children to mimic the related bare AST node.

   procedure Free_Handles (Handle : in out Rewriting_Handle);
   --  Free all resources tied to Handle. This also releases the rewriting
   --  handle singleton in Handle's Context.

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
   is
      Result : constant Rewriting_Handle := new Rewriting_Handle_Type'
        (Context => Context,
         Units   => <>);
   begin
      Set_Rewriting_Handle (Context, Result);
      return Result;
   end Start_Rewriting;

   -----------
   -- Apply --
   -----------

   procedure Apply (Handle : in out Rewriting_Handle) is
   begin
      Free_Handles (Handle);
   end Apply;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
      use Unit_Maps;

      Context        : constant Analysis_Context := Analysis.Context (Unit);
      Context_Handle : constant Rewriting_Handle := Handle (Context);
      Filename       : constant Unbounded_String :=
         To_Unbounded_String (Get_Filename (Unit));
      Cur            : constant Cursor := Context_Handle.Units.Find (Filename);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      end if;

      declare
         Result : constant Unit_Rewriting_Handle :=
            new Unit_Rewriting_Handle_Type'(Context_Handle => Context_Handle,
                                            Unit           => Unit,
                                            Nodes          => <>);
      begin
         Context_Handle.Units.Insert (Filename, Result);
         return Result;
      end;
   end Handle;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle is
   begin
      return Handle (Bare_Node (Node));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_entity.api_name} is
   begin
      return Create (Convert (Handle.Node));
   end Node;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : ${root_node_type_name}) return Node_Rewriting_Handle is
   begin
      if Node = null then
         return No_Node_Rewriting_Handle;
      end if;

      declare
         use Node_Maps;

         N           : constant AST_Node_Pointer := Convert (Node);
         Unit_Handle : constant Unit_Rewriting_Handle :=
            Handle (Node.Get_Unit);
         Cur         : constant Cursor := Unit_Handle.Nodes.Find (N);
      begin
         --  If we have already built a handle for this node, just return it
         if Cur /= No_Element then
            return Element (Cur);

         --  Otherwise, if this node has a parent, make sure this parent has
         --  its own handle, then expand its children. This last must create
         --  the handle we are supposed to return.
         elsif Node.Parent /= null then
            Expand_Children (Handle (Node.Parent), Unit_Handle);
            return Element (Unit_Handle.Nodes.Find (N));
         end if;

         --  Otherwise, we are dealing with the root node: just create its
         --  rewriting handle.
         return Allocate (Node, Unit_Handle, No_Node_Rewriting_Handle);
      end;
   end Handle;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Node          : ${root_node_type_name};
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      N      : constant AST_Node_Pointer := Convert (Node);
      Result : constant Node_Rewriting_Handle :=
         new Node_Rewriting_Handle_Type'
           (Context_Handle => Unit_Handle.Context_Handle,
            Node           => N,
            Parent         => Parent_Handle,
            Children       => Unexpanded_Children);
   begin
      Unit_Handle.Nodes.Insert (N, Result);
      return Result;
   end Allocate;

   ---------------------
   -- Expand_Children --
   ---------------------

   procedure Expand_Children
     (Node        : Node_Rewriting_Handle;
      Unit_Handle : Unit_Rewriting_Handle)
   is
      Children : Node_Children renames Node.Children;
   begin
      if Children.Expanded then
         return;
      end if;

      Children := (Expanded => True, Vector => <>);
      declare
         N     : constant ${root_node_type_name} := Convert (Node.Node);
         Count : constant Natural := N.Children_Count;
      begin
         Children.Vector.Reserve_Capacity (Ada.Containers.Count_Type (Count));
         for I in 1 .. Count loop
            Children.Vector.Append (Allocate (N.Child (I), Unit_Handle, Node));
         end loop;
      end;
   end Expand_Children;

   ------------------
   -- Free_Handles --
   ------------------

   procedure Free_Handles (Handle : in out Rewriting_Handle) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Rewriting_Handle_Type, Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Unit_Rewriting_Handle_Type, Unit_Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Rewriting_Handle_Type, Node_Rewriting_Handle);

      Ctx : constant Analysis_Context := Context (Handle);
   begin
      --  Free all resources tied to Handle
      for Unit of Handle.Units loop
         for Node of Unit.Nodes loop
            Free (Node);
         end loop;
         Free (Unit);
      end loop;
      Free (Handle);

      --  Release the rewriting handle singleton for its context
      Set_Rewriting_Handle (Ctx, Handle);
   end Free_Handles;

end ${ada_lib_name}.Rewriting;
