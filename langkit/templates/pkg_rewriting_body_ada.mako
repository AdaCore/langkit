## vim: filetype=makoada

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;
with ${ada_lib_name}.Analysis.Parsers; use ${ada_lib_name}.Analysis.Parsers;
with ${ada_lib_name}.Unparsing.Implementation;
use ${ada_lib_name}.Unparsing.Implementation;

package body ${ada_lib_name}.Rewriting is

   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Rewriting_Handle_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle_Pointer, Rewriting_Handle);

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
     (Convert (Get_Rewriting_Handle (Context)));

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
     (Handle.Context);

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

   procedure Expand_Children (Node : Node_Rewriting_Handle);
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
      Set_Rewriting_Handle (Context, Convert (Result));
      return Result;
   end Start_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Boolean is

      type Processed_Unit_Record is record
         Unit     : Analysis_Unit;
         New_Data : Reparsed_Unit;
      end record;
      type Processed_Unit is access Processed_Unit_Record;
      procedure Free is new Ada.Unchecked_Deallocation
        (Processed_Unit_Record, Processed_Unit);

      package Processed_Unit_Vectors is new Ada.Containers.Vectors
        (Positive, Processed_Unit);

      Units   : Processed_Unit_Vectors.Vector;
      Success : Boolean := True;

   begin
      --  Try to reparse all units that were potentially modified
      for Unit_Handle of Handle.Units loop
         declare
            procedure Init_Parser
              (Unit     : Analysis_Unit;
               Read_BOM : Boolean;
               Parser   : in out Parser_Type);
            --  Callback for the call to Do_Parsing below

            PU     : constant Processed_Unit := new Processed_Unit_Record'
              (Unit     => Unit_Handle.Unit,
               New_Data => <>);
            Buffer : String_Access;

            -----------------
            -- Init_Parser --
            -----------------

            procedure Init_Parser
              (Unit     : Analysis_Unit;
               Read_BOM : Boolean;
               Parser   : in out Parser_Type) is
            begin
               Init_Parser_From_Buffer
                 (Buffer          => Buffer.all,
                  Charset         => Get_Charset (Unit),
                  Read_BOM        => Read_BOM,
                  Unit            => PU.Unit,
                  TDH             => Token_Data (PU.Unit),
                  Symbol_Literals => Parsers.Symbol_Literal_Array_Access
                    (Symbol_Literals (Context (PU.Unit))),
                  With_Trivia     => True,
                  Parser          => Parser);
            end Init_Parser;
         begin
            Units.Append (PU);

            --  Reparse (i.e. unparse and then parse) this rewritten unit
            Buffer := Unparse (Unit_Handle.Root, PU.Unit);
            Do_Parsing
              (Unit        => PU.Unit,
               Read_BOM    => False,
               Init_Parser => Init_Parser'Access,
               Result      => PU.New_Data);
            Free (Buffer);

            --  If there is a parsing error, abort the rewriting process
            if not PU.New_Data.Diagnostics.Is_Empty then
               Success := False;
               Destroy (PU.New_Data);
               exit;
            end if;
         end;
      end loop;

      --  If all reparsing went fine, actually replace the AST nodes all over
      --  the context and free all resources associated to Handle.
      if Success then
         for PU of Units loop
            Update_After_Reparse (PU.Unit, PU.New_Data);
         end loop;
         Free_Handles (Handle);
      end if;

      --  Clean-up our local resources and return
      for PU of Units loop
         Free (PU);
      end loop;
      return Success;
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
                                            Root           => <>,
                                            Nodes          => <>);
      begin
         Context_Handle.Units.Insert (Filename, Result);
         Result.Root := Handle (Root (Unit));
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
      return Create (Handle.Node);
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

         Unit_Handle : constant Unit_Rewriting_Handle :=
            Handle (Node.Get_Unit);
         Cur         : constant Cursor := Unit_Handle.Nodes.Find (Node);
      begin
         --  If we have already built a handle for this node, just return it
         if Cur /= No_Element then
            return Element (Cur);

         --  Otherwise, if this node has a parent, make sure this parent has
         --  its own handle, then expand its children. This last must create
         --  the handle we are supposed to return.
         elsif Node.Parent /= null then
            Expand_Children (Handle (Node.Parent));
            return Element (Unit_Handle.Nodes.Find (Node));
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
      Result : constant Node_Rewriting_Handle :=
         new Node_Rewriting_Handle_Type'
           (Context_Handle => Unit_Handle.Context_Handle,
            Node           => Node,
            Parent         => Parent_Handle,
            Kind           => Node.Kind,
            Children       => Unexpanded_Children);
   begin
      Unit_Handle.Nodes.Insert (Node, Result);
      return Result;
   end Allocate;

   ---------------------
   -- Expand_Children --
   ---------------------

   procedure Expand_Children (Node : Node_Rewriting_Handle) is
      Children : Node_Children renames Node.Children;
   begin
      --  If this handle has already be expanded, there is nothing to do
      if Children.Kind /= Unexpanded then
         return;
      end if;

      --  Otherwise, expand to the appropriate children form: token node or
      --  regular one.
      declare
         N           : constant ${root_node_type_name} := Node.Node;
         Unit_Handle : constant Unit_Rewriting_Handle := Handle (N.Get_Unit);
      begin
         if N.Is_Token_Node then
            Children := (Kind => Expanded_Token_Node,
                         Text => To_Unbounded_Wide_Wide_String (N.Text));

         else
            Children := (Kind => Expanded_Regular, Vector => <>);
            declare
               Count : constant Natural := N.Children_Count;
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  Children.Vector.Append
                    (Allocate (N.Child (I), Unit_Handle, Node));
               end loop;
            end;
         end if;
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
      Set_Rewriting_Handle (Ctx, Convert (Handle));
   end Free_Handles;

   ----------
   -- Kind --
   ----------

   overriding function Kind
     (Node : access Node_Rewriting_Handle_Type) return ${root_node_kind_name}
   is
   begin
      return Node.Kind;
   end Kind;

   --------------------
   -- Children_Count --
   --------------------

   overriding function Children_Count
     (Node : access Node_Rewriting_Handle_Type) return Natural is
   begin
      return
        (case Node.Children.Kind is
         when Unexpanded          => Node.Node.Children_Count,
         when Expanded_Regular    => Natural (Node.Children.Vector.Length),
         when Expanded_Token_Node => 0);
   end Children_Count;

   --------------------
   -- Abstract_Child --
   --------------------

   overriding function Abstract_Child
     (Node  : access Node_Rewriting_Handle_Type;
      Index : Positive) return Analysis.Implementation.Abstract_Node is
   begin
      return
        (case Node.Children.Kind is
         when Unexpanded          => Node.Node.Abstract_Child (Index),
         when Expanded_Regular    =>
            Analysis.Implementation.Abstract_Node
              (Node.Children.Vector.Element (Index)),
         when Expanded_Token_Node => null);
   end Abstract_Child;

   -------------------
   -- Abstract_Text --
   -------------------

   overriding function Abstract_Text
     (Node : access Node_Rewriting_Handle_Type) return Text_Type is
   begin
      case Node.Children.Kind is
         when Unexpanded =>
            if Is_Token_Node_Kind (Node.Kind) then
               return Node.Node.Text;
            else
               raise Program_Error;
            end if;

         when Expanded_Regular =>
            raise Program_Error;

         when Expanded_Token_Node =>
            return To_Wide_Wide_String (Node.Children.Text);
      end case;
   end Abstract_Text;

end ${ada_lib_name}.Rewriting;
