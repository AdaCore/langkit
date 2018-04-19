## vim: filetype=makoada

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;
with ${ada_lib_name}.Analysis.Parsers; use ${ada_lib_name}.Analysis.Parsers;
with ${ada_lib_name}.Introspection;    use ${ada_lib_name}.Introspection;
with ${ada_lib_name}.Lexer;            use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;
with ${ada_lib_name}.Unparsing.Implementation;
use ${ada_lib_name}.Unparsing.Implementation;

package body ${ada_lib_name}.Rewriting is

   --  Access to rewriting handle data is always done through Rewriting_Handle,
   --  values (never through Rewriting_Handle_Pointer, except after calling
   --  Convert), so there is no aliasing problem possible.

   pragma Warnings (Off, "possible aliasing problem for type");
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Rewriting_Handle_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle_Pointer, Rewriting_Handle);
   pragma Warnings (On, "possible aliasing problem for type");

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
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
      with Pre =>
         Context /= No_Rewriting_Handle
         and then (Unit_Handle = No_Unit_Rewriting_Handle
                   or else Unit_Handle.Context_Handle = Context)
         and then (Parent_Handle = No_Node_Rewriting_Handle
                   or else Parent_Handle.Context_Handle = Context);
   --  Allocate a handle for Node and register it in Unit_Handle's map

   procedure Expand_Children (Node : Node_Rewriting_Handle)
      with Pre => Node /= No_Node_Rewriting_Handle;
   --  If Node.Children.Kind is Unexpanded, populate Node's list of Children to
   --  mimic the related bare AST node. Otherwise, do nothing.

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
        (Context   => Context,
         Units     => <>,
         Pool      => Create,
         New_Nodes => <>);
   begin
      Result.New_Nodes := Nodes_Pools.Create (Result.Pool);
      Set_Rewriting_Handle (Context, Convert (Result));
      return Result;
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
   begin
      Free_Handles (Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is

      type Processed_Unit_Record is record
         Unit     : Analysis_Unit;
         New_Data : Reparsed_Unit;
      end record;
      type Processed_Unit is access Processed_Unit_Record;
      procedure Free is new Ada.Unchecked_Deallocation
        (Processed_Unit_Record, Processed_Unit);

      package Processed_Unit_Vectors is new Ada.Containers.Vectors
        (Positive, Processed_Unit);

      Units  : Processed_Unit_Vectors.Vector;
      Result : Apply_Result := (Success => True);

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
            Buffer := Unparse (Unit_Handle.Root, PU.Unit,
                               Preserve_Formatting => True);
            Do_Parsing
              (Unit        => PU.Unit,
               Read_BOM    => False,
               Init_Parser => Init_Parser'Access,
               Result      => PU.New_Data);
            Free (Buffer);

            --  If there is a parsing error, abort the rewriting process
            if not PU.New_Data.Diagnostics.Is_Empty then
               Result := (Success     => False,
                          Unit        => PU.Unit,
                          Diagnostics => <>);
               Result.Diagnostics.Move (PU.New_Data.Diagnostics);
               Destroy (PU.New_Data);
               exit;
            end if;
         end;
      end loop;

      --  If all reparsing went fine, actually replace the AST nodes all over
      --  the context and free all resources associated to Handle.
      if Result.Success then
         for PU of Units loop
            Update_After_Reparse (PU.Unit, PU.New_Data);
         end loop;
         Free_Handles (Handle);
      end if;

      --  Clean-up our local resources and return
      for PU of Units loop
         Free (PU);
      end loop;
      return Result;
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array
   is
      Count  : constant Natural := Natural (Handle.Units.Length);
      Result : Unit_Rewriting_Handle_Array (1 .. Count);
      I      : Positive := 1;
   begin
      for Unit of Handle.Units loop
         Result (I) := Unit;
         I := I + 1;
      end loop;
      return Result;
   end Unit_Handles;

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

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit
   is
   begin
      return Handle.Unit;
   end Unit;

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

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      return Handle.Context_Handle;
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Unparsing.Implementation.Unparse
        (Handle, Preserve_Formatting => True);
   end Unparse;

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
            Handle (Node.Unit);
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
         return Allocate (Node, Unit_Handle.Context_Handle, Unit_Handle,
                          No_Node_Rewriting_Handle);
      end;
   end Handle;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Node          : ${root_node_type_name};
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      Tied   : constant Boolean := Unit_Handle /= No_Unit_Rewriting_Handle;
      Result : constant Node_Rewriting_Handle :=
         new Node_Rewriting_Handle_Type'
           (Context_Handle => Context,
            Node           => Node,
            Parent         => Parent_Handle,
            Kind           => Node.Kind,
            Tied           => Tied,
            Children       => Unexpanded_Children);
   begin
      if Tied then
         Unit_Handle.Nodes.Insert (Node, Result);
      end if;
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
         Unit_Handle : constant Unit_Rewriting_Handle := Handle (N.Unit);
      begin
         if N.Is_Token_Node then
            Children := (Kind => Expanded_Token_Node,
                         Text => To_Unbounded_Wide_Wide_String (N.Text));

         else
            Children := (Kind => Expanded_Regular, Vector => <>);
            declare
               Count : constant Natural := N.Abstract_Children_Count;
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  declare
                     Child : constant ${root_node_type_name} := N.Child (I);
                  begin
                     Children.Vector.Append
                       ((if Child = null
                         then null
                         else Allocate (Child, Unit_Handle.Context_Handle,
                                        Unit_Handle, Node)));
                  end;
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
      for Node of Handle.New_Nodes loop
         declare
            N : Node_Rewriting_Handle := Node;
         begin
            Free (N);
         end;
      end loop;
      Free (Handle.Pool);
      Free (Handle);

      --  Release the rewriting handle singleton for its context
      Set_Rewriting_Handle (Ctx, Convert (Handle));
   end Free_Handles;

   -------------------
   -- Abstract_Kind --
   -------------------

   overriding function Abstract_Kind
     (Node : access Node_Rewriting_Handle_Type) return ${root_node_kind_name}
   is
   begin
      return Node.Kind;
   end Abstract_Kind;

   -----------------------------
   -- Abstract_Children_Count --
   -----------------------------

   overriding function Abstract_Children_Count
     (Node : access Node_Rewriting_Handle_Type) return Natural is
   begin
      return
        (case Node.Children.Kind is
         when Unexpanded          => Node.Node.Abstract_Children_Count,
         when Expanded_Regular    => Natural (Node.Children.Vector.Length),
         when Expanded_Token_Node => 0);
   end Abstract_Children_Count;

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

   -----------------------------
   -- Abstract_Rewritten_Node --
   -----------------------------

   overriding function Abstract_Rewritten_Node
     (Node : access Node_Rewriting_Handle_Type) return ${root_node_type_name}
  is
   begin
      return Node.Node;
   end Abstract_Rewritten_Node;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return ${root_node_kind_name}
   is
   begin
      return Handle.Kind;
   end Kind;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Handle.Tied;
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Handle.Parent;
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      return Handle.Abstract_Children_Count;
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle is
   begin
      --  If this handle represents an already existing node, make sure it is
      --  expanded so we have a handle to return.
      Expand_Children (Handle);

      --  Only regular nodes can have fields. As Index is checked to be
      --  in-bounds in the pre-condition, we can assume here that the result
      --  exists.
      return Handle.Children.Vector.Element (Index);
   end Child;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Handle.Root;
   end Root;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      --  If this handle represents an already existing node, make sure it is
      --  expanded so that its children vector can be modified.
      Expand_Children (Handle);

      --  Only regular nodes can have fields. As Index is checked to be
      --  in-bounds in the pre-condition, we can assume here that we have an
      --  Expanded_Regular children record.
      declare
         Child_Slot : Node_Rewriting_Handle renames
            Handle.Children.Vector.Reference (Index);
      begin
         --  Untie the child to be replaced if it exists
         if Child_Slot /= No_Node_Rewriting_Handle then
            Child_Slot.Parent := No_Node_Rewriting_Handle;
            Child_Slot.Tied := False;
         end if;

         --  Tie the new child if it exists
         if Child /= No_Node_Rewriting_Handle then
            Child.Parent := Handle;
            Child.Tied := True;
         end if;

         Child_Slot := Child;
      end;
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      case Handle.Children.Kind is
         when Unexpanded =>
            return Text (Handle.Node);
         when Expanded_Regular =>
            return (raise Program_Error);
         when Expanded_Token_Node =>
            return To_Wide_Wide_String (Handle.Children.Text);
      end case;
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      --  Make sure Handle is expanded so we have a Text field to override
      Expand_Children (Handle);

      Handle.Children.Text := To_Unbounded_Wide_Wide_String (Text);
   end Set_Text;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      if Handle.Root /= No_Node_Rewriting_Handle then
         Handle.Root.Tied := False;
      end if;

      Handle.Root := Root;
      if Root /= No_Node_Rewriting_Handle then
         Root.Tied := True;
      end if;
   end Set_Root;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle) is
   begin
      --  First, just create room for the new node and let Set_Child take care
      --  of tiding Child to Handle's tree.
      Expand_Children (Handle);
      Handle.Children.Vector.Insert (Index, No_Node_Rewriting_Handle);
      Set_Child (Handle, Index, Child);
   end Insert_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle) is
   begin
      Insert_Child (Handle, Children_Count (Handle) + 1, Child);
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) is
   begin
      --  First, let Set_Child take care of untiding the child to remove, and
      --  then actually remove the corresponding children list slot.
      Set_Child (Handle, Index, No_Node_Rewriting_Handle);
      Handle.Children.Vector.Delete (Index);
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle;
   begin
      if Handle = No_Node_Rewriting_Handle then
         return Handle;
      end if;

      --  Make sure the original handle is expanded so we can iterate on it
      Expand_Children (Handle);

      Result := new Node_Rewriting_Handle_Type'
        (Context_Handle => Handle.Context_Handle,
         Node           => Handle.Node,
         Parent         => No_Node_Rewriting_Handle,
         Kind           => Handle.Kind,
         Tied           => False,
         Children       => <>);
      Nodes_Pools.Append (Handle.Context_Handle.New_Nodes, Result);

      --  Recursively clone children
      case Handle.Children.Kind is
         when Unexpanded =>
            raise Program_Error;

         when Expanded_Token_Node =>
            Result.Children := (Kind => Expanded_Token_Node,
                                Text => Handle.Children.Text);

         when Expanded_Regular =>
            Result.Children := (Kind => Expanded_Regular, Vector => <>);
            Result.Children.Vector.Reserve_Capacity
              (Handle.Children.Vector.Length);
            for I in 1 .. Handle.Children.Vector.Last_Index loop
               Result.Children.Vector.Append
                 (Clone (Handle.Children.Vector.Element (I)));
            end loop;
      end case;

      return Result;
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${root_node_kind_name}) return Node_Rewriting_Handle is
   begin
      if Is_Token_Node (Kind) then
         return Create_Token_Node (Handle, Kind, "");
      else
         declare
            Refs     : constant Field_Reference_Array := Fields (Kind);
            Children : constant Node_Rewriting_Handle_Array (Refs'Range) :=
               (others => No_Node_Rewriting_Handle);
         begin
            return Create_Regular_Node (Handle, Kind, Children);
         end;
      end if;
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${root_node_kind_name};
      Text   : Text_Type) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle := new Node_Rewriting_Handle_Type'
        (Context_Handle => Handle,
         Node           => null,
         Parent         => No_Node_Rewriting_Handle,
         Kind           => Kind,
         Tied           => False,
         Children       => (Kind => Expanded_Token_Node,
                            Text => To_Unbounded_Wide_Wide_String (Text)));
   begin
      Nodes_Pools.Append (Handle.New_Nodes, Result);
      return Result;
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${root_node_kind_name};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle := new Node_Rewriting_Handle_Type'
        (Context_Handle => Handle,
         Node           => null,
         Parent         => No_Node_Rewriting_Handle,
         Kind           => Kind,
         Tied           => False,
         Children       => (Kind   => Expanded_Regular,
                            Vector => <>));
   begin
      Result.Children.Vector.Reserve_Capacity (Children'Length);
      for C of Children loop
         Result.Children.Vector.Append (C);
         if C /= No_Node_Rewriting_Handle then
            C.Parent := Result;
            C.Tied := True;
         end if;
      end loop;
      Nodes_Pools.Append (Handle.New_Nodes, Result);
      return Result;
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle
   is
      type State_Type is (
         Default,
         --  Default state: no meta character being processed

         Open_Brace,
         --  The previous character is a open brace: the current one
         --  determines what it means.

         Close_Brace
         --  The previous character is a closing brace: the current one must be
         --  another closing brace.
      );

      Buffer   : Unbounded_Wide_Wide_String;
      State    : State_Type := Default;
      Next_Arg : Positive := Arguments'First;
   begin
      --  Interpret the template looping over its characters with a state
      --  machine.
      for C of Template loop
         case State is
         when Default =>
            case C is
            when '{' =>
               State := Open_Brace;
            when '}' =>
               State := Close_Brace;
            when others =>
               Append (Buffer, C);
            end case;

         when Open_Brace =>
            case C is
            when '{' =>
               State := Default;
               Append (Buffer, C);
            when '}' =>
               State := Default;
               if Next_Arg in Arguments'Range then
                  declare
                     Unparsed_Arg : constant Wide_Wide_String :=
                        Rewriting.Unparse (Arguments (Next_Arg));
                  begin
                     Next_Arg := Next_Arg + 1;
                     Append (Buffer, Unparsed_Arg);
                  end;
               else
                  raise Template_Args_Error with
                     "not enough arguments provided";
               end if;
            when others =>
               raise Template_Format_Error with
                  "standalone ""{"" character";
            end case;

         when Close_Brace =>
            case C is
            when '}' =>
               State := Default;
               Append (Buffer, C);
            when others =>
               raise Template_Format_Error with
                  "standalone ""}"" character";
            end case;
         end case;
      end loop;

      --  Make sure that there is no standalone metacharacter at the end of the
      --  template.
      case State is
         when Default => null;
         when Open_Brace =>
            raise Template_Format_Error with "standalone ""{"" character";
         when Close_Brace =>
            raise Template_Format_Error with "standalone ""}"" character";
      end case;

      --  Make sure all given arguments were consumed
      if Next_Arg in Arguments'Range then
         raise Template_Args_Error with "too many arguments provided";
      end if;

      --  Now parse the resulting buffer and create the corresponding tree of
      --  nodes.
      declare
         Context  : constant Analysis_Context := Rewriting.Context (Handle);
         Unit     : constant Analysis_Unit := Templates_Unit (Context);
         Reparsed : Reparsed_Unit;

         procedure Init_Parser
           (Unit     : Analysis_Unit;
            Read_BOM : Boolean;
            Parser   : in out Parser_Type);
         --  Callback for Do_Parsing

         function Transform
           (Node   : ${root_node_type_name};
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle;
         --  Turn a node from the Reparsed unit into a recursively expanded
         --  node rewriting handle.

         -----------------
         -- Init_Parser --
         -----------------

         procedure Init_Parser
           (Unit     : Analysis_Unit;
            Read_BOM : Boolean;
            Parser   : in out Parser_Type)
         is
            pragma Unreferenced (Read_BOM);
         begin
            Init_Parser_From_Buffer
              (To_Wide_Wide_String (Buffer),
               Unit, Token_Data (Unit),
               Analysis.Parsers.Symbol_Literal_Array_Access
                 (Symbol_Literals (Context)),
               With_Trivia => True, Parser => Parser);
         end Init_Parser;

         ---------------
         -- Transform --
         ---------------

         function Transform
           (Node   : ${root_node_type_name};
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle
         is
            Result : Node_Rewriting_Handle;
         begin
            if Node = null then
               return No_Node_Rewriting_Handle;
            end if;

            --  Allocate the handle for Node, and don't forget to remove the
            --  backlink to Node itself as it exists only temporarily for
            --  template instantiation.
            Result := Allocate (Node, Handle, No_Unit_Rewriting_Handle,
                                Parent);
            Result.Node := null;

            if Node.Is_Token_Node then
               declare
                  Index : constant Natural := Natural (Node.Token_Start_Index);
                  Data  : constant Lexer.Token_Data_Type :=
                     Reparsed.TDH.Tokens.Get (Index);
                  Text      : constant Text_Type := Reparsed.TDH.Source_Buffer
                    (Data.Source_First .. Data.Source_Last);
               begin
                  Result.Children :=
                    (Kind => Expanded_Token_Node,
                     Text => To_Unbounded_Wide_Wide_String (Text));
               end;

            else
               declare
                  Count : constant Natural := Node.Abstract_Children_Count;
               begin
                  Result.Children := (Kind => Expanded_Regular, Vector => <>);
                  Result.Children.Vector.Reserve_Capacity
                    (Ada.Containers.Count_Type (Count));
                  for I in 1 .. Count loop
                     Result.Children.Vector.Append
                       (Transform (Node.Child (I), Result));
                  end loop;
               end;
            end if;
            return Result;
         end Transform;

      begin
         Set_Rule (Unit, Rule);
         Do_Parsing (Unit, False, Init_Parser'Access, Reparsed);
         if not Reparsed.Diagnostics.Is_Empty then
            Destroy (Reparsed);
            raise Template_Instantiation_Error;
         end if;

         declare
            Result : constant Node_Rewriting_Handle :=
               Transform (Reparsed.AST_Root, No_Node_Rewriting_Handle);
         begin
            Destroy (Reparsed);
            return Result;
         end;
      end;
   end Create_From_Template;

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
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle, ${n.ada_kind_name},
               (${', '.join('{} => {}'.format(i, f.name)
                            for i, f in enumerate(n.get_parse_fields(), 1))}));
         end;

      % endif
   % endfor

end ${ada_lib_name}.Rewriting;
