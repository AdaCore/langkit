## vim: filetype=makoada

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common;         use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Lexer_Implementation;
use ${ada_lib_name}.Lexer_Implementation;

with ${ada_lib_name}.Unparsing_Implementation;
use ${ada_lib_name}.Unparsing_Implementation;

package body ${ada_lib_name}.Rewriting_Implementation is

   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Rewriting_Handle_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle_Pointer, Rewriting_Handle);

   procedure Pre_Check (Value : Boolean; Msg : String);
   --  Raise a Precondition_Failure exception with the given message
   --  if the Value is False.

   ---------------
   -- Pre_Check --
   ---------------

   procedure Pre_Check (Value : Boolean; Msg : String) is
   begin
      if not Value then
         raise Precondition_Failure with Msg;
      end if;
   end Pre_Check;

   <%def name="pre_check_rw_handle(handle)">
      ## Check that the given rewriting handle is not null
      Pre_Check
        (${handle} /= No_Rewriting_Handle,
         "${handle} should not be null");
   </%def>

   <%def name="pre_check_rw_no_handle(handle)">
      ## Check that given rewriting handle *is* null
      Pre_Check
        (${handle} = No_Rewriting_Handle,
         "${handle} must be null");
   </%def>

   <%def name="pre_check_urw_handle(handle)">
      ## Check that the given unit rewriting handle is not null
      Pre_Check
        (${handle} /= No_Unit_Rewriting_Handle,
         "${handle} should not be null");
   </%def>

   <%def name="pre_check_nrw_handle(handle)">
      ## Check that the given node rewriting handle is not null
      Pre_Check
        (${handle} /= No_Node_Rewriting_Handle,
         "${handle} should not be null");
   </%def>

   <%def name="pre_check_child_index(handle, index, for_insertion)">
      <%
      upper_bound = "Children_Count ({}){}".format(
         handle,
         " + 1" if for_insertion else ""
      )
      %>
      ## Check that `index` is in range 1 .. `upper_bound`, where
      ## `upper_bound` is equal to the number of children of `handle`
      ## if `for_insertion` is false, or to that number + 1 if it is true.
      ##
      ## Note: the call to Pre_Check is inlined so that the construction
      ## of the exception message happens only if the check fails.
      if ${index} > ${upper_bound} then
         raise Precondition_Failure
            with "Invalid index " & ${index}'Image & ": ${handle} has " &
                  Children_Count (${handle})'Image & " children";
      end if;
   </%def>

   <%def name="pre_check_unit_no_diags(unit)">
      ## Check that the given analysis unit does not have any diagnostics
      Pre_Check
        (not Has_Diagnostics (${unit}),
         "${unit} must not have diagnostics");
   </%def>

   <%def name="pre_check_is_token_kind(kind)">
      ## Check that the given node kind corresponds to a token node kind
      Pre_Check
        (Is_Token_Node (${kind}),
         "Expected a token node. Got " & ${kind}'Image);
   </%def>

   <%def name="pre_check_is_not_token_kind(kind)">
      ## Check that the given node kind *does not* correspond to a token node
      ## kind.
      Pre_Check
        (not Is_Token_Node (${kind}),
         "Expected a token node. Got " & ${kind}'Image);
   </%def>

   <%def name="pre_check_is_not_error_kind(kind)">
      ## Check that the given node kind corresponds to a token node kind
      Pre_Check
        (not Is_Error_Node (${kind}),
         "Expected a non-error node. Got " & ${kind}'Image);
   </%def>

   <%def name="pre_check_is_list_kind(kind)">
      ## Check that the given node kind corresponds to a list node kind
      Pre_Check
        (Is_List_Node (${kind}),
         "Expected a list node. Got " & ${kind}'Image);
   </%def>

   <%def name="pre_check_null_or_untied(handle)">
      ## Check that the given node handle is not tied to any unit rewriting
      ## handle.
      ## Note: No_Node_Rewriting_Handle matches this predicate.
      Pre_Check
        (${handle} = No_Node_Rewriting_Handle or else not Tied (${handle}),
         "${handle} must not be tied to another rewriting context.");
   </%def>

   <%def name="pre_check_is_tied(handle)">
      ## Check that the given node rewriting handle is well tied to an analysis
      ## unit.
      Pre_Check
        (Tied (${handle}),
         "${handle} must be tied to an analysis unit.");
   </%def>

   <%def name="pre_check_null_or_valid_context(node, handle)">
      ## Check that the given node rewriting handle `node` is either null, or is
      ## associated to the given rewriting handle `handle`.
      Pre_Check
        (${node} = No_Node_Rewriting_Handle
            or else Context (${node}) = ${handle},
         "${node} should be associated to rewriting context ${handle}.");
   </%def>

   function Handle (Context : Internal_Context) return Rewriting_Handle is
     (Convert (Get_Rewriting_Handle (Context)));

   function Context (Handle : Rewriting_Handle) return Internal_Context is
   begin
      ${pre_check_rw_handle('Handle')}
      return Handle.Context;
   end Context;

   function Allocate
     (Kind          : ${T.node_kind};
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

   function Allocate
     (Node          : ${T.root_node.name};
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

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle;
      Unit           : Unit_Rewriting_Handle);
   --  Tie the node represented by handle so that either:
   --
   --    * it is the root of Unit (Parent is null);
   --    * it is a child of Parent (Unit is null).
   --
   --  Do nothing if Handle is null.

   procedure Untie (Handle : Node_Rewriting_Handle);
   --  Untie the node represented by Handle. Do nothing if Handle is null.

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle is
   begin
      ${pre_check_rw_no_handle ('Handle (Context)')}

      if Context.File_Reader /= null then
         raise Precondition_Failure with
            "tree rewriting forbidden with a file reader";
      end if;

      declare
         Result : constant Rewriting_Handle := new Rewriting_Handle_Type'
           (Context   => Context,
            Units     => <>,
            Pool      => Create,
            New_Nodes => <>);
      begin
         Result.New_Nodes := Nodes_Pools.Create (Result.Pool);
         Set_Rewriting_Handle (Context, Convert (Result));
         return Result;
      end;
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
   begin
      ${pre_check_rw_handle('Handle')}
      Free_Handles (Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is

      type Processed_Unit_Record is record
         Unit     : Internal_Unit;
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
      ${pre_check_rw_handle('Handle')}

      --  Try to reparse all units that were potentially modified
      for Unit_Handle of Handle.Units loop
         declare
            PU    : constant Processed_Unit := new Processed_Unit_Record'
              (Unit     => Unit_Handle.Unit,
               New_Data => <>);
            Input : Internal_Lexer_Input :=
              (Kind        => Bytes_Buffer,
               Charset     => <>,
               Read_BOM    => False,
               Bytes       => System.Null_Address,
               Bytes_Count => 0);
            Bytes : String_Access;

            function Error_Result return Apply_Result
            is ((Success => False, Unit => PU.Unit, Diagnostics => <>));
         begin
            Units.Append (PU);

            --  Reparse (i.e. unparse and then parse) this rewritten unit
            begin
               Bytes := Unparse
                 (Create_Abstract_Node (Unit_Handle.Root),
                  PU.Unit,
                  Preserve_Formatting => True,
                  As_Unit             => True);
            exception
               when Exc : Malformed_Tree_Error =>
                  Result := Error_Result;
                  Append
                    (Result.Diagnostics,
                     No_Source_Location_Range,
                     To_Text (Exception_Message (Exc)));
                  exit;
            end;
            Input.Charset := Unit_Handle.Unit.Charset;
            Input.Bytes := Bytes.all'Address;
            Input.Bytes_Count := Bytes.all'Length;
            Do_Parsing (PU.Unit, Input, PU.New_Data);
            Free (Bytes);

            --  If there is a parsing error, abort the rewriting process
            if not PU.New_Data.Diagnostics.Is_Empty then
               Result := Error_Result;
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
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array is
   begin
      ${pre_check_rw_handle('Handle')}

      declare
         Count  : constant Natural := Natural (Handle.Units.Length);
         Result : Unit_Rewriting_Handle_Array (1 .. Count);
         I      : Positive := 1;
      begin
         for Unit of Handle.Units loop
            Result (I) := Unit;
            I := I + 1;
         end loop;
         return Result;
      end;
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle is
   begin
      ${pre_check_rw_handle('Handle (Context (Unit))')}
      ${pre_check_unit_no_diags('Unit')}

      declare
         use Unit_Maps;

         Context        : constant Internal_Context := Unit.Context;
         Context_Handle : constant Rewriting_Handle := Handle (Context);
         Filename       : constant Unbounded_String :=
            To_Unbounded_String (Get_Filename (Unit));

         Cur : constant Cursor := Context_Handle.Units.Find (Filename);
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
      end;
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit
   is
   begin
      ${pre_check_urw_handle('Handle')}
      return Handle.Unit;
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      ${pre_check_urw_handle('Handle')}
      return Handle.Root;
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      ${pre_check_urw_handle('Handle')}
      ${pre_check_null_or_untied('Root')}

      Untie (Handle.Root);
      Handle.Root := Root;
      Tie (Root, No_Node_Rewriting_Handle, Handle);
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      ${pre_check_urw_handle('Handle')}
      return Unparsing_Implementation.Unparse
        (Node                => Create_Abstract_Node (Handle.Root),
         Unit                => Handle.Unit,
         Preserve_Formatting => True,
         As_Unit             => True);
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle (Node : ${T.root_node.name}) return Node_Rewriting_Handle is
   begin
      ${pre_check_rw_handle('Handle (Context (Node.Unit))')}
      ${pre_check_unit_no_diags('Node.Unit')}

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

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return ${T.root_node.name} is
   begin
      ${pre_check_nrw_handle('Handle')}
      return Handle.Node;
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      ${pre_check_nrw_handle('Handle')}
      return Handle.Context_Handle;
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      ${pre_check_nrw_handle('Handle')}
      return To_Wide_Wide_String
        (Unparsing_Implementation.Unparse
           (Create_Abstract_Node (Handle),
            Unit                => null,
            Preserve_Formatting => True,
            As_Unit             => False));
   end Unparse;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Kind          : ${T.node_kind};
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      Tied : constant Boolean := Unit_Handle /= No_Unit_Rewriting_Handle;
   begin
      return new Node_Rewriting_Handle_Type'
        (Context_Handle => Context,
         Node           => null,
         Parent         => Parent_Handle,
         Kind           => Kind,
         Tied           => Tied,
         Root_Of        =>
           (if Tied and then Parent_Handle = No_Node_Rewriting_Handle
            then Unit_Handle
            else No_Unit_Rewriting_Handle),
         Children       => Unexpanded_Children);
   end Allocate;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Node          : ${T.root_node.name};
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      Result : constant Node_Rewriting_Handle := Allocate
        (Node.Kind, Context, Unit_Handle, Parent_Handle);
   begin
      Result.Node := Node;
      if Result.Tied then
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
         N           : constant ${T.root_node.name} := Node.Node;
         Unit_Handle : constant Unit_Rewriting_Handle :=
            Handle (N.Unit);
      begin
         if Is_Token_Node (N) then
            Children := (Kind => Expanded_Token_Node,
                         Text => To_Unbounded_Wide_Wide_String (Text (N)));

         else
            Children := (Kind => Expanded_Regular, Vector => <>);
            declare
               Count : constant Natural := Children_Count (N);
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  declare
                     Child : constant ${T.root_node.name} :=
                        Implementation.Child (N, I);
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

      Ctx : constant Internal_Context := Context (Handle);
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

   ---------
   -- Tie --
   ---------

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle;
      Unit           : Unit_Rewriting_Handle) is
   begin
      if Handle /= No_Node_Rewriting_Handle then
         Handle.Parent := Parent;
         Handle.Tied := True;
         if Parent = No_Node_Rewriting_Handle then
            Handle.Root_Of := Unit;
         end if;
      end if;
   end Tie;

   -----------
   -- Untie --
   -----------

   procedure Untie (Handle : Node_Rewriting_Handle) is
   begin
      if Handle /= No_Node_Rewriting_Handle then
         Handle.Parent := No_Node_Rewriting_Handle;
         Handle.Tied := False;
         Handle.Root_Of := No_Unit_Rewriting_Handle;
      end if;
   end Untie;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return ${T.node_kind} is
   begin
      ${pre_check_nrw_handle('Handle')}
      return Handle.Kind;
   end Kind;

   -----------
   -- Image --
   -----------

   function Image (Handle : Node_Rewriting_Handle) return String is
   begin
      if Handle = No_Node_Rewriting_Handle then
         return "None";
      end if;

      declare
         Tied_Suffix : constant String :=
           (if Tied (Handle) then " (tied)" else "");
      begin
         if Node (Handle) = null then
            declare
               K          : constant ${T.node_kind} := Kind (Handle);
               Tok_Suffix : constant String :=
                 (if Is_Token_Node (K)
                  then " " & Image (Text (Handle), With_Quotes => True)
                  else "");
            begin
               return "<" & K'Image & Tok_Suffix & Tied_Suffix & ">";
            end;
         else
            declare
               Ent : constant ${T.entity.name} :=
                 (Node => Node (Handle),
                  Info => No_Entity_Info);
               Img : constant String := Image (Ent);
            begin
               return Img (Img'First .. Img'Last - 1) & Tied_Suffix & ">";
            end;
         end if;
      end;
   end Image;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      ${pre_check_nrw_handle('Handle')}
      return Handle.Tied;
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      ${pre_check_nrw_handle('Handle')}
      return Handle.Parent;
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      ${pre_check_nrw_handle('Handle')}
      return
        (case Handle.Children.Kind is
         when Unexpanded          => Children_Count (Handle.Node),
         when Expanded_Regular    => Natural (Handle.Children.Vector.Length),
         when Expanded_Token_Node => 0);
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle is
   begin
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_child_index('Handle', 'Index', for_insertion=False)}

      --  If this handle represents an already existing node, make sure it is
      --  expanded so we have a handle to return.
      Expand_Children (Handle);
      return Handle.Children.Vector.Element (Index);
   end Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_child_index('Handle', 'Index', for_insertion=False)}
      ${pre_check_null_or_untied('Child')}

      --  If this handle represents an already existing node, make sure it is
      --  expanded so that its children vector can be modified.
      Expand_Children (Handle);

      declare
         Child_Slot : Node_Rewriting_Handle renames
            Handle.Children.Vector.Reference (Index);
      begin
         --  Untie the child to be replaced if it exists
         Untie (Child_Slot);

         --  Tie the new child if it exists
         Tie (Child, Handle, No_Unit_Rewriting_Handle);

         Child_Slot := Child;
      end;
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_is_token_kind('Kind (Handle)')}

      case Handle.Children.Kind is
         when Unexpanded =>
            if Is_Token_Node (Handle.Kind) then
               return Text (Handle.Node);
            else
               raise Program_Error;
            end if;
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
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_is_token_kind('Kind (Handle)')}

      --  Make sure Handle is expanded so we have a Text field to override
      Expand_Children (Handle);

      Handle.Children.Text := To_Unbounded_Wide_Wide_String (Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_is_tied('Handle')}
      ${pre_check_null_or_untied('New_Node')}

      if Handle = New_Node then
         return;
      end if;

      if Handle.Root_Of = No_Unit_Rewriting_Handle then
         --  If Handle is not the root node of its owning unit, go replace it
         --  in its parent's children list.
         declare
            Parent : Node_Rewriting_Handle renames Handle.Parent;
            Index  : Natural := 0;
         begin
            for I in 1 .. Children_Count (Parent) loop
               if Child (Parent, I) = Handle then
                  Index := I;
                  exit;
               end if;
            end loop;
            pragma Assert (Index > 0);
            Set_Child (Parent, Index, New_Node);
         end;

      else
         --  Otherwise, replace it as a root node
         Set_Root (Handle.Root_Of, New_Node);
      end if;
   end Replace;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle) is
   begin
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_is_list_kind('Kind (Handle)')}
      ${pre_check_child_index('Handle', 'Index', for_insertion=True)}
      ${pre_check_null_or_untied('Child')}

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
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_is_list_kind('Kind (Handle)')}
      ${pre_check_null_or_untied('Child')}

      Insert_Child (Handle, Children_Count (Handle) + 1, Child);
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) is
   begin
      ${pre_check_nrw_handle('Handle')}
      ${pre_check_is_list_kind('Kind (Handle)')}
      ${pre_check_child_index('Handle', 'Index', for_insertion=True)}

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

      --  If the input handle is associated to a node, so should be the cloned
      --  handle, so that its formatting is copied as well.
      Result :=
        (if Handle.Node = null
         then Allocate (Handle.Kind, Handle.Context_Handle,
                        No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle)
         else Allocate (Handle.Node, Handle.Context_Handle,
                        No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle));
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
      Kind   : ${T.node_kind}) return Node_Rewriting_Handle is
   begin
      ${pre_check_rw_handle('Handle')}
      ${pre_check_is_not_error_kind('Kind')}

      if Is_Token_Node (Kind) then
         return Create_Token_Node (Handle, Kind, "");
      else
         declare
            Count    : constant Integer := Kind_To_Node_Children_Count (Kind);
            Children : constant Node_Rewriting_Handle_Array (1 ..  Count) :=
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
      Kind   : ${T.node_kind};
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      ${pre_check_rw_handle('Handle')}
      ${pre_check_is_token_kind('Kind')}

      declare
         Result : constant Node_Rewriting_Handle := Allocate
           (Kind, Handle, No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle);
      begin
         Result.Children := (Kind => Expanded_Token_Node,
                             Text => To_Unbounded_Wide_Wide_String (Text));
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${T.node_kind};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle is
   begin
      ${pre_check_rw_handle('Handle')}
      ${pre_check_is_not_token_kind('Kind')}
      ${pre_check_is_not_error_kind('Kind')}
      for One_Child of Children loop
         ${pre_check_null_or_untied('One_Child')}
      end loop;

      declare
         Result : Node_Rewriting_Handle := Allocate
           (Kind, Handle, No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle);
      begin
         Result.Children := (Kind   => Expanded_Regular,
                             Vector => <>);
         Result.Children.Vector.Reserve_Capacity (Children'Length);
         for C of Children loop
            Result.Children.Vector.Append (C);
            if C /= No_Node_Rewriting_Handle then
               Tie (C, Result, No_Unit_Rewriting_Handle);
            end if;
         end loop;
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
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
      for One_Argument of Arguments loop
         ${pre_check_null_or_valid_context('One_Argument', 'Handle')}
      end loop;

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
                        Unparse (Arguments (Next_Arg));
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
         Context  : constant Internal_Context :=
           Rewriting_Implementation.Context (Handle);
         Unit     : constant Internal_Unit := Templates_Unit (Context);
         Reparsed : Reparsed_Unit;
         Text     : constant Text_Type := To_Wide_Wide_String (Buffer);
         Input    : constant Internal_Lexer_Input :=
           (Kind       => Text_Buffer,
            Text       => Text'Address,
            Text_Count => Text'Length);

         function Transform
           (Node   : ${T.root_node.name};
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle;
         --  Turn a node from the Reparsed unit into a recursively expanded
         --  node rewriting handle.

         ---------------
         -- Transform --
         ---------------

         function Transform
           (Node   : ${T.root_node.name};
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle
         is
            Result : Node_Rewriting_Handle;
         begin
            if Node = null then
               return No_Node_Rewriting_Handle;
            end if;

            --  Allocate the handle for Node, and don't forget to remove the
            --  backlink to Node itself as it exists only temporarily for
            --  template instantiation. Also, track the newly allocated node
            --  so that it is freed correctly upon destruction of the
            --  rewriting context.
            Result := Allocate (Node, Handle, No_Unit_Rewriting_Handle,
                                Parent);
            Result.Node := null;
            Nodes_Pools.Append (Handle.New_Nodes, Result);

            if Is_Token_Node (Node) then
               declare
                  Index : constant Natural := Natural (Node.Token_Start_Index);
                  Data  : constant Stored_Token_Data :=
                     Reparsed.TDH.Tokens.Get (Index);
                  Text  : constant Text_Type := Reparsed.TDH.Source_Buffer
                    (Data.Source_First .. Data.Source_Last);
               begin
                  Result.Children :=
                    (Kind => Expanded_Token_Node,
                     Text => To_Unbounded_Wide_Wide_String (Text));
               end;

            else
               declare
                  Count : constant Natural := Children_Count (Node);
               begin
                  Result.Children := (Kind => Expanded_Regular, Vector => <>);
                  Result.Children.Vector.Reserve_Capacity
                    (Ada.Containers.Count_Type (Count));
                  for I in 1 .. Count loop
                     Result.Children.Vector.Append
                       (Transform (Child (Node, I), Result));
                  end loop;
               end;
            end if;
            return Result;
         end Transform;

      begin
         Set_Rule (Unit, Rule);
         Do_Parsing (Unit, Input, Reparsed);
         if not Reparsed.Diagnostics.Is_Empty then
            Destroy (Reparsed);
            raise Template_Instantiation_Error;
         end if;

         declare
            Result : constant Node_Rewriting_Handle :=
               Transform (Reparsed.Ast_Root, No_Node_Rewriting_Handle);
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
            ${pre_check_rw_handle('Handle')}

            return Create_Regular_Node
              (Handle, ${n.ada_kind_name},
               (${', '.join('{} => {}'.format(i, f.name)
                            for i, f in enumerate(n.get_parse_fields(), 1))}));
         end;

      % endif
   % endfor
end ${ada_lib_name}.Rewriting_Implementation;
