
with Ada.Containers.Hashed_Sets;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;
with System.Memory;

with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Hashes;
with Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Slocs;       use Liblktlang_Support.Slocs;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;
with Liblktlang_Support.Types;       use Liblktlang_Support.Types;

with Liblktlang.Common;       use Liblktlang.Common;
with Liblktlang.Generic_API;  use Liblktlang.Generic_API;
with Liblktlang.Generic_API.Introspection;
use Liblktlang.Generic_API.Introspection;
with Liblktlang.Implementation;
with Liblktlang.Lexer_Implementation;
use Liblktlang.Lexer_Implementation;
with Liblktlang.Generic_Impl; use Liblktlang.Generic_Impl;
with Liblktlang.Unparsing_Implementation;
use Liblktlang.Unparsing_Implementation;

package body Liblktlang.Rewriting_Implementation is

   use type System.Memory.size_t;

   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Rewriting_Handle_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle_Pointer, Rewriting_Handle);

   function Hash is new Liblktlang_Support.Hashes.Hash_Access
     (Node_Rewriting_Handle_Type, Node_Rewriting_Handle);
   package NRH_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Node_Rewriting_Handle,
      Equivalent_Elements => "=",
      Hash                => Hash);

   procedure Pre_Check (Value : Boolean; Msg : String);
   --  Raise a Precondition_Failure exception with the given message
   --  if the Value is False.

   function Parent_Is_List (Node : Node_Rewriting_Handle) return Boolean
   is (Node.Parent /= null and then Node.Parent.Children.Kind = Expanded_List);
   --  Return whether Node's parent is a list node.
   --
   --  Since a tied and non-root node rewriting handle can exist only when its
   --  parent is expanded, it is safe checking the parent's Children field.

   function Index_For
     (Handle : Node_Rewriting_Handle;
      Member : Struct_Member_Ref) return Positive;
   --  Return the 1-based index of the ``Member`` parse field in the node
   --  referenced ``Handle``. Raise a ``Precondition_Failure`` if there is no
   --  such member.

   function Index_In_Parent_List
     (Handle : Node_Rewriting_Handle) return Positive;
   --  Assuming ``Handle`` is a node whose parent is a list node, return its
   --  1-based index in that list node.

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  Assign ``Child`` to the child slot at the given ``Index`` in ``Handle``

   ---------------
   -- Pre_Check --
   ---------------

   procedure Pre_Check (Value : Boolean; Msg : String) is
   begin
      if not Value then
         raise Precondition_Failure with Msg;
      end if;
   end Pre_Check;

   ---------------
   -- Index_For --
   ---------------

   function Index_For
     (Handle : Node_Rewriting_Handle;
      Member : Struct_Member_Ref) return Positive
   is
      Result : Positive := 1;
   begin
      if Language (Owner (Member)) /= Self_Id then
         raise Precondition_Failure with "invalid member language";
      elsif Is_Property (Member) then
         raise Precondition_Failure with "got property, parse field expected";
      end if;

      declare
         T            : constant Type_Ref := Kind_To_Type (Handle.Kind);
         Node_Members : constant Struct_Member_Ref_Array := Members (T);
      begin
         if Is_Null_For (Member, T) then
            raise Precondition_Failure
              with "invalid reference to the node rewriting handle of a null"
                   & " field";
         end if;
         for M of Node_Members loop
            if M = Member then
               return Result;
            elsif not Is_Property (M) and not Is_Null_For (M, T) then
               Result := Result + 1;
            end if;
         end loop;
      end;

      return (raise Precondition_Failure with "no such member on this node");
   end Index_For;

   --------------------------
   -- Index_In_Parent_List --
   --------------------------

   function Index_In_Parent_List
     (Handle : Node_Rewriting_Handle) return Positive
   is
      Parent : constant Node_Rewriting_Handle := Handle.Parent;
   begin
      for I in 1 .. Natural (Parent.Children.Vector.Length) loop
         if Parent.Children.Vector.Element (I) = Handle then
            return I;
         end if;
      end loop;

      --  We should not be able to reach this point, as ``Handle`` should
      --  always be present in the list of children of ``Handle.Parent``
      --  (otherwise the tree of node rewriting handles is corrupted).

      return (raise Program_Error);
   end Index_In_Parent_List;

   

   

   

   

   

   

   

   

   

   

   

   

   

   

   function Handle (Context : Internal_Context) return Rewriting_Handle is
     (Convert (Get_Rewriting_Handle (Context)));

   function Context (Handle : Rewriting_Handle) return Internal_Context is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Context;
   end Context;

   function Allocate
     (Kind          : Lkt_Node_Kind_Type;
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
     (Node          : Bare_Lkt_Node;
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

   function Allocate_Stub
     (Context : Rewriting_Handle) return Node_Rewriting_Handle;
   --  Allocate a stub rewriting node in ``Context``, to be used as a temporary
   --  node in ``Rotate``.

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

   -------------------------
   -- C_Context_To_Handle --
   -------------------------

   function C_Context_To_Handle
     (Context : Internal_Context) return Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Handle (Context);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Context_To_Handle;

   -------------------------
   -- C_Handle_To_Context --
   -------------------------

   function C_Handle_To_Context
     (Handle : Rewriting_Handle) return Internal_Context is
   begin
      Clear_Last_Exception;
      return Context (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Handle_To_Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle (Context) = No_Rewriting_Handle,
         "Handle (Context) must be null");
   

      declare
         Result : constant Rewriting_Handle := new Rewriting_Handle_Type'
           (Context   => Context,
            Units     => <>,
            Pool      => Create,
            New_Nodes => <>,
            Stubs     => <>);
      begin
         Result.New_Nodes := Nodes_Pools.Create (Result.Pool);
         Result.Stubs := Nodes_Pools.Create (Result.Pool);
         Set_Rewriting_Handle (Context, Convert (Result));
         return Result;
      end;
   end Start_Rewriting;

   -----------------------
   -- C_Start_Rewriting --
   -----------------------

   function C_Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Start_Rewriting (Context);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      Free_Handles (Handle);
   end Abort_Rewriting;

   -----------------------
   -- C_Abort_Rewriting --
   -----------------------

   procedure C_Abort_Rewriting (Handle : Rewriting_Handle) is
      H : Rewriting_Handle := Handle;
   begin
      Clear_Last_Exception;
      Abort_Rewriting (H);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Abort_Rewriting;

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
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

      --  Try to reparse all units that were potentially modified
      for Unit_Handle of Handle.Units loop
         declare
            PU    : constant Processed_Unit := new Processed_Unit_Record'
              (Unit     => Unit_Handle.Unit,
               New_Data => <>);
            Input : Liblktlang_Support.Internal.Analysis.Lexer_Input :=
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
            if PU.New_Data.Present
               and then not PU.New_Data.Diagnostics.Is_Empty
            then
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

   -------------
   -- C_Apply --
   -------------

   procedure C_Apply
     (Handle : Rewriting_Handle;
      Result : access C_Apply_Result)
   is
      H          : Rewriting_Handle := Handle;
      Ada_Result : Apply_Result;
   begin
      Clear_Last_Exception;

      Ada_Result := Apply (H);
      Result.Success := (if Ada_Result.Success then 1 else 0);
      if not Ada_Result.Success then
         Result.Unit := Ada_Result.Unit;
         if Ada_Result.Diagnostics.Is_Empty then
            Result.Diagnostics_Count := 0;
            Result.Diagnostics := null;
         else
            Result.Diagnostics_Count :=
              Interfaces.C.int (Ada_Result.Diagnostics.Length);
            declare
               type Array_Type is
                 array (1 .. Natural (Result.Diagnostics_Count))
                 of lkt_diagnostic;
               Diagnostics_Address : constant System.Address :=
                 System.Memory.Alloc (Array_Type'Size / 8);
               Diagnostics         : Array_Type
                 with Import, Address => Diagnostics_Address;
            begin
               Result.Diagnostics :=
                 C_Diagnostic_Array.To_Pointer (Diagnostics_Address);
               for I in Diagnostics'Range loop
                  declare
                     D : Diagnostic renames Ada_Result.Diagnostics (I);
                  begin
                     Diagnostics (I).Sloc_Range := Wrap (D.Sloc_Range);
                     Diagnostics (I).Message :=
                       Wrap_Alloc (To_Text (D.Message));
                  end;
               end loop;
            end;
         end if;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Apply;

   -----------------------
   -- Free_Apply_Result --
   -----------------------

   procedure Free_Apply_Result (Result : access C_Apply_Result) is
      use type Interfaces.C.int;
   begin
      Clear_Last_Exception;

      if Result.Success = 0 then
         if Result.Diagnostics_Count /= 0 then
            declare
               type Array_Type is
                 array (1 .. Natural (Result.Diagnostics_Count))
                 of lkt_diagnostic;
               Diagnostics_Address : constant System.Address :=
                 (C_Diagnostic_Array.To_Address (Result.Diagnostics));
               Diagnostics         : Array_Type
                 with Import, Address => Diagnostics_Address;
            begin
               for D of Diagnostics loop
                  lkt_destroy_text
                    (D.Message'Unrestricted_Access);
               end loop;
            end;
            System.Memory.Free
              (C_Diagnostic_Array.To_Address (Result.Diagnostics));
         end if;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end Free_Apply_Result;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

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

   --------------------
   -- C_Unit_Handles --
   --------------------

   function C_Unit_Handles
     (Handle : Rewriting_Handle) return C_Unit_Array.Object_Pointer is
   begin
      Clear_Last_Exception;

      declare
         Units : constant Unit_Rewriting_Handle_Array := Unit_Handles (Handle);
         type Array_Type is
           array (Units'First .. Units'Last + 1)
           of Unit_Rewriting_Handle;
         Result_Address : constant System.Address :=
           System.Memory.Alloc (Array_Type'Size / 8);
         Result         : Array_Type
           with Import, Address => Result_Address;
      begin
         for I in Units'Range loop
            Result (I) := Units (I);
         end loop;
         Result (Result'Last) := null;
         return C_Unit_Array.To_Pointer (Result_Address);
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle (Context (Unit)) /= No_Rewriting_Handle,
         "Handle (Context (Unit)) should not be null");
   
      
      Pre_Check
        (not Has_Diagnostics (Unit),
         "Unit must not have diagnostics");
   

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
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Unit;
   end Unit;

   ----------------------
   -- C_Unit_To_Handle --
   ----------------------

   function C_Unit_To_Handle
     (Unit : Internal_Unit) return Unit_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Handle (Unit);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Unit_To_Handle;

   ----------------------
   -- C_Handle_To_Unit --
   ----------------------

   function C_Handle_To_Unit
     (Handle : Unit_Rewriting_Handle) return Internal_Unit is
   begin
      Clear_Last_Exception;
      return Unit (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Handle_To_Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Root;
   end Root;

   ------------
   -- C_Root --
   ------------

   function C_Root
     (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Root (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Root = No_Node_Rewriting_Handle or else not Tied (Root),
         "Root must not be tied to another rewriting context.");
   

      Untie (Handle.Root);
      Handle.Root := Root;
      Tie (Root, No_Node_Rewriting_Handle, Handle);
   end Set_Root;

   ----------------
   -- C_Set_Root --
   ----------------

   procedure C_Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Set_Root (Handle, Root);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      return Unparsing_Implementation.Unparse
        (Node                => Create_Abstract_Node (Handle.Root),
         Unit                => Handle.Unit,
         Preserve_Formatting => True,
         As_Unit             => True);
   end Unparse;

   ---------------
   -- C_Unparse --
   ---------------

   procedure C_Unparse
     (Handle : Unit_Rewriting_Handle; Result : access lkt_text)
   is
      Text : Unbounded_Text_Type;
   begin
      Clear_Last_Exception;
      Text := Unparse (Handle);
      Result.all := Wrap_Alloc (Text);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Unparse;

   ------------
   -- Handle --
   ------------

   function Handle (Node : Bare_Lkt_Node) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle (Context (Node.Unit)) /= No_Rewriting_Handle,
         "Handle (Context (Node.Unit)) should not be null");
   
      
      Pre_Check
        (not Has_Diagnostics (Node.Unit),
         "Node.Unit must not have diagnostics");
   

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

   ----------------------
   -- C_Node_To_Handle --
   ----------------------

   function C_Node_To_Handle (Node : lkt_base_node) return Node_Rewriting_Handle
   is
      N : Bare_Lkt_Node;
   begin
      Clear_Last_Exception;
      N := Unwrap (Node);
      return Handle (N);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Node_To_Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return Bare_Lkt_Node is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Node;
   end Node;

   ----------------------
   -- C_Handle_To_Node --
   ----------------------

   function C_Handle_To_Node
     (Handle : Node_Rewriting_Handle) return lkt_base_node
   is
      N : Bare_Lkt_Node;
   begin
      Clear_Last_Exception;
      N := Node (Handle);
      return Wrap (N);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return Wrap (null);
   end C_Handle_To_Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Context_Handle;
   end Context;

   -----------------------
   -- C_Node_To_Context --
   -----------------------

   function C_Node_To_Context
     (Node : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Context (Node);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Node_To_Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return To_Wide_Wide_String
        (Unparsing_Implementation.Unparse
           (Create_Abstract_Node (Handle),
            Unit                => null,
            Preserve_Formatting => True,
            As_Unit             => False));
   end Unparse;

   ---------------
   -- C_Unparse --
   ---------------

   procedure C_Unparse
     (Handle : Node_Rewriting_Handle; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         Text : constant Text_Type := Unparse (Handle);
      begin
         Result.all := Wrap_Alloc (Text);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Unparse;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Kind          : Lkt_Node_Kind_Type;
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
         Previous       => No_Node_Rewriting_Handle,
         Next           => No_Node_Rewriting_Handle,
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
     (Node          : Bare_Lkt_Node;
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

   -------------------
   -- Allocate_Stub --
   -------------------

   function Allocate_Stub
     (Context : Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Allocate
        (Kind          => Lkt_Node_Kind_Type'First,
         Context       => Context,
         Unit_Handle   => No_Unit_Rewriting_Handle,
         Parent_Handle => No_Node_Rewriting_Handle);
   end Allocate_Stub;

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

      --  Otherwise, expand to the appropriate children form

      declare
         N           : constant Bare_Lkt_Node := Node.Node;
         Unit_Handle : constant Unit_Rewriting_Handle :=
            Handle (N.Unit);

         function Allocate_Child
           (Child : Bare_Lkt_Node) return Node_Rewriting_Handle
         is (if Child = null
             then No_Node_Rewriting_Handle
             else Allocate
                   (Child, Unit_Handle.Context_Handle, Unit_Handle, Node));
      begin
         if Is_Token_Node (N) then

            --  N is a token node: its expanded form contains only its text

            Children := (Kind => Expanded_Token_Node,
                         Text => To_Unbounded_Wide_Wide_String (Text (N)));

         elsif Is_List_Node (N.Kind) then

            --  N is a list node: its expanded form contains a doubly linked
            --  list for its children.

            declare
               Count                : constant Natural := Children_Count (N);
               First, Last, Current : Node_Rewriting_Handle :=
                 No_Node_Rewriting_Handle;
            begin
               for I in 1 .. Count loop
                  Current := Allocate_Child (Implementation.Child (N, I));
                  if First = No_Node_Rewriting_Handle then
                     First := Current;
                     Last := Current;
                  else
                     Last.Next := Current;
                     Current.Previous := Last;
                     Last := Current;
                  end if;
               end loop;
               Children :=
                 (Kind  => Expanded_List,
                  First => First,
                  Last  => Last,
                  Count => Count);
            end;

         else
            --  N is a regular node: its expanded form contains a vector for
            --  all non-null syntax fields.

            Children := (Kind => Expanded_Regular, Vector => <>);
            declare
               Count : constant Natural := Children_Count (N);
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  Children.Vector.Append
                    (Allocate_Child (Implementation.Child (N, I)));
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
      for Node of Handle.Stubs loop
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
         Handle.Previous := No_Node_Rewriting_Handle;
         Handle.Next := No_Node_Rewriting_Handle;
         Handle.Tied := False;
         Handle.Root_Of := No_Unit_Rewriting_Handle;
      end if;
   end Untie;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return Lkt_Node_Kind_Type is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Kind;
   end Kind;

   ------------
   -- C_Kind --
   ------------

   function C_Kind (Handle : Node_Rewriting_Handle) return lkt_node_kind_enum is
   begin
      Clear_Last_Exception;
      declare
         K : constant Lkt_Node_Kind_Type := Kind (Handle);
      begin
         return lkt_node_kind_enum (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end C_Kind;

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
               K          : constant Lkt_Node_Kind_Type := Kind (Handle);
               Tok_Suffix : constant String :=
                 (if Is_Token_Node (K)
                  then " " & Image (Text (Handle), With_Quotes => True)
                  else "");
            begin
               return "<" & K'Image & Tok_Suffix & Tied_Suffix & ">";
            end;
         else
            declare
               Ent : constant Internal_Entity :=
                 (Node => Node (Handle),
                  Info => No_Entity_Info);
               Img : constant String := Image (Ent);
            begin
               return Img (Img'First .. Img'Last - 1) & Tied_Suffix & ">";
            end;
         end if;
      end;
   end Image;

   -------------
   -- C_Image --
   -------------

   procedure C_Image
     (Handle : Node_Rewriting_Handle; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := To_Text (Image (Handle));
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Image;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Tied;
   end Tied;

   ------------
   -- C_Tied --
   ------------

   function C_Tied (Handle : Node_Rewriting_Handle) return Interfaces.C.int is
   begin
      Clear_Last_Exception;
      return (if Tied (Handle) then 1 else 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end C_Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Parent;
   end Parent;

   --------------
   -- C_Parent --
   --------------

   function C_Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Parent (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return
        (case Handle.Children.Kind is
         when Unexpanded          => Children_Count (Handle.Node),
         when Expanded_Regular    => Natural (Handle.Children.Vector.Length),
         when Expanded_List       => Handle.Children.Count,
         when Expanded_Token_Node => 0);
   end Children_Count;

   ----------------------
   -- C_Children_Count --
   ----------------------

   function C_Children_Count
     (Handle : Node_Rewriting_Handle) return Interfaces.C.int is
   begin
      Clear_Last_Exception;
      return Interfaces.C.int (Children_Count (Handle));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end C_Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle
   is
      Index : Positive;
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   

      Index := Index_For (Handle, Field);

      --  If this handle represents an already existing node, make sure it is
      --  expanded so we have a handle to return.
      Expand_Children (Handle);
      return Handle.Children.Vector.Element (Index);
   end Child;

   -------------
   -- C_Child --
   -------------

   function C_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Interfaces.C.int) return Node_Rewriting_Handle
   is
      F : Struct_Member_Ref;
   begin
      Clear_Last_Exception;
      F := From_Index (Self_Id, Struct_Member_Index (Field));
      return Child (Handle, F);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Child;

   --------------
   -- Children --
   --------------

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array
   is
      I : Positive := 1;
      N : Node_Rewriting_Handle;
   begin
      return Result : Node_Rewriting_Handle_Array
                        (1 .. Children_Count (Handle))
      do
         if Is_List_Node (Handle) then
            N := First_Child (Handle);
            while N /= No_Node_Rewriting_Handle loop
               Result (I) := N;
               I := I + 1;
               N := Next_Child (N);
            end loop;
         else
            declare
               T            : constant Type_Ref :=
                 Kind_To_Type (Kind (Handle));
               Node_Members : constant Struct_Member_Ref_Array := Members (T);
            begin
               for M of Node_Members loop
                  if Is_Field (M) and then not Is_Null_For (M, T) then
                     Result (I) := Child (Handle, M);
                     I := I + 1;
                  end if;
               end loop;
            end;
         end if;
      end return;
   end Children;

   ----------------
   -- C_Children --
   ----------------

   procedure C_Children
     (Handle   : Node_Rewriting_Handle;
      Children : access C_Node_Array.Object_Pointer;
      Count    : access Interfaces.C.int) is
   begin
      Clear_Last_Exception;

      declare
         Result : constant Node_Rewriting_Handle_Array :=
           Rewriting_Implementation.Children (Handle);

         subtype Array_Type is Node_Rewriting_Handle_Array (Result'Range);
         Children_Address : constant System.Address :=
           System.Memory.Alloc (Array_Type'Size / 8);
         Ada_Children     : Array_Type
           with Import, Address => Children_Address;
      begin
         Ada_Children := Result;
         Children.all := C_Node_Array.To_Pointer (Children_Address);
         Count.all := Result'Length;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Children;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle) is
   begin
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

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");
   

      Set_Child (Handle, Index_For (Handle, Field), Child);
   end Set_Child;

   -----------------
   -- C_Set_Child --
   -----------------

   procedure C_Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Interfaces.C.int;
      Child  : Node_Rewriting_Handle)
   is
      F : Struct_Member_Ref;
   begin
      Clear_Last_Exception;
      F := From_Index (Self_Id, Struct_Member_Index (Field));
      Set_Child (Handle, F, Child);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_Token_Node (Kind (Handle)),
         "Expected a token node. Got " & Kind (Handle)'Image);
   

      case Handle.Children.Kind is
         when Unexpanded =>
            if Is_Token_Node (Handle.Kind) then
               return Text (Handle.Node);
            else
               raise Program_Error;
            end if;
         when Expanded_Regular | Expanded_List =>
            return (raise Program_Error);
         when Expanded_Token_Node =>
            return To_Wide_Wide_String (Handle.Children.Text);
      end case;
   end Text;

   ------------
   -- C_Text --
   ------------

   procedure C_Text
     (Handle : Node_Rewriting_Handle; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         T : constant Text_Type := Text (Handle);
      begin
         Result.all := Wrap_Alloc (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_Token_Node (Kind (Handle)),
         "Expected a token node. Got " & Kind (Handle)'Image);
   

      --  Make sure Handle is expanded so we have a Text field to override
      Expand_Children (Handle);

      Handle.Children.Text := To_Unbounded_Wide_Wide_String (Text);
   end Set_Text;

   ----------------
   -- C_Set_Text --
   ----------------

   procedure C_Set_Text
     (Handle : Node_Rewriting_Handle; Text : access lkt_text)
   is
      T : constant Text_Type (1 .. Natural (Text.Length))
      with Import, Address => Text.Chars;
   begin
      Clear_Last_Exception;
      Set_Text (Handle, T);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
      Parent : Node_Rewriting_Handle;
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Tied (Handle),
         "Handle must be tied to an analysis unit.");
   
      
      Pre_Check
        (New_Node = No_Node_Rewriting_Handle or else not Tied (New_Node),
         "New_Node must not be tied to another rewriting context.");
   

      if Handle = New_Node then
         return;
      end if;

      if Handle.Root_Of = No_Unit_Rewriting_Handle then

         --  If Handle is not the root node of its owning unit, go replace it
         --  in its parent's children list.

         if Parent_Is_List (Handle) then
            if New_Node = No_Node_Rewriting_Handle then
               Remove_Child (Handle);
               return;
            end if;

            Parent := Handle.Parent;
            if Handle.Previous = No_Node_Rewriting_Handle then
               Parent.Children.First := New_Node;
            else
               Handle.Previous.Next := New_Node;
            end if;
            if Handle.Next = No_Node_Rewriting_Handle then
               Parent.Children.Last := New_Node;
            else
               Handle.Next.Previous := New_Node;
            end if;
            Tie (New_Node, Parent, No_Unit_Rewriting_Handle);
            New_Node.Previous := Handle.Previous;
            New_Node.Next := Handle.Next;
            Untie (Handle);
         else
            Set_Child (Handle.Parent, Index_In_Parent_List (Handle), New_Node);
         end if;

      else
         --  Otherwise, replace it as a root node
         Set_Root (Handle.Root_Of, New_Node);
      end if;
   end Replace;

   ---------------
   -- C_Replace --
   ---------------

   procedure C_Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Replace (Handle, New_Node);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Replace;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Handles : Node_Rewriting_Handle_Array) is

      function Non_Null_Tied (Handle : Node_Rewriting_Handle) return Boolean
      is (Handle /= No_Node_Rewriting_Handle and then Tied (Handle));

      RH : Rewriting_Handle := No_Rewriting_Handle;
   begin
      --  Rotate is a no-op if there are less than two handles or none is tied

      if Handles'Length < 2 then
         return;
      end if;

      for H of Handles loop
         if Non_Null_Tied (H) then
            RH := H.Context_Handle;
            exit;
         end if;
      end loop;
      if RH = No_Rewriting_Handle then
         return;
      end if;

      --  Check that each non-null handle is present at most once in the input
      --  list.

      declare
         Handle_Set : NRH_Sets.Set;
      begin
         for H of Handles loop
            if H /= No_Node_Rewriting_Handle then
               begin
                  Handle_Set.Insert (H);
               exception
                  when Constraint_Error =>
                     raise Precondition_Failure with
                       "non-null handles can be present at most once";
               end;
            end if;
         end loop;
      end;

      --  Now that inputs are validated, we can start the rotation. As a
      --  reminder: replace H1 by H2, H2 by H3, ... Or in other words: put H2
      --  in the location of H1, put H3 in the location of H1.

      declare
         Stubs       : array (Handles'Range) of Node_Rewriting_Handle;
         Stub_Cursor : Nodes_Pools.Cursor := Nodes_Pools.First (RH.Stubs);
      begin
         --  First create stubs to replace the nodes that will be replaced with
         --  other nodes.

         for I in Handles'Range loop
            declare
               Node : constant Node_Rewriting_Handle := Handles (I);
               Repl : constant Node_Rewriting_Handle :=
                 Handles (if I = Handles'Last then Handles'First else I + 1);
               Stub : Node_Rewriting_Handle renames Stubs (I);
            begin
               --  Get a stub if needed. If an already allocated stub is
               --  available, use it, otherwise allocate one.

               if Non_Null_Tied (Node) and then Non_Null_Tied (Repl) then
                  if Nodes_Pools.Has_Element (RH.Stubs, Stub_Cursor) then
                     Stub := Nodes_Pools.Get (RH.Stubs, Stub_Cursor);
                     Stub_Cursor := Nodes_Pools.Next (RH.Stubs, Stub_Cursor);
                  else
                     Stub := Allocate_Stub (RH);
                     Nodes_Pools.Append (RH.Stubs, Stub);
                  end if;
               else
                  Stub := No_Node_Rewriting_Handle;
               end if;
            end;
         end loop;

         --  Now replace nodes with their stubs. We have to do this as separate
         --  pass because the replacement affects the "Tied" predicate that is
         --  used in the first pass.

         for I in Handles'Range loop
            if Handles (I) /= No_Node_Rewriting_Handle then
               Replace (Handles (I), Stubs (I));
            end if;
         end loop;

         --  Now that all the nodes to rotate are untied, replace stubs with
         --  the definitive nodes.

         for I in Handles'Range loop
            declare
               Stub : constant Node_Rewriting_Handle := Stubs (I);
               Repl : Node_Rewriting_Handle;
            begin
               if Stub /= No_Node_Rewriting_Handle then
                  Repl := Handles
                    (if I = Handles'Last then Handles'First else I + 1);
                  Replace (Stub, Repl);
               end if;
            end;
         end loop;
      end;
   end Rotate;

   --------------
   -- C_Rotate --
   --------------

   procedure C_Rotate
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int) is
   begin
      Clear_Last_Exception;

      declare
         subtype Array_Type is
           Node_Rewriting_Handle_Array (1 .. Natural (Count));
         Handles_Address : constant System.Address :=
           C_Node_Array.To_Address (Handles);
         Ada_Handles     : constant Array_Type
           with Import, Address => Handles_Address;
      begin
         Rotate (Ada_Handles);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Rotate;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Is_List_Node (Kind (Handle));
   end Is_List_Node;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   

      Expand_Children (Handle);
      return Handle.Children.First;
   end First_Child;

   -------------------
   -- C_First_Child --
   -------------------

   function C_First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return First_Child (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   

      Expand_Children (Handle);
      return Handle.Children.Last;
   end Last_Child;

   ------------------
   -- C_Last_Child --
   ------------------

   function C_Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Last_Child (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Last_Child;

   ----------------
   -- Next_Child --
   ----------------

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Handle.Parent /= No_Node_Rewriting_Handle,
         "Handle.Parent should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle.Parent)),
         "Expected a list node. Got " & Kind (Handle.Parent)'Image);
   

      return Handle.Next;
   end Next_Child;

   ------------------
   -- C_Next_Child --
   ------------------

   function C_Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      Clear_Last_Exception;
      return Next_Child (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Next_Child;

   --------------------
   -- Previous_Child --
   --------------------

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Handle.Parent /= No_Node_Rewriting_Handle,
         "Handle.Parent should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle.Parent)),
         "Expected a list node. Got " & Kind (Handle.Parent)'Image);
   

      return Handle.Previous;
   end Previous_Child;

   ----------------------
   -- C_Previous_Child --
   ----------------------

   function C_Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      Clear_Last_Exception;
      return Previous_Child (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Previous_Child;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle) is
      Old_Previous, Parent : Node_Rewriting_Handle;
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      Parent := Handle.Parent;
      
      Pre_Check
        (Parent /= No_Node_Rewriting_Handle,
         "Parent should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Parent)),
         "Expected a list node. Got " & Kind (Parent)'Image);
   
      
      Pre_Check
        (New_Sibling = No_Node_Rewriting_Handle or else not Tied (New_Sibling),
         "New_Sibling must not be tied to another rewriting context.");
   

      Old_Previous := Handle.Previous;
      if Old_Previous = No_Node_Rewriting_Handle then
         Handle.Parent.Children.First := New_Sibling;
      else
         Old_Previous.Next := New_Sibling;
      end if;
      New_Sibling.Previous := Old_Previous;
      New_Sibling.Next := Handle;
      Handle.Previous := New_Sibling;
      Tie (New_Sibling, Parent, No_Unit_Rewriting_Handle);
      Parent.Children.Count := Parent.Children.Count + 1;
   end Insert_Before;

   ---------------------
   -- C_Insert_Before --
   ---------------------

   procedure C_Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Insert_Before (Handle, New_Sibling);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After (Handle, New_Sibling : Node_Rewriting_Handle) is
      Old_Next, Parent : Node_Rewriting_Handle;
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      Parent := Handle.Parent;
      
      Pre_Check
        (Parent /= No_Node_Rewriting_Handle,
         "Parent should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Parent)),
         "Expected a list node. Got " & Kind (Parent)'Image);
   
      
      Pre_Check
        (New_Sibling = No_Node_Rewriting_Handle or else not Tied (New_Sibling),
         "New_Sibling must not be tied to another rewriting context.");
   

      Old_Next := Handle.Next;
      if Old_Next = No_Node_Rewriting_Handle then
         Handle.Parent.Children.Last := New_Sibling;
      else
         Old_Next.Previous := New_Sibling;
      end if;
      New_Sibling.Next := Old_Next;
      New_Sibling.Previous := Handle;
      Handle.Next := New_Sibling;
      Tie (New_Sibling, Parent, No_Unit_Rewriting_Handle);
      Parent.Children.Count := Parent.Children.Count + 1;
   end Insert_After;

   --------------------
   -- C_Insert_After --
   --------------------

   procedure C_Insert_After (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Insert_After (Handle, New_Sibling);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_After;

   ------------------
   -- Insert_First --
   ------------------

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   
      
      Pre_Check
        (New_Child = No_Node_Rewriting_Handle or else not Tied (New_Child),
         "New_Child must not be tied to another rewriting context.");
   

      Expand_Children (Handle);
      if Handle.Children.First /= No_Node_Rewriting_Handle then
         Handle.Children.First.Previous := New_Child;
         New_Child.Next := Handle.Children.First;
      end if;
      Handle.Children.First := New_Child;
      if Handle.Children.Last = No_Node_Rewriting_Handle then
         Handle.Children.Last := New_Child;
      end if;
      Tie (New_Child, Handle, No_Unit_Rewriting_Handle);
      Handle.Children.Count := Handle.Children.Count + 1;
   end Insert_First;

   --------------------
   -- C_Insert_First --
   --------------------

   procedure C_Insert_First (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Insert_First (Handle, New_Sibling);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_First;

   -----------------
   -- Insert_Last --
   -----------------

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   
      
      Pre_Check
        (New_Child = No_Node_Rewriting_Handle or else not Tied (New_Child),
         "New_Child must not be tied to another rewriting context.");
   

      Expand_Children (Handle);
      if Handle.Children.Last /= No_Node_Rewriting_Handle then
         Handle.Children.Last.Next := New_Child;
         New_Child.Previous := Handle.Children.Last;
      end if;
      Handle.Children.Last := New_Child;
      if Handle.Children.First = No_Node_Rewriting_Handle then
         Handle.Children.First := New_Child;
      end if;
      Tie (New_Child, Handle, No_Unit_Rewriting_Handle);
      Handle.Children.Count := Handle.Children.Count + 1;
   end Insert_Last;

   -------------------
   -- C_Insert_Last --
   -------------------

   procedure C_Insert_Last (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Insert_Last (Handle, New_Sibling);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_Last;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle) is
      Parent : Node_Rewriting_Handle;
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Handle.Parent /= No_Node_Rewriting_Handle,
         "Handle.Parent should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle.Parent)),
         "Expected a list node. Got " & Kind (Handle.Parent)'Image);
   

      Expand_Children (Handle);
      Parent := Handle.Parent;
      if Parent.Children.First = Handle then
         Parent.Children.First := Handle.Next;
         if Handle.Next = No_Node_Rewriting_Handle then
            Parent.Children.Last := No_Node_Rewriting_Handle;
         else
            Handle.Next.Previous := No_Node_Rewriting_Handle;
         end if;
      elsif Parent.Children.Last = Handle then
         Parent.Children.Last := Handle.Previous;
         Parent.Children.Last.Next := No_Node_Rewriting_Handle;
      else
         Handle.Previous.Next := Handle.Next;
         Handle.Next.Previous := Handle.Previous;
      end if;
      Untie (Handle);
      Parent.Children.Count := Parent.Children.Count - 1;
   end Remove_Child;

   --------------------
   -- C_Remove_Child --
   --------------------

   procedure C_Remove_Child (Handle : Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      Remove_Child (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Remove_Child;

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

         when Expanded_List =>
            declare
               First, Last, Cloned : Node_Rewriting_Handle :=
                 No_Node_Rewriting_Handle;
               Current             : Node_Rewriting_Handle :=
                 Handle.Children.First;
            begin
               while Current /= No_Node_Rewriting_Handle loop
                  Cloned := Clone (Current);
                  Tie (Cloned, Result, No_Unit_Rewriting_Handle);
                  if First = No_Node_Rewriting_Handle then
                     First := Cloned;
                     Last := Cloned;
                  else
                     Last.Next := Cloned;
                     Cloned.Previous := Last;
                     Last := Cloned;
                  end if;
                  Current := Next_Child (Current);
               end loop;
               Result.Children :=
                 (Kind  => Expanded_List,
                  First => First,
                  Last  => Last,
                  Count => Handle.Children.Count);
            end;

         when Expanded_Regular =>
            Result.Children := (Kind => Expanded_Regular, Vector => <>);
            Result.Children.Vector.Reserve_Capacity
              (Handle.Children.Vector.Length);
            for I in 1 .. Handle.Children.Vector.Last_Index loop
               declare
                  Child : constant Node_Rewriting_Handle :=
                    Clone (Handle.Children.Vector.Element (I));
               begin
                  Tie (Child, Result, No_Unit_Rewriting_Handle);
                  Result.Children.Vector.Append (Child);
               end;
            end loop;
      end case;

      return Result;
   end Clone;

   -------------
   -- C_Clone --
   -------------

   function C_Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return Clone (Handle);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (not Is_Error_Node (Kind),
         "Expected a non-error node. Got " & Kind'Image);
   

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

   -------------------
   -- C_Create_Node --
   -------------------

   function C_Create_Node
     (Handle : Rewriting_Handle;
      Kind   : lkt_node_kind_enum) return Node_Rewriting_Handle
   is
      K : Lkt_Node_Kind_Type;
   begin
      Clear_Last_Exception;
      K := Lkt_Node_Kind_Type'Enum_Val (Kind);
      return Create_Node (Handle, K);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_Token_Node (Kind),
         "Expected a token node. Got " & Kind'Image);
   

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
   -- C_Create_Token_Node --
   -------------------------

   function C_Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : lkt_node_kind_enum;
      Text   : access lkt_text) return Node_Rewriting_Handle
   is
      K : Lkt_Node_Kind_Type;
      T : constant Text_Type (1 .. Natural (Text.Length))
      with Import, Address => Text.Chars;
   begin
      Clear_Last_Exception;
      K := Lkt_Node_Kind_Type'Enum_Val (Kind);
      return Create_Token_Node (Handle, K, T);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Lkt_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
   is
      List : Boolean;
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (not Is_Token_Node (Kind),
         "Expected a token node. Got " & Kind'Image);
   
      
      Pre_Check
        (not Is_Error_Node (Kind),
         "Expected a non-error node. Got " & Kind'Image);
   
      List := Is_List_Node (Kind);
      for One_Child of Children loop
         if List then
            
      Pre_Check
        (One_Child /= No_Node_Rewriting_Handle and then not Tied (One_Child),
         "One_Child must not be tied to another rewriting context.");
   
         else
            
      Pre_Check
        (One_Child = No_Node_Rewriting_Handle or else not Tied (One_Child),
         "One_Child must not be tied to another rewriting context.");
   
         end if;
      end loop;

      declare
         Result : Node_Rewriting_Handle := Allocate
           (Kind, Handle, No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle);
      begin
         if List then
            declare
               First, Last : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
            begin
               for C of Children loop
                  if First = No_Node_Rewriting_Handle then
                     First := C;
                  else
                     Last.Next := C;
                     C.Previous := Last;
                  end if;
                  Last := C;
                  Tie (C, Result, No_Unit_Rewriting_Handle);
               end loop;
               Result.Children :=
                 (Kind  => Expanded_List,
                  First => First,
                  Last  => Last,
                  Count => Children'Length);
            end;
         else
            Result.Children := (Kind   => Expanded_Regular,
                                Vector => <>);
            Result.Children.Vector.Reserve_Capacity (Children'Length);
            for C of Children loop
               Result.Children.Vector.Append (C);
               if C /= No_Node_Rewriting_Handle then
                  Tie (C, Result, No_Unit_Rewriting_Handle);
               end if;
            end loop;
         end if;
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
   end Create_Regular_Node;

   ---------------------------
   -- C_Create_Regular_Node --
   ---------------------------

   function C_Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : lkt_node_kind_enum;
      Children : C_Node_Array.Object_Pointer;
      Count    : Interfaces.C.int) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      declare
         K : constant Lkt_Node_Kind_Type := Lkt_Node_Kind_Type'Enum_Val (Kind);

         subtype Array_Type is
           Node_Rewriting_Handle_Array (1 .. Natural (Count));
         Children_Address : constant System.Address :=
           C_Node_Array.To_Address (Children);
         Ada_Children     : constant Array_Type
           with Import, Address => Children_Address;
      begin
         return Create_Regular_Node (Handle, K, Ada_Children);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Create_Regular_Node;

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
         
      Pre_Check
        (One_Argument = No_Node_Rewriting_Handle
            or else Context (One_Argument) = Handle,
         "One_Argument should be associated to rewriting context Handle.");
   
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
         Input    : constant Liblktlang_Support.Internal.Analysis.Lexer_Input :=
           (Kind       => Text_Buffer,
            Text       => Text'Address,
            Text_Count => Text'Length);

         function Transform
           (Node   : Bare_Lkt_Node;
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle;
         --  Turn a node from the Reparsed unit into a recursively expanded
         --  node rewriting handle.

         ---------------
         -- Transform --
         ---------------

         function Transform
           (Node   : Bare_Lkt_Node;
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

            elsif Is_List_Node (Node.Kind) then
               declare
                  Count                : constant Natural :=
                    Children_Count (Node);
                  First, Last, Current : Node_Rewriting_Handle :=
                    No_Node_Rewriting_Handle;
               begin
                  for I in 1 .. Count loop
                     Current :=
                       Transform (Implementation.Child (Node, I), Result);
                     if First = No_Node_Rewriting_Handle then
                        First := Current;
                        Last := Current;
                     else
                        Last.Next := Current;
                        Current.Previous := Last;
                        Last := Current;
                     end if;
                     Tie (Current, Result, No_Unit_Rewriting_Handle);
                  end loop;
                  Result.Children :=
                    (Kind  => Expanded_List,
                     First => First,
                     Last  => Last,
                     Count => Count);
               end;

            else
               declare
                  Count : constant Natural := Children_Count (Node);
               begin
                  Result.Children := (Kind => Expanded_Regular, Vector => <>);
                  Result.Children.Vector.Reserve_Capacity
                    (Ada.Containers.Count_Type (Count));
                  for I in 1 .. Count loop
                     declare
                        C : constant Node_Rewriting_Handle :=
                          Transform (Child (Node, I), Result);
                     begin
                        Tie (C, Result, No_Unit_Rewriting_Handle);
                        Result.Children.Vector.Append (C);
                     end;
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
               Transform (+Reparsed.Ast_Root, No_Node_Rewriting_Handle);
         begin
            Destroy (Reparsed);
            return Result;
         end;
      end;
   end Create_From_Template;

   ----------------------------
   -- C_Create_From_Template --
   ----------------------------

   function C_Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : access lkt_text;
      Arguments : C_Node_Array.Object_Pointer;
      Count     : Interfaces.C.int;
      Rule      : lkt_grammar_rule) return Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      declare
         Ada_Template : constant Text_Type (1 .. Natural (Template.Length))
         with Import, Address => Template.Chars;

         subtype Array_Type is
           Node_Rewriting_Handle_Array (1 .. Natural (Count));
         Arguments_Address : constant System.Address :=
           C_Node_Array.To_Address (Arguments);
         Ada_Arguments     : constant Array_Type
           with Import, Address => Arguments_Address;
      begin
         return Create_From_Template
           (Handle, Ada_Template, Ada_Arguments, Rule);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Create_From_Template;


         function Create_Argument
           (Handle : Rewriting_Handle
               ; Argument_F_Name : Node_Rewriting_Handle
               ; Argument_F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Argument,
                 (1 => Argument_F_Name,
                  2 => Argument_F_Value));
         end;


         function Create_Lexer_Case_Rule_Cond_Alt
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : Node_Rewriting_Handle
               ; Lexer_Case_Rule_Cond_Alt_F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lexer_Case_Rule_Cond_Alt,
                 (1 => Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs,
                  2 => Lexer_Case_Rule_Cond_Alt_F_Send));
         end;


         function Create_Lexer_Case_Rule_Default_Alt
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_Default_Alt_F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lexer_Case_Rule_Default_Alt,
                 (1 => Lexer_Case_Rule_Default_Alt_F_Send));
         end;


         function Create_Grammar_Rule_Decl
           (Handle : Rewriting_Handle
               ; Grammar_Rule_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Grammar_Rule_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Rule_Decl,
                 (1 => Grammar_Rule_Decl_F_Syn_Name,
                  2 => Grammar_Rule_Decl_F_Expr));
         end;


         function Create_Synthetic_Lexer_Decl
           (Handle : Rewriting_Handle
               ; Synthetic_Lexer_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Synthetic_Lexer_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Synthetic_Lexer_Decl,
                 (1 => Synthetic_Lexer_Decl_F_Syn_Name,
                  2 => Synthetic_Lexer_Decl_F_Expr));
         end;


         function Create_Node_Decl
           (Handle : Rewriting_Handle
               ; Node_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Node_Decl,
                 (1 => Node_Decl_F_Syn_Name));
         end;


         function Create_Self_Decl
           (Handle : Rewriting_Handle
               ; Self_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Self_Decl,
                 (1 => Self_Decl_F_Syn_Name));
         end;


         function Create_Enum_Lit_Decl
           (Handle : Rewriting_Handle
               ; Enum_Lit_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Enum_Lit_Decl,
                 (1 => Enum_Lit_Decl_F_Syn_Name));
         end;


         function Create_Field_Decl
           (Handle : Rewriting_Handle
               ; Field_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Field_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Field_Decl_F_Trait_Ref : Node_Rewriting_Handle
               ; Field_Decl_F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Field_Decl,
                 (1 => Field_Decl_F_Syn_Name,
                  2 => Field_Decl_F_Decl_Type,
                  3 => Field_Decl_F_Trait_Ref,
                  4 => Field_Decl_F_Default_Val));
         end;


         function Create_Fun_Param_Decl
           (Handle : Rewriting_Handle
               ; Fun_Param_Decl_F_Decl_Annotations : Node_Rewriting_Handle
               ; Fun_Param_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Fun_Param_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Fun_Param_Decl_F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Fun_Param_Decl,
                 (1 => Fun_Param_Decl_F_Decl_Annotations,
                  2 => Fun_Param_Decl_F_Syn_Name,
                  3 => Fun_Param_Decl_F_Decl_Type,
                  4 => Fun_Param_Decl_F_Default_Val));
         end;


         function Create_Lambda_Param_Decl
           (Handle : Rewriting_Handle
               ; Lambda_Param_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Lambda_Param_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Lambda_Param_Decl_F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lambda_Param_Decl,
                 (1 => Lambda_Param_Decl_F_Syn_Name,
                  2 => Lambda_Param_Decl_F_Decl_Type,
                  3 => Lambda_Param_Decl_F_Default_Val));
         end;


         function Create_Dyn_Var_Decl
           (Handle : Rewriting_Handle
               ; Dyn_Var_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Dyn_Var_Decl_F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Dyn_Var_Decl,
                 (1 => Dyn_Var_Decl_F_Syn_Name,
                  2 => Dyn_Var_Decl_F_Decl_Type));
         end;


         function Create_Match_Val_Decl
           (Handle : Rewriting_Handle
               ; Match_Val_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Match_Val_Decl_F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Match_Val_Decl,
                 (1 => Match_Val_Decl_F_Syn_Name,
                  2 => Match_Val_Decl_F_Decl_Type));
         end;


         function Create_Val_Decl
           (Handle : Rewriting_Handle
               ; Val_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Val_Decl_F_Decl_Type : Node_Rewriting_Handle
               ; Val_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Val_Decl,
                 (1 => Val_Decl_F_Syn_Name,
                  2 => Val_Decl_F_Decl_Type,
                  3 => Val_Decl_F_Expr));
         end;


         function Create_Fun_Decl
           (Handle : Rewriting_Handle
               ; Fun_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Fun_Decl_F_Params : Node_Rewriting_Handle
               ; Fun_Decl_F_Return_Type : Node_Rewriting_Handle
               ; Fun_Decl_F_Trait_Ref : Node_Rewriting_Handle
               ; Fun_Decl_F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Fun_Decl,
                 (1 => Fun_Decl_F_Syn_Name,
                  2 => Fun_Decl_F_Params,
                  3 => Fun_Decl_F_Return_Type,
                  4 => Fun_Decl_F_Trait_Ref,
                  5 => Fun_Decl_F_Body));
         end;


         function Create_Env_Spec_Decl
           (Handle : Rewriting_Handle
               ; Env_Spec_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Env_Spec_Decl_F_Actions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Env_Spec_Decl,
                 (1 => Env_Spec_Decl_F_Syn_Name,
                  2 => Env_Spec_Decl_F_Actions));
         end;


         function Create_Generic_Decl
           (Handle : Rewriting_Handle
               ; Generic_Decl_F_Generic_Param_Decls : Node_Rewriting_Handle
               ; Generic_Decl_F_Decl : Node_Rewriting_Handle
               ; Generic_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Generic_Decl,
                 (1 => Generic_Decl_F_Generic_Param_Decls,
                  2 => Generic_Decl_F_Decl,
                  3 => Generic_Decl_F_Syn_Name));
         end;


         function Create_Grammar_Decl
           (Handle : Rewriting_Handle
               ; Grammar_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Grammar_Decl_F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Decl,
                 (1 => Grammar_Decl_F_Syn_Name,
                  2 => Grammar_Decl_F_Rules));
         end;


         function Create_Lexer_Decl
           (Handle : Rewriting_Handle
               ; Lexer_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Lexer_Decl_F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lexer_Decl,
                 (1 => Lexer_Decl_F_Syn_Name,
                  2 => Lexer_Decl_F_Rules));
         end;


         function Create_Lexer_Family_Decl
           (Handle : Rewriting_Handle
               ; Lexer_Family_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Lexer_Family_Decl_F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lexer_Family_Decl,
                 (1 => Lexer_Family_Decl_F_Syn_Name,
                  2 => Lexer_Family_Decl_F_Rules));
         end;


         function Create_Synth_Fun_Decl
           (Handle : Rewriting_Handle
               ; Synth_Fun_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Synth_Fun_Decl,
                 (1 => Synth_Fun_Decl_F_Syn_Name));
         end;


         function Create_Synth_Param_Decl
           (Handle : Rewriting_Handle
               ; Synth_Param_Decl_F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Synth_Param_Decl,
                 (1 => Synth_Param_Decl_F_Syn_Name));
         end;


         function Create_Any_Type_Decl
           (Handle : Rewriting_Handle
               ; Any_Type_Decl_F_Traits : Node_Rewriting_Handle
               ; Any_Type_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Any_Type_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Any_Type_Decl,
                 (1 => Any_Type_Decl_F_Traits,
                  2 => Any_Type_Decl_F_Syn_Name,
                  3 => Any_Type_Decl_F_Syn_Base_Type));
         end;


         function Create_Enum_Class_Alt_Decl
           (Handle : Rewriting_Handle
               ; Enum_Class_Alt_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Enum_Class_Alt_Decl_F_Traits : Node_Rewriting_Handle
               ; Enum_Class_Alt_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Enum_Class_Alt_Decl,
                 (1 => Enum_Class_Alt_Decl_F_Syn_Name,
                  2 => Enum_Class_Alt_Decl_F_Traits,
                  3 => Enum_Class_Alt_Decl_F_Syn_Base_Type));
         end;


         function Create_Function_Type
           (Handle : Rewriting_Handle
               ; Function_Type_F_Syn_Name : Node_Rewriting_Handle
               ; Function_Type_F_Traits : Node_Rewriting_Handle
               ; Function_Type_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Function_Type,
                 (1 => Function_Type_F_Syn_Name,
                  2 => Function_Type_F_Traits,
                  3 => Function_Type_F_Syn_Base_Type));
         end;


         function Create_Generic_Param_Type_Decl
           (Handle : Rewriting_Handle
               ; Generic_Param_Type_Decl_F_Has_Class : Node_Rewriting_Handle
               ; Generic_Param_Type_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Generic_Param_Type_Decl_F_Traits : Node_Rewriting_Handle
               ; Generic_Param_Type_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Generic_Param_Type_Decl,
                 (1 => Generic_Param_Type_Decl_F_Has_Class,
                  2 => Generic_Param_Type_Decl_F_Syn_Name,
                  3 => Generic_Param_Type_Decl_F_Traits,
                  4 => Generic_Param_Type_Decl_F_Syn_Base_Type));
         end;


         function Create_Class_Decl
           (Handle : Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Traits : Node_Rewriting_Handle
               ; Class_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Class_Decl,
                 (1 => Basic_Class_Decl_F_Syn_Name,
                  2 => Basic_Class_Decl_F_Syn_Base_Type,
                  3 => Basic_Class_Decl_F_Traits,
                  4 => Class_Decl_F_Decls));
         end;


         function Create_Enum_Class_Decl
           (Handle : Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Basic_Class_Decl_F_Traits : Node_Rewriting_Handle
               ; Enum_Class_Decl_F_Branches : Node_Rewriting_Handle
               ; Enum_Class_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Enum_Class_Decl,
                 (1 => Basic_Class_Decl_F_Syn_Name,
                  2 => Basic_Class_Decl_F_Syn_Base_Type,
                  3 => Basic_Class_Decl_F_Traits,
                  4 => Enum_Class_Decl_F_Branches,
                  5 => Enum_Class_Decl_F_Decls));
         end;


         function Create_Enum_Type_Decl
           (Handle : Rewriting_Handle
               ; Enum_Type_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Traits : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Literals : Node_Rewriting_Handle
               ; Enum_Type_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Enum_Type_Decl,
                 (1 => Enum_Type_Decl_F_Syn_Name,
                  2 => Enum_Type_Decl_F_Traits,
                  3 => Enum_Type_Decl_F_Syn_Base_Type,
                  4 => Enum_Type_Decl_F_Literals,
                  5 => Enum_Type_Decl_F_Decls));
         end;


         function Create_Struct_Decl
           (Handle : Rewriting_Handle
               ; Struct_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Struct_Decl_F_Traits : Node_Rewriting_Handle
               ; Struct_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Struct_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Struct_Decl,
                 (1 => Struct_Decl_F_Syn_Name,
                  2 => Struct_Decl_F_Traits,
                  3 => Struct_Decl_F_Syn_Base_Type,
                  4 => Struct_Decl_F_Decls));
         end;


         function Create_Trait_Decl
           (Handle : Rewriting_Handle
               ; Trait_Decl_F_Syn_Name : Node_Rewriting_Handle
               ; Trait_Decl_F_Traits : Node_Rewriting_Handle
               ; Trait_Decl_F_Syn_Base_Type : Node_Rewriting_Handle
               ; Trait_Decl_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Trait_Decl,
                 (1 => Trait_Decl_F_Syn_Name,
                  2 => Trait_Decl_F_Traits,
                  3 => Trait_Decl_F_Syn_Base_Type,
                  4 => Trait_Decl_F_Decls));
         end;


         function Create_Decl_Annotation
           (Handle : Rewriting_Handle
               ; Decl_Annotation_F_Name : Node_Rewriting_Handle
               ; Decl_Annotation_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Decl_Annotation,
                 (1 => Decl_Annotation_F_Name,
                  2 => Decl_Annotation_F_Args));
         end;


         function Create_Decl_Annotation_Args
           (Handle : Rewriting_Handle
               ; Decl_Annotation_Args_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Decl_Annotation_Args,
                 (1 => Decl_Annotation_Args_F_Args));
         end;


         function Create_Elsif_Branch
           (Handle : Rewriting_Handle
               ; Elsif_Branch_F_Cond_Expr : Node_Rewriting_Handle
               ; Elsif_Branch_F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Elsif_Branch,
                 (1 => Elsif_Branch_F_Cond_Expr,
                  2 => Elsif_Branch_F_Then_Expr));
         end;


         function Create_Enum_Class_Case
           (Handle : Rewriting_Handle
               ; Enum_Class_Case_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Enum_Class_Case,
                 (1 => Enum_Class_Case_F_Decls));
         end;


         function Create_Any_Of
           (Handle : Rewriting_Handle
               ; Any_Of_F_Expr : Node_Rewriting_Handle
               ; Any_Of_F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Any_Of,
                 (1 => Any_Of_F_Expr,
                  2 => Any_Of_F_Values));
         end;


         function Create_Array_Literal
           (Handle : Rewriting_Handle
               ; Array_Literal_F_Exprs : Node_Rewriting_Handle
               ; Array_Literal_F_Element_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Array_Literal,
                 (1 => Array_Literal_F_Exprs,
                  2 => Array_Literal_F_Element_Type));
         end;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; Base_Call_Expr_F_Name : Node_Rewriting_Handle
               ; Base_Call_Expr_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Call_Expr,
                 (1 => Base_Call_Expr_F_Name,
                  2 => Base_Call_Expr_F_Args));
         end;


         function Create_Logic_Predicate
           (Handle : Rewriting_Handle
               ; Base_Call_Expr_F_Name : Node_Rewriting_Handle
               ; Base_Call_Expr_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Logic_Predicate,
                 (1 => Base_Call_Expr_F_Name,
                  2 => Base_Call_Expr_F_Args));
         end;


         function Create_Logic_Propagate_Call
           (Handle : Rewriting_Handle
               ; Base_Call_Expr_F_Name : Node_Rewriting_Handle
               ; Base_Call_Expr_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Logic_Propagate_Call,
                 (1 => Base_Call_Expr_F_Name,
                  2 => Base_Call_Expr_F_Args));
         end;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; Bin_Op_F_Left : Node_Rewriting_Handle
               ; Bin_Op_F_Op : Node_Rewriting_Handle
               ; Bin_Op_F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Bin_Op,
                 (1 => Bin_Op_F_Left,
                  2 => Bin_Op_F_Op,
                  3 => Bin_Op_F_Right));
         end;


         function Create_Block_Expr
           (Handle : Rewriting_Handle
               ; Block_Expr_F_Val_Defs : Node_Rewriting_Handle
               ; Block_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Block_Expr,
                 (1 => Block_Expr_F_Val_Defs,
                  2 => Block_Expr_F_Expr));
         end;


         function Create_Cast_Expr
           (Handle : Rewriting_Handle
               ; Cast_Expr_F_Expr : Node_Rewriting_Handle
               ; Cast_Expr_F_Null_Cond : Node_Rewriting_Handle
               ; Cast_Expr_F_Excludes_Null : Node_Rewriting_Handle
               ; Cast_Expr_F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Cast_Expr,
                 (1 => Cast_Expr_F_Expr,
                  2 => Cast_Expr_F_Null_Cond,
                  3 => Cast_Expr_F_Excludes_Null,
                  4 => Cast_Expr_F_Dest_Type));
         end;


         function Create_Dot_Expr
           (Handle : Rewriting_Handle
               ; Dot_Expr_F_Prefix : Node_Rewriting_Handle
               ; Dot_Expr_F_Null_Cond : Node_Rewriting_Handle
               ; Dot_Expr_F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Dot_Expr,
                 (1 => Dot_Expr_F_Prefix,
                  2 => Dot_Expr_F_Null_Cond,
                  3 => Dot_Expr_F_Suffix));
         end;


         function Create_Error_On_Null
           (Handle : Rewriting_Handle
               ; Error_On_Null_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Error_On_Null,
                 (1 => Error_On_Null_F_Expr));
         end;


         function Create_Generic_Instantiation
           (Handle : Rewriting_Handle
               ; Generic_Instantiation_F_Name : Node_Rewriting_Handle
               ; Generic_Instantiation_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Generic_Instantiation,
                 (1 => Generic_Instantiation_F_Name,
                  2 => Generic_Instantiation_F_Args));
         end;


         function Create_Grammar_Discard
           (Handle : Rewriting_Handle
               ; Grammar_Discard_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Discard,
                 (1 => Grammar_Discard_F_Expr));
         end;


         function Create_Grammar_Dont_Skip
           (Handle : Rewriting_Handle
               ; Grammar_Dont_Skip_F_Expr : Node_Rewriting_Handle
               ; Grammar_Dont_Skip_F_Dont_Skip : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Dont_Skip,
                 (1 => Grammar_Dont_Skip_F_Expr,
                  2 => Grammar_Dont_Skip_F_Dont_Skip));
         end;


         function Create_Grammar_List
           (Handle : Rewriting_Handle
               ; Grammar_List_F_List_Type : Node_Rewriting_Handle
               ; Grammar_List_F_Kind : Node_Rewriting_Handle
               ; Grammar_List_F_Expr : Node_Rewriting_Handle
               ; Grammar_List_F_Sep : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_List,
                 (1 => Grammar_List_F_List_Type,
                  2 => Grammar_List_F_Kind,
                  3 => Grammar_List_F_Expr,
                  4 => Grammar_List_F_Sep));
         end;


         function Create_Grammar_Null
           (Handle : Rewriting_Handle
               ; Grammar_Null_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Null,
                 (1 => Grammar_Null_F_Name));
         end;


         function Create_Grammar_Opt
           (Handle : Rewriting_Handle
               ; Grammar_Opt_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Opt,
                 (1 => Grammar_Opt_F_Expr));
         end;


         function Create_Grammar_Opt_Error
           (Handle : Rewriting_Handle
               ; Grammar_Opt_Error_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Opt_Error,
                 (1 => Grammar_Opt_Error_F_Expr));
         end;


         function Create_Grammar_Opt_Error_Group
           (Handle : Rewriting_Handle
               ; Grammar_Opt_Error_Group_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Opt_Error_Group,
                 (1 => Grammar_Opt_Error_Group_F_Expr));
         end;


         function Create_Grammar_Opt_Group
           (Handle : Rewriting_Handle
               ; Grammar_Opt_Group_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Opt_Group,
                 (1 => Grammar_Opt_Group_F_Expr));
         end;


         function Create_Grammar_Or_Expr
           (Handle : Rewriting_Handle
               ; Grammar_Or_Expr_F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Or_Expr,
                 (1 => Grammar_Or_Expr_F_Sub_Exprs));
         end;


         function Create_Grammar_Pick
           (Handle : Rewriting_Handle
               ; Grammar_Pick_F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Pick,
                 (1 => Grammar_Pick_F_Exprs));
         end;


         function Create_Grammar_Implicit_Pick
           (Handle : Rewriting_Handle
               ; Grammar_Pick_F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Implicit_Pick,
                 (1 => Grammar_Pick_F_Exprs));
         end;


         function Create_Grammar_Predicate
           (Handle : Rewriting_Handle
               ; Grammar_Predicate_F_Expr : Node_Rewriting_Handle
               ; Grammar_Predicate_F_Prop_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Predicate,
                 (1 => Grammar_Predicate_F_Expr,
                  2 => Grammar_Predicate_F_Prop_Ref));
         end;


         function Create_Grammar_Rule_Ref
           (Handle : Rewriting_Handle
               ; Grammar_Rule_Ref_F_Node_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Rule_Ref,
                 (1 => Grammar_Rule_Ref_F_Node_Name));
         end;


         function Create_Grammar_Skip
           (Handle : Rewriting_Handle
               ; Grammar_Skip_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Skip,
                 (1 => Grammar_Skip_F_Name));
         end;


         function Create_Grammar_Stop_Cut
           (Handle : Rewriting_Handle
               ; Grammar_Stop_Cut_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_Stop_Cut,
                 (1 => Grammar_Stop_Cut_F_Expr));
         end;


         function Create_Parse_Node_Expr
           (Handle : Rewriting_Handle
               ; Parse_Node_Expr_F_Node_Name : Node_Rewriting_Handle
               ; Parse_Node_Expr_F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Parse_Node_Expr,
                 (1 => Parse_Node_Expr_F_Node_Name,
                  2 => Parse_Node_Expr_F_Sub_Exprs));
         end;


         function Create_Token_No_Case_Lit
           (Handle : Rewriting_Handle
               ; Token_No_Case_Lit_F_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Token_No_Case_Lit,
                 (1 => Token_No_Case_Lit_F_Lit));
         end;


         function Create_Token_Pattern_Concat
           (Handle : Rewriting_Handle
               ; Token_Pattern_Concat_F_Left : Node_Rewriting_Handle
               ; Token_Pattern_Concat_F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Token_Pattern_Concat,
                 (1 => Token_Pattern_Concat_F_Left,
                  2 => Token_Pattern_Concat_F_Right));
         end;


         function Create_Token_Ref
           (Handle : Rewriting_Handle
               ; Token_Ref_F_Token_Name : Node_Rewriting_Handle
               ; Token_Ref_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Token_Ref,
                 (1 => Token_Ref_F_Token_Name,
                  2 => Token_Ref_F_Expr));
         end;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; If_Expr_F_Cond_Expr : Node_Rewriting_Handle
               ; If_Expr_F_Then_Expr : Node_Rewriting_Handle
               ; If_Expr_F_Alternatives : Node_Rewriting_Handle
               ; If_Expr_F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_If_Expr,
                 (1 => If_Expr_F_Cond_Expr,
                  2 => If_Expr_F_Then_Expr,
                  3 => If_Expr_F_Alternatives,
                  4 => If_Expr_F_Else_Expr));
         end;


         function Create_Isa
           (Handle : Rewriting_Handle
               ; Isa_F_Expr : Node_Rewriting_Handle
               ; Isa_F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Isa,
                 (1 => Isa_F_Expr,
                  2 => Isa_F_Dest_Type));
         end;


         function Create_Keep_Expr
           (Handle : Rewriting_Handle
               ; Keep_Expr_F_Expr : Node_Rewriting_Handle
               ; Keep_Expr_F_Null_Cond : Node_Rewriting_Handle
               ; Keep_Expr_F_Keep_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Keep_Expr,
                 (1 => Keep_Expr_F_Expr,
                  2 => Keep_Expr_F_Null_Cond,
                  3 => Keep_Expr_F_Keep_Type));
         end;


         function Create_Lambda_Expr
           (Handle : Rewriting_Handle
               ; Lambda_Expr_F_Params : Node_Rewriting_Handle
               ; Lambda_Expr_F_Return_Type : Node_Rewriting_Handle
               ; Lambda_Expr_F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lambda_Expr,
                 (1 => Lambda_Expr_F_Params,
                  2 => Lambda_Expr_F_Return_Type,
                  3 => Lambda_Expr_F_Body));
         end;


         function Create_Null_Lit
           (Handle : Rewriting_Handle
               ; Null_Lit_F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Null_Lit,
                 (1 => Null_Lit_F_Dest_Type));
         end;


         function Create_Block_String_Lit
           (Handle : Rewriting_Handle
               ; Block_String_Lit_F_Lines : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Block_String_Lit,
                 (1 => Block_String_Lit_F_Lines));
         end;


         function Create_Logic_Assign
           (Handle : Rewriting_Handle
               ; Logic_Assign_F_Dest_Var : Node_Rewriting_Handle
               ; Logic_Assign_F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Logic_Assign,
                 (1 => Logic_Assign_F_Dest_Var,
                  2 => Logic_Assign_F_Value));
         end;


         function Create_Logic_Expr
           (Handle : Rewriting_Handle
               ; Logic_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Logic_Expr,
                 (1 => Logic_Expr_F_Expr));
         end;


         function Create_Logic_Propagate
           (Handle : Rewriting_Handle
               ; Logic_Propagate_F_Dest_Var : Node_Rewriting_Handle
               ; Logic_Propagate_F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Logic_Propagate,
                 (1 => Logic_Propagate_F_Dest_Var,
                  2 => Logic_Propagate_F_Call));
         end;


         function Create_Logic_Unify
           (Handle : Rewriting_Handle
               ; Logic_Unify_F_Lhs : Node_Rewriting_Handle
               ; Logic_Unify_F_Rhs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Logic_Unify,
                 (1 => Logic_Unify_F_Lhs,
                  2 => Logic_Unify_F_Rhs));
         end;


         function Create_Match_Expr
           (Handle : Rewriting_Handle
               ; Match_Expr_F_Match_Expr : Node_Rewriting_Handle
               ; Match_Expr_F_Branches : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Match_Expr,
                 (1 => Match_Expr_F_Match_Expr,
                  2 => Match_Expr_F_Branches));
         end;


         function Create_Not_Expr
           (Handle : Rewriting_Handle
               ; Not_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Not_Expr,
                 (1 => Not_Expr_F_Expr));
         end;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; Paren_Expr_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Paren_Expr,
                 (1 => Paren_Expr_F_Expr));
         end;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; Raise_Expr_F_Dest_Type : Node_Rewriting_Handle
               ; Raise_Expr_F_Except_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Raise_Expr,
                 (1 => Raise_Expr_F_Dest_Type,
                  2 => Raise_Expr_F_Except_Expr));
         end;


         function Create_Subscript_Expr
           (Handle : Rewriting_Handle
               ; Subscript_Expr_F_Prefix : Node_Rewriting_Handle
               ; Subscript_Expr_F_Null_Cond : Node_Rewriting_Handle
               ; Subscript_Expr_F_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Subscript_Expr,
                 (1 => Subscript_Expr_F_Prefix,
                  2 => Subscript_Expr_F_Null_Cond,
                  3 => Subscript_Expr_F_Index));
         end;


         function Create_Try_Expr
           (Handle : Rewriting_Handle
               ; Try_Expr_F_Try_Expr : Node_Rewriting_Handle
               ; Try_Expr_F_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Try_Expr,
                 (1 => Try_Expr_F_Try_Expr,
                  2 => Try_Expr_F_Or_Expr));
         end;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; Un_Op_F_Op : Node_Rewriting_Handle
               ; Un_Op_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Un_Op,
                 (1 => Un_Op_F_Op,
                  2 => Un_Op_F_Expr));
         end;


         function Create_Full_Decl
           (Handle : Rewriting_Handle
               ; Full_Decl_F_Doc : Node_Rewriting_Handle
               ; Full_Decl_F_Decl_Annotations : Node_Rewriting_Handle
               ; Full_Decl_F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Full_Decl,
                 (1 => Full_Decl_F_Doc,
                  2 => Full_Decl_F_Decl_Annotations,
                  3 => Full_Decl_F_Decl));
         end;


         function Create_Grammar_List_Sep
           (Handle : Rewriting_Handle
               ; Grammar_List_Sep_F_Token : Node_Rewriting_Handle
               ; Grammar_List_Sep_F_Extra : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Grammar_List_Sep,
                 (1 => Grammar_List_Sep_F_Token,
                  2 => Grammar_List_Sep_F_Extra));
         end;


         function Create_Import
           (Handle : Rewriting_Handle
               ; Import_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Import,
                 (1 => Import_F_Name));
         end;


         function Create_Langkit_Root
           (Handle : Rewriting_Handle
               ; Langkit_Root_F_Imports : Node_Rewriting_Handle
               ; Langkit_Root_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Langkit_Root,
                 (1 => Langkit_Root_F_Imports,
                  2 => Langkit_Root_F_Decls));
         end;


         function Create_Lexer_Case_Rule
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_F_Expr : Node_Rewriting_Handle
               ; Lexer_Case_Rule_F_Alts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lexer_Case_Rule,
                 (1 => Lexer_Case_Rule_F_Expr,
                  2 => Lexer_Case_Rule_F_Alts));
         end;


         function Create_Lexer_Case_Rule_Send
           (Handle : Rewriting_Handle
               ; Lexer_Case_Rule_Send_F_Sent : Node_Rewriting_Handle
               ; Lexer_Case_Rule_Send_F_Match_Size : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Lexer_Case_Rule_Send,
                 (1 => Lexer_Case_Rule_Send_F_Sent,
                  2 => Lexer_Case_Rule_Send_F_Match_Size));
         end;


         function Create_Match_Branch
           (Handle : Rewriting_Handle
               ; Match_Branch_F_Decl : Node_Rewriting_Handle
               ; Match_Branch_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Match_Branch,
                 (1 => Match_Branch_F_Decl,
                  2 => Match_Branch_F_Expr));
         end;


         function Create_Function_Type_Ref
           (Handle : Rewriting_Handle
               ; Function_Type_Ref_F_Param_Types : Node_Rewriting_Handle
               ; Function_Type_Ref_F_Return_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Function_Type_Ref,
                 (1 => Function_Type_Ref_F_Param_Types,
                  2 => Function_Type_Ref_F_Return_Type));
         end;


         function Create_Generic_Type_Ref
           (Handle : Rewriting_Handle
               ; Generic_Type_Ref_F_Type_Name : Node_Rewriting_Handle
               ; Generic_Type_Ref_F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Generic_Type_Ref,
                 (1 => Generic_Type_Ref_F_Type_Name,
                  2 => Generic_Type_Ref_F_Args));
         end;


         function Create_Simple_Type_Ref
           (Handle : Rewriting_Handle
               ; Simple_Type_Ref_F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Simple_Type_Ref,
                 (1 => Simple_Type_Ref_F_Type_Name));
         end;


         function Create_Var_Bind
           (Handle : Rewriting_Handle
               ; Var_Bind_F_Name : Node_Rewriting_Handle
               ; Var_Bind_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Lkt_Var_Bind,
                 (1 => Var_Bind_F_Name,
                  2 => Var_Bind_F_Expr));
         end;

end Liblktlang.Rewriting_Implementation;
