--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.Bump_Ptr;         use Langkit_Support.Bump_Ptr;
with Langkit_Support.Errors;           use Langkit_Support.Errors;
with Langkit_Support.Hashes;
with Langkit_Support.Internal.Analysis;
use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;
use Langkit_Support.Internal.Conversions;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Names;            use Langkit_Support.Names;
with Langkit_Support.Rewriting.Unparsing;
use Langkit_Support.Rewriting.Unparsing;
with Langkit_Support.Slocs;            use Langkit_Support.Slocs;
with Langkit_Support.Text;             use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;            use Langkit_Support.Types;
with Langkit_Support.Unparsing_Config; use Langkit_Support.Unparsing_Config;

package body Langkit_Support.Generic_API.Rewriting is

   use Langkit_Support.Errors.Rewriting;
   use Langkit_Support.Errors.Unparsing;

   function Create_Safety_Net
     (Handle : Rewriting_Handle_Access) return Rewriting_Safety_Net;
   --  Create a safety net for ``Handle``

   function Create_Safety_Net
     (Handle : Unit_Rewriting_Handle_Access) return Rewriting_Safety_Net
   is (if Handle = null
       then No_Rewriting_Safety_Net
       else Create_Safety_Net (Handle.Context_Handle));

   function Create_Safety_Net
     (Handle : Node_Rewriting_Handle_Access) return Rewriting_Safety_Net
   is (if Handle = null
       then No_Rewriting_Safety_Net
       else Create_Safety_Net (Handle.Context_Handle));

   procedure Check_Safety_Net
     (Label : String; Safety_Net : Rewriting_Safety_Net);
   --  Raise a ``Stale_Reference_Error`` if ``Safety_Net`` shows that a stale
   --  reference to a rewriting handle is being used.

   procedure Check_Safety_Net (Label : String; Handle : Rewriting_Handle);
   --  Raise a ``Stale_Reference_Error`` if ``Handle`` is a stable reference

   procedure Check_Safety_Net (Label : String; Handle : Unit_Rewriting_Handle);
   --  Raise a ``Stale_Reference_Error`` if ``Handle`` is a stable reference

   procedure Check_Safety_Net (Label : String; Handle : Node_Rewriting_Handle);
   --  Raise a ``Stale_Reference_Error`` if ``Handle`` is a stable reference

   procedure Check_Safety_Net
     (Label : String; Handles : Node_Rewriting_Handle_Array);
   --  Raise a ``Stale_Reference_Error`` if any of the safety nets in
   --  ``Handles`` shows that a stale reference to a rewriting handle is being
   --  used.

   function Wrap_Handle
     (Handle : Rewriting_Handle_Access) return Rewriting_Handle
   is (Ref => Handle, Safety_Net => Create_Safety_Net (Handle))
   with
     Export, External_Name => External_Name_Prefix & "wrap_rewriting_handle";

   function Unwrap_Handle
     (Handle : Rewriting_Handle) return Rewriting_Handle_Access
   is (Handle.Ref)
   with
     Export, External_Name => External_Name_Prefix & "unwrap_rewriting_handle";

   function Wrap_URH
     (Handle : Unit_Rewriting_Handle_Access) return Unit_Rewriting_Handle
   is (Ref => Handle, Safety_Net => Create_Safety_Net (Handle))
   with
     Export,
     External_Name => External_Name_Prefix & "wrap_unit_rewriting_handle";

   function Unwrap_URH
     (Handle : Unit_Rewriting_Handle) return Unit_Rewriting_Handle_Access
   is (Handle.Ref)
   with
     Export,
     External_Name => External_Name_Prefix & "unwrap_unit_rewriting_handle";

   function Wrap_NRH
     (Handle : Node_Rewriting_Handle_Access) return Node_Rewriting_Handle
   is (Ref => Handle, Safety_Net => Create_Safety_Net (Handle))
   with
     Export,
     External_Name => External_Name_Prefix & "wrap_node_rewriting_handle";

   function Unwrap_NRH
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Access
   is (Handle.Ref)
   with
     Export,
     External_Name => External_Name_Prefix & "unwrap_node_rewriting_handle";

   function Unwrap_Unparsing_Configuration
     (Config : Unparsing_Configuration) return Unparsing_Configuration_Access
   with
     Import,
     External_Name => External_Name_Prefix & "unwrap_unparsing_config";

   function Hash is new Langkit_Support.Hashes.Hash_Access
     (Node_Rewriting_Handle_Record, Node_Rewriting_Handle_Access);
   package NRH_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Node_Rewriting_Handle_Access,
      Equivalent_Elements => "=",
      Hash                => Hash);

   procedure Pre_Check_Ctx (Label : String; Context : Lk_Context);
   --  Check that the given context is not null

   procedure Pre_Check_RW_Handle
     (Label : String; Handle : Rewriting_Handle_Access);
   --  Check that the given rewriting handle is not null

   procedure Pre_Check_URW_Handle
     (Label : String; Handle : Unit_Rewriting_Handle_Access);
   --  Check that the given unit rewriting handle is not null

   procedure Pre_Check_NRW_Handle
     (Label : String; Handle : Node_Rewriting_Handle_Access);
   --  Check that the given node rewriting handle is not null

   procedure Pre_Check_Unit_No_Diags (Label : String; Unit : Lk_Unit);
   --  Check that the given analysis unit does not have any diagnostics

   procedure Pre_Check_Is_Token_Kind (Label : String; T : Type_Ref);
   --  Check that the ``T`` designates a token node type

   procedure Pre_Check_Is_Not_Token_Kind (Label : String; T : Type_Ref);
   --  Check that the ``T`` does not designate a token node type

   procedure Pre_Check_Is_Not_Error_Kind (Label : String; T : Type_Ref);
   --  Check that the ``T`` does not designate an error node type

   procedure Pre_Check_Is_List_Kind (Label : String; T : Type_Ref);
   --  Check that the ``T`` designates a list node type

   procedure Pre_Check_Is_Tied
     (Label : String; Handle : Node_Rewriting_Handle_Access);
   --  Check that the given node handle is a non-null node tied to a rewriting
   --  context.

   procedure Pre_Check_Untied
     (Label : String; Handle : Node_Rewriting_Handle_Access);
   --  Check that the given node handle is not null nor tied to any unit
   --  rewriting context.

   procedure Pre_Check_Null_Or_Untied
     (Label : String; Handle : Node_Rewriting_Handle_Access);
   --  Check that the given node handle is null or not tied to any unit
   --  rewriting context.

   procedure Pre_Check_Null_Or_Valid_Context
     (Label  : String;
      Node   : Node_Rewriting_Handle_Access;
      Handle : Rewriting_Handle_Access);
   --  Check that the ``Node`` rewriting handle is either null, or is
   --  associated to the given rewriting ``Handle``.

   procedure Pre_Check_Same_Language
     (Label : String; Actual, Expected : Language_Id);
   --  Check that the actual and expected languages are the same

   procedure Pre_Check_Type_Ref
     (Label : String; T : Type_Ref; Language : Language_Id);
   --  Check that ``T`` is not null and that its language is ``Language``

   procedure Pre_Check_Member_Ref
     (Label : String; Member : Struct_Member_Ref; Language : Language_Id);
   --  Check that ``Member`` is not null and that its language is ``Language``

   function Parent_Is_List (Node : Node_Rewriting_Handle_Access) return Boolean
   is (Node.Parent /= null and then Node.Parent.Children.Kind = Expanded_List);
   --  Return whether Node's parent is a list node.
   --
   --  Since a tied and non-root node rewriting handle can exist only when its
   --  parent is expanded, it is safe checking the parent's Children field.

   function Index_For
     (Handle : Node_Rewriting_Handle_Access;
      Member : Struct_Member_Ref) return Positive;
   --  Return the 1-based index of the ``Member`` parse field in the node
   --  referenced ``Handle``. Raise a ``Precondition_Failure`` if there is no
   --  such member.

   function Index_In_Parent_List
     (Handle : Node_Rewriting_Handle_Access) return Positive;
   --  Assuming ``Handle`` is a node whose parent is a list node, return its
   --  1-based index in that list node.

   procedure Set_Child
     (Handle : Node_Rewriting_Handle_Access;
      Index  : Positive;
      Child  : Node_Rewriting_Handle_Access);
   --  Assign ``Child`` to the child slot at the given ``Index`` in ``Handle``

   function Allocate
     (Kind          : Type_Ref;
      Context       : Rewriting_Handle_Access;
      Unit_Handle   : Unit_Rewriting_Handle_Access;
      Parent_Handle : Node_Rewriting_Handle_Access)
      return Node_Rewriting_Handle_Access
      with Pre =>
         Context /= null
         and then (Unit_Handle = null
                   or else Unit_Handle.Context_Handle = Context)
         and then (Parent_Handle = null
                   or else Parent_Handle.Context_Handle = Context);
   --  Allocate a handle for Node and register it in Unit_Handle's map

   function Allocate
     (Node          : Lk_Node;
      Context       : Rewriting_Handle_Access;
      Unit_Handle   : Unit_Rewriting_Handle_Access;
      Parent_Handle : Node_Rewriting_Handle_Access)
      return Node_Rewriting_Handle_Access
      with Pre =>
         Context /= null
         and then (Unit_Handle = null
                   or else Unit_Handle.Context_Handle = Context)
         and then (Parent_Handle = null
                   or else Parent_Handle.Context_Handle = Context);
   --  Allocate a handle for Node and register it in Unit_Handle's map

   function Allocate_Stub
     (Context : Rewriting_Handle_Access) return Node_Rewriting_Handle_Access;
   --  Allocate a stub rewriting node in ``Context``, to be used as a temporary
   --  node in ``Rotate``.

   procedure Expand_Children (Node : Node_Rewriting_Handle_Access)
      with Pre => Node /= null;
   --  If Node.Children.Kind is Unexpanded, populate Node's list of Children to
   --  mimic the related bare AST node. Otherwise, do nothing.

   procedure Free_Handles (Handle : in out Rewriting_Handle_Access);
   --  Free all resources tied to Handle. This also releases the rewriting
   --  handle singleton in Handle's Context.

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle_Access;
      Unit           : Unit_Rewriting_Handle_Access);
   --  Tie the node represented by handle so that either:
   --
   --    * it is the root of Unit (Parent is null);
   --    * it is a child of Parent (Unit is null).
   --
   --  Do nothing if Handle is null.

   procedure Untie (Handle : Node_Rewriting_Handle_Access);
   --  Untie the node represented by Handle. Do nothing if Handle is null.

   function Clone
     (Handle : Node_Rewriting_Handle_Access)
      return Node_Rewriting_Handle_Access;
   --  Internal implementation for the ``Clone`` public function

   function Image (Handle : Node_Rewriting_Handle_Access) return String;
   --  Implementation for the node Image

   -----------------------
   -- Create_Safety_Net --
   -----------------------

   function Create_Safety_Net
     (Handle : Rewriting_Handle_Access) return Rewriting_Safety_Net is
   begin
      if Handle = null then
         return No_Rewriting_Safety_Net;
      end if;

      declare
         Context : constant Internal_Context :=
           Unwrap_Context (Handle.Context);
      begin
         return
           (Context           => Context,
            Context_Version   => Version (Context),
            Rewriting_Version => Rewriting_Version (Context));
      end;
   end Create_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net
     (Label : String; Safety_Net : Rewriting_Safety_Net) is
   begin
      if Safety_Net.Context = No_Internal_Context then
         null;

      elsif Version (Safety_Net.Context) /= Safety_Net.Context_Version then
         raise Stale_Reference_Error with
           "context was released (" & Label & ")";

      elsif Rewriting_Version (Safety_Net.Context)
              /= Safety_Net.Rewriting_Version
      then
         raise Stale_Reference_Error with
           "rewriting session was terminated (" & Label & ")";
      end if;
   end Check_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Label : String; Handle : Rewriting_Handle) is
   begin
      if Handle.Ref /= null then
         Check_Safety_Net (Label, Handle.Safety_Net);
      end if;
   end Check_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Label : String; Handle : Unit_Rewriting_Handle)
   is
   begin
      if Handle.Ref /= null then
         Check_Safety_Net (Label, Handle.Safety_Net);
      end if;
   end Check_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Label : String; Handle : Node_Rewriting_Handle)
   is
   begin
      if Handle.Ref /= null then
         Check_Safety_Net (Label, Handle.Safety_Net);
      end if;
   end Check_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net
     (Label : String; Handles : Node_Rewriting_Handle_Array) is
   begin
      for I in Handles'Range loop
         Check_Safety_Net (Label & " (" & I'Image & ")", Handles (I));
      end loop;
   end Check_Safety_Net;

   -------------------
   -- Pre_Check_Ctx --
   -------------------

   procedure Pre_Check_Ctx (Label : String; Context : Lk_Context) is
   begin
      if Context = No_Lk_Context then
         raise Precondition_Failure with Label & " must not be null";
      end if;
   end Pre_Check_Ctx;

   -------------------------
   -- Pre_Check_RW_Handle --
   -------------------------

   procedure Pre_Check_RW_Handle
     (Label : String; Handle : Rewriting_Handle_Access) is
   begin
      if Handle = null then
         raise Precondition_Failure with Label & " must not be null";
      end if;
   end Pre_Check_RW_Handle;

   --------------------------
   -- Pre_Check_URW_Handle --
   --------------------------

   procedure Pre_Check_URW_Handle
     (Label : String; Handle : Unit_Rewriting_Handle_Access) is
   begin
      if Handle = null then
         raise Precondition_Failure with Label & " must not be null";
      end if;
   end Pre_Check_URW_Handle;

   --------------------------
   -- Pre_Check_NRW_Handle --
   --------------------------

   procedure Pre_Check_NRW_Handle
     (Label : String; Handle : Node_Rewriting_Handle_Access) is
   begin
      if Handle = null then
         raise Precondition_Failure with Label & " must not be null";
      end if;
   end Pre_Check_NRW_Handle;

   -----------------------------
   -- Pre_Check_Unit_No_Diags --
   -----------------------------

   procedure Pre_Check_Unit_No_Diags (Label : String; Unit : Lk_Unit) is
   begin
      if Unit.Has_Diagnostics then
         raise Precondition_Failure with Label & " must not have diagnostics";
      end if;
   end Pre_Check_Unit_No_Diags;

   -----------------------------
   -- Pre_Check_Is_Token_Kind --
   -----------------------------

   procedure Pre_Check_Is_Token_Kind (Label : String; T : Type_Ref) is
   begin
      if not Is_Token_Node (T) then
         raise Precondition_Failure with
           Label & " must be a token node, got " & Debug_Name (T);
      end if;
   end Pre_Check_Is_Token_Kind;

   ---------------------------------
   -- Pre_Check_Is_Not_Token_Kind --
   ---------------------------------

   procedure Pre_Check_Is_Not_Token_Kind (Label : String; T : Type_Ref) is
   begin
      if Is_Token_Node (T) then
         raise Precondition_Failure with
           Label & " must not be a token node, got " & Debug_Name (T);
      end if;
   end Pre_Check_Is_Not_Token_Kind;

   ---------------------------------
   -- Pre_Check_Is_Not_Error_Kind --
   ---------------------------------

   procedure Pre_Check_Is_Not_Error_Kind (Label : String; T : Type_Ref) is
   begin
      if Is_Error_Node (T) then
         raise Precondition_Failure with
           Label & " must not be an error node, got " & Debug_Name (T);
      end if;
   end Pre_Check_Is_Not_Error_Kind;

   ----------------------------
   -- Pre_Check_Is_List_Kind --
   ----------------------------

   procedure Pre_Check_Is_List_Kind (Label : String; T : Type_Ref) is
   begin
      if not Is_List_Node (T) then
         raise Precondition_Failure with
           Label & " must be a list node, got " & Debug_Name (T);
      end if;
   end Pre_Check_Is_List_Kind;

   -----------------------
   -- Pre_Check_Is_Tied --
   -----------------------

   procedure Pre_Check_Is_Tied
     (Label : String; Handle : Node_Rewriting_Handle_Access) is
   begin
      if Handle = null or else not Handle.Tied then
         raise Precondition_Failure with
           Label & " must be non-null and tied to a rewriting context";
      end if;
   end Pre_Check_Is_Tied;

   ----------------------
   -- Pre_Check_Untied --
   ----------------------

   procedure Pre_Check_Untied
     (Label : String; Handle : Node_Rewriting_Handle_Access) is
   begin
      if Handle = null or else Handle.Tied then
         raise Precondition_Failure with
           Label
           & " must be non-null and not tied to another rewriting context";
      end if;
   end Pre_Check_Untied;

   ------------------------------
   -- Pre_Check_Null_Or_Untied --
   ------------------------------

   procedure Pre_Check_Null_Or_Untied
     (Label : String; Handle : Node_Rewriting_Handle_Access) is
   begin
      if Handle /= null and then Handle.Tied then
         raise Precondition_Failure with
           Label & " must not be tied to another rewriting context";
      end if;
   end Pre_Check_Null_Or_Untied;

   -------------------------------------
   -- Pre_Check_Null_Or_Valid_Context --
   -------------------------------------

   procedure Pre_Check_Null_Or_Valid_Context
     (Label  : String;
      Node   : Node_Rewriting_Handle_Access;
      Handle : Rewriting_Handle_Access) is
   begin
      if Node /= null and then Node.Context_Handle /= Handle then
         raise Precondition_Failure with
            Label & " should be associated to the right rewriting context";
      end if;
   end Pre_Check_Null_Or_Valid_Context;

   -----------------------------
   -- Pre_Check_Same_Language --
   -----------------------------

   procedure Pre_Check_Same_Language
     (Label : String; Actual, Expected : Language_Id) is
   begin
      if Actual /= No_Language_Id and then Actual /= Expected then
         raise Precondition_Failure with
            Label & " should be associated to "
            & Language_Name (Expected).Image & ", got "
            & Language_Name (Actual).Image;
      end if;
   end Pre_Check_Same_Language;

   ------------------------
   -- Pre_Check_Type_Ref --
   ------------------------

   procedure Pre_Check_Type_Ref
     (Label : String; T : Type_Ref; Language : Language_Id) is
   begin
      if T = No_Type_Ref then
         raise Precondition_Failure with Label & " must not be null";
      end if;
      Pre_Check_Same_Language (Label, T.Language, Language);
   end Pre_Check_Type_Ref;

   --------------------------
   -- Pre_Check_Member_Ref --
   --------------------------

   procedure Pre_Check_Member_Ref
     (Label : String; Member : Struct_Member_Ref; Language : Language_Id)
   is
   begin
      if Member = No_Struct_Member_Ref then
         raise Precondition_Failure with Label & " must not be null";
      end if;
      Pre_Check_Same_Language (Label, Member.Language, Language);
   end Pre_Check_Member_Ref;

   ---------------
   -- Index_For --
   ---------------

   function Index_For
     (Handle : Node_Rewriting_Handle_Access;
      Member : Struct_Member_Ref) return Positive
   is
      Result : Positive := 1;
   begin
      if Member = No_Struct_Member_Ref then
         raise Precondition_Failure with "Member must not be null";
      end if;
      Pre_Check_Same_Language
        ("Member", Member.Language, Handle.Context_Handle.Language);
      if Is_Property (Member) then
         raise Precondition_Failure with "got property, parse field expected";
      end if;

      declare
         T            : constant Type_Ref := Handle.Kind;
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
     (Handle : Node_Rewriting_Handle_Access) return Positive
   is
      Parent : constant Node_Rewriting_Handle_Access := Handle.Parent;
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

   ------------
   -- Handle --
   ------------

   function Handle (Context : Lk_Context) return Rewriting_Handle is
      function "+" is new Ada.Unchecked_Conversion
        (System.Address, Rewriting_Handle_Access);
   begin
      Pre_Check_Ctx ("Context", Context);

      declare
         C : constant Internal_Context := Unwrap_Context (Context);
      begin
         return Wrap_Handle (+Get_Rewriting_Handle (C));
      end;
   end Handle;

   -------------
   -- Context --
   -------------

   function Context (Handle : Rewriting_Handle) return Lk_Context is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);
      return Handle.Ref.Context;
   end Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Lk_Context;
      Config  : Unparsing_Configuration := No_Unparsing_Configuration)
      return Rewriting_Handle
   is
      use type System.Address;

      function "+" is new Ada.Unchecked_Conversion
        (Rewriting_Handle_Access, System.Address);

      Cfg : Unparsing_Configuration := Config;
   begin
      Pre_Check_Ctx ("Context", Context);

      if Config = No_Unparsing_Configuration then
         Cfg := Default_Unparsing_Configuration (Context.Language);
      end if;

      declare
         C      : constant Internal_Context := Unwrap_Context (Context);
         Result : Rewriting_Handle_Access;
      begin
         if Get_Rewriting_Handle (C) /= System.Null_Address then
            raise Precondition_Failure with "Handle (Context) must be null";
         end if;

         Result := new Rewriting_Handle_Record'
           (Language  => Context.Language,
            Context   => Context,
            Config    => Cfg,
            Units     => <>,
            Pool      => Create,
            New_Nodes => <>,
            Stubs     => <>);
         Result.New_Nodes := Nodes_Pools.Create (Result.Pool);
         Result.Stubs := Nodes_Pools.Create (Result.Pool);
         Set_Rewriting_Handle (C, +Result);
         return Wrap_Handle (Result);
      end;
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);
      Free_Handles (Handle.Ref);
      Handle := No_Rewriting_Handle;
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is
      H                : Rewriting_Handle_Access renames Handle.Ref;
      Desc             : Language_Descriptor_Access;
      Unparsing_Config : Unparsing_Configuration_Access;

      --  We first run the unparser on all rewritten units without modifying
      --  these units, and apply modifications only once we are sure the
      --  unparsing was successful everywhere. The following data structures
      --  hold the result of the unparsing.

      type Processed_Unit_Record is record
         Unit     : Lk_Unit;
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
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_RW_Handle ("Handle", H);
      Desc := +H.Context.Language;
      Unparsing_Config := Unwrap_Unparsing_Configuration (H.Config);

      --  Try to reparse all units that were potentially modified

      for Unit_Handle of H.Units loop
         declare
            PU    : constant Processed_Unit := new Processed_Unit_Record'
              (Unit     => Unit_Handle.Unit,
               New_Data => <>);
            Input : Lexer_Input :=
              (Kind        => Bytes_Buffer,
               Charset     => <>,
               Read_BOM    => False,
               Bytes       => System.Null_Address,
               Bytes_Count => 0);
            Bytes : String_Access;

            New_Root : constant Abstract_Node :=
              Abstract_Node_From_Rewriting (Unit_Handle.Root);

            function Error_Result return Apply_Result
            is ((Success => False, Unit => PU.Unit, Diagnostics => <>));
         begin
            Units.Append (PU);

            --  Reparse (i.e. unparse and then parse) this rewritten unit

            begin
               Bytes := Unparse
                 (New_Root,
                  PU.Unit,
                  Unparsing_Config    => Unparsing_Config,
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
            Input.Charset := To_Unbounded_String (Unit_Handle.Unit.Charset);
            Input.Bytes := Bytes.all'Address;
            Input.Bytes_Count := Bytes.all'Length;
            Desc.Unit_Do_Parsing.all
              (Unwrap_Unit (Unit_Handle.Unit), Input, PU.New_Data);
            Free (Bytes);

            --  If there is a parsing error or if the reparsed tree does not
            --  have the same shape as the rewriting handle tree, abort the
            --  rewriting process.

            if PU.New_Data.Present
               and then not
                 (PU.New_Data.Diagnostics.Is_Empty
                  and then Has_Same_Shape
                             (H.Context.Language, New_Root, PU.New_Data))
            then
               Result := Error_Result;
               Result.Diagnostics.Move (PU.New_Data.Diagnostics);
               Destroy (PU.New_Data);
               exit;
            end if;
         end;
      end loop;

      --  If reparsing went fine for all units, actually replace the AST nodes
      --  all over the context and free all resources associated to Handle.

      if Result.Success then
         for PU of Units loop
            Desc.Unit_Update_After_Reparse.all
              (Unwrap_Unit (PU.Unit), PU.New_Data);
         end loop;
         Free_Handles (H);
         Handle := No_Rewriting_Handle;
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
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);

      declare
         Count  : constant Natural := Natural (Handle.Ref.Units.Length);
         Result : Unit_Rewriting_Handle_Array (1 .. Count);
         I      : Positive := 1;
      begin
         for Unit of Handle.Ref.Units loop
            Result (I) := Wrap_URH (Unit);
            I := I + 1;
         end loop;
         return Result;
      end;
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Lk_Unit) return Unit_Rewriting_Handle is
   begin
      Pre_Check_RW_Handle ("Handle (Unit.Context)", Handle (Unit.Context).Ref);
      Pre_Check_Unit_No_Diags ("Unit", Unit);

      declare
         use Unit_Maps;

         Context        : constant Lk_Context := Unit.Context;
         Context_Handle : constant Rewriting_Handle_Access :=
           Handle (Context).Ref;
         Filename       : constant Unbounded_String :=
           To_Unbounded_String (Unit.Filename);

         Cur : constant Cursor := Context_Handle.Units.Find (Filename);
      begin
         if Cur /= No_Element then
            return Wrap_URH (Element (Cur));
         end if;

         declare
            Result : constant Unit_Rewriting_Handle_Access :=
              new Unit_Rewriting_Handle_Record'
                (Context_Handle => Context_Handle,
                 Unit           => Unit,
                 Root           => <>,
                 Nodes          => <>);
         begin
            Context_Handle.Units.Insert (Filename, Result);
            Result.Root := Handle (Unit.Root).Ref;
            return Wrap_URH (Result);
         end;
      end;
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Lk_Unit is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_URW_Handle ("Handle", Handle.Ref);
      return Handle.Ref.Unit;
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_URW_Handle ("Handle", Handle.Ref);
      return Wrap_NRH (Handle.Ref.Root);
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle)
   is
      H : Unit_Rewriting_Handle_Access renames Handle.Ref;
      R : Node_Rewriting_Handle_Access renames Root.Ref;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("Root", Root);
      Pre_Check_URW_Handle ("Handle", H);
      Pre_Check_Null_Or_Untied ("Root", R);

      Untie (H.Root);
      H.Root := R;
      Tie (R, null, H);
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_URW_Handle ("Handle", Handle.Ref);
      return Unparse
        (Node             => Abstract_Node_From_Rewriting (Handle.Ref.Root),
         Unit             => Handle.Ref.Unit,
         Unparsing_Config => Unwrap_Unparsing_Configuration
                               (Handle.Ref.Context_Handle.Config),
         As_Unit          => True);
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle (Node : Lk_Node) return Node_Rewriting_Handle is
   begin
      Pre_Check_RW_Handle
        ("Handle (Node.Unit.Context)", Handle (Node.Unit.Context).Ref);
      Pre_Check_Unit_No_Diags ("Node.Unit", Node.Unit);

      if Node.Is_Null then
         return No_Node_Rewriting_Handle;
      end if;

      declare
         use Node_Maps;

         Unit_Handle : constant Unit_Rewriting_Handle_Access :=
            Handle (Node.Unit).Ref;
         Cur         : constant Cursor := Unit_Handle.Nodes.Find (Node);
      begin
         --  If we have already built a handle for this node, just return it

         if Cur /= No_Element then
            return Wrap_NRH (Element (Cur));

         --  Otherwise, if this node has a parent, make sure this parent has
         --  its own handle, then expand its children. This last must create
         --  the handle we are supposed to return.

         elsif not Node.Parent.Is_Null then
            Expand_Children (Handle (Node.Parent).Ref);
            return Wrap_NRH (Element (Unit_Handle.Nodes.Find (Node)));
         end if;

         --  Otherwise, we are dealing with the root node: just create its
         --  rewriting handle.

         return Wrap_NRH
           (Allocate (Node, Unit_Handle.Context_Handle, Unit_Handle, null));
      end;
   end Handle;

   ----------
   -- Node --
   ----------

   function Node (Handle : Node_Rewriting_Handle) return Lk_Node is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Handle.Ref.Node;
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Wrap_Handle (Handle.Ref.Context_Handle);
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
      Result : Unbounded_Text_Type;
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Result := Unparse
        (Node             => Abstract_Node_From_Rewriting (Handle.Ref),
         Unit             => No_Lk_Unit,
         Unparsing_Config => Unwrap_Unparsing_Configuration
                               (Handle.Ref.Context_Handle.Config),
         As_Unit          => False);
      return To_Text (Result);
   end Unparse;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Kind          : Type_Ref;
      Context       : Rewriting_Handle_Access;
      Unit_Handle   : Unit_Rewriting_Handle_Access;
      Parent_Handle : Node_Rewriting_Handle_Access)
      return Node_Rewriting_Handle_Access
   is
      Tied : constant Boolean := Unit_Handle /= null;
   begin
      return new Node_Rewriting_Handle_Record'
        (Context_Handle => Context,
         Node           => No_Lk_Node,
         Parent         => Parent_Handle,
         Previous       => null,
         Next           => null,
         Kind           => Kind,
         Tied           => Tied,
         Tile           => 0,
         Root_Of        =>
           (if Tied and then Parent_Handle = null
            then Unit_Handle
            else null),
         Children       => Unexpanded_Children);
   end Allocate;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Node          : Lk_Node;
      Context       : Rewriting_Handle_Access;
      Unit_Handle   : Unit_Rewriting_Handle_Access;
      Parent_Handle : Node_Rewriting_Handle_Access)
      return Node_Rewriting_Handle_Access
   is
      Result : constant Node_Rewriting_Handle_Access :=
        Allocate (Type_Of (Node), Context, Unit_Handle, Parent_Handle);
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
     (Context : Rewriting_Handle_Access) return Node_Rewriting_Handle_Access
   is
   begin
      return Allocate
        (Kind          => No_Type_Ref,
         Context       => Context,
         Unit_Handle   => null,
         Parent_Handle => null);
   end Allocate_Stub;

   ---------------------
   -- Expand_Children --
   ---------------------

   procedure Expand_Children (Node : Node_Rewriting_Handle_Access) is
      Children : Node_Children renames Node.Children;
   begin
      --  If this handle has already be expanded, there is nothing to do

      if Children.Kind /= Unexpanded then
         return;
      end if;

      --  Otherwise, expand to the appropriate children form

      declare
         N           : constant Lk_Node := Node.Node;
         Unit_Handle : constant Unit_Rewriting_Handle_Access :=
           Handle (N.Unit).Ref;

         function Allocate_Child
           (Child : Lk_Node) return Node_Rewriting_Handle_Access
         is (if Child.Is_Null
             then null
             else Allocate
                    (Child, Unit_Handle.Context_Handle, Unit_Handle, Node));
      begin
         if Is_Token_Node (N) then

            --  N is a token node: its expanded form contains only its text

            Children :=
              (Kind => Expanded_Token_Node,
               Text => To_Unbounded_Text (N.Text));

         elsif Type_Of (N).Is_List_Node then

            --  N is a list node: its expanded form contains a doubly linked
            --  list for its children.

            declare
               Count                : constant Natural := N.Children_Count;
               First, Last, Current : Node_Rewriting_Handle_Access := null;
            begin
               for I in 1 .. Count loop
                  Current := Allocate_Child (N.Child (I));
                  if First = null then
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
               Count : constant Natural := N.Children_Count;
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  Children.Vector.Append (Allocate_Child (N.Child (I)));
               end loop;
            end;
         end if;
      end;
   end Expand_Children;

   ------------------
   -- Free_Handles --
   ------------------

   procedure Free_Handles (Handle : in out Rewriting_Handle_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Rewriting_Handle_Record, Rewriting_Handle_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Unit_Rewriting_Handle_Record, Unit_Rewriting_Handle_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Rewriting_Handle_Record, Node_Rewriting_Handle_Access);

      C : constant Internal_Context := Unwrap_Context (Handle.Context);
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
            N : Node_Rewriting_Handle_Access := Node;
         begin
            Free (N);
         end;
      end loop;
      for Node of Handle.Stubs loop
         declare
            N : Node_Rewriting_Handle_Access := Node;
         begin
            Free (N);
         end;
      end loop;
      Free (Handle.Pool);
      Free (Handle);

      Set_Rewriting_Handle (C, System.Null_Address);
   end Free_Handles;

   ---------
   -- Tie --
   ---------

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle_Access;
      Unit           : Unit_Rewriting_Handle_Access) is
   begin
      if Handle /= null then
         Handle.Parent := Parent;
         Handle.Tied := True;
         if Parent = null then
            Handle.Root_Of := Unit;
         end if;
      end if;
   end Tie;

   -----------
   -- Untie --
   -----------

   procedure Untie (Handle : Node_Rewriting_Handle_Access) is
   begin
      if Handle /= null then
         Handle.Parent := null;
         Handle.Previous := null;
         Handle.Next := null;
         Handle.Tied := False;
         Handle.Root_Of := null;
      end if;
   end Untie;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Handle : Node_Rewriting_Handle) return Type_Ref is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Handle.Ref.Kind;
   end Type_Of;

   -----------
   -- Image --
   -----------

   function Image (Handle : Node_Rewriting_Handle_Access) return String is
   begin
      if Handle = null then
         return "None";
      end if;

      declare
         Tied_Suffix : constant String :=
           (if Handle.Tied then " (tied)" else "");
      begin
         if Handle.Node.Is_Null then
            declare
               K          : constant Type_Ref := Handle.Kind;
               Tok_Suffix : constant String :=
                 (if K.Is_Token_Node
                  then " " & Image (Handle.Text, With_Quotes => True)
                  else "");
            begin
               return "<" & K.Debug_Name & Tok_Suffix & Tied_Suffix & ">";
            end;
         else
            declare
               Img : constant String := Handle.Node.Image;
            begin
               return Img (Img'First .. Img'Last - 1) & Tied_Suffix & ">";
            end;
         end if;
      end;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Handle : Node_Rewriting_Handle) return String is
   begin
      Check_Safety_Net ("Handle", Handle);
      return Image (Handle.Ref);
   end Image;

   -----------
   -- Print --
   -----------

   procedure Print
     (Handle : Node_Rewriting_Handle; Line_Prefix : String := "")
   is
      procedure Recurse
        (Node : Node_Rewriting_Handle_Access; Line_Prefix : String);

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Node : Node_Rewriting_Handle_Access; Line_Prefix : String) is
      begin
         Put (Line_Prefix & Image (Node));
         if Node = null then
            New_Line;
            return;
         end if;

         declare
            T               : Type_Ref renames Node.Kind;
            Children        : Node_Children renames Node.Children;
            Attr_Prefix     : constant String := Line_Prefix & "|";
            Children_Prefix : constant String := Line_Prefix & "|  ";
         begin
            case Children.Kind is
               when Unexpanded =>
                  Put_Line (": <copy of original node>");

               when Expanded_Regular =>
                  New_Line;
                  declare
                     I : Positive := 1;
                  begin
                     for M of T.Members loop
                        if M.Is_Field and then not M.Is_Null_For (T) then
                           Put_Line
                             (Attr_Prefix
                              & Image (Format_Name (Member_Name (M), Lower))
                              & ":");
                           Recurse (Children.Vector (I), Children_Prefix);
                           I := I + 1;
                        end if;
                     end loop;
                  end;

               when Expanded_List =>
                  if Children.First = null then
                     Put_Line (": <empty list>");
                     return;
                  end if;

                  New_Line;
                  declare
                     C : Node_Rewriting_Handle_Access := Children.First;
                  begin
                     while C /= null loop
                        Recurse (C, Children_Prefix);
                        C := C.Next;
                     end loop;
                  end;

               when Expanded_Token_Node =>
                  New_Line;
            end case;
         end;
      end Recurse;
   begin
      Check_Safety_Net ("Handle", Handle);
      Recurse (Handle.Ref, Line_Prefix);
   end Print;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Handle.Ref.Tied;
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Wrap_NRH (Handle.Ref.Parent);
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Children_Count (Handle.Ref);
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle
   is
      H     : Node_Rewriting_Handle_Access renames Handle.Ref;
      Index : Positive;
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", H);
      Pre_Check_Member_Ref
        ("Field", Field, Handle.Ref.Context_Handle.Language);

      Index := Index_For (H, Field);

      --  If this handle represents an already existing node, make sure it is
      --  expanded so we have a handle to return.

      Expand_Children (H);
      return Wrap_NRH (H.Children.Vector.Element (Index));
   end Child;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      return Result : Node_Rewriting_Handle := Handle do
         for F of Fields loop
            Result := Child (Result, F);
         end loop;
      end return;
   end Child;

   --------------
   -- Children --
   --------------

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array
   is
      I : Positive := 1;
      N : Node_Rewriting_Handle;
   begin
      return Result : Node_Rewriting_Handle_Array (1 .. Handle.Children_Count)
      do
         if Handle.Is_List_Node then
            N := Handle.First_Child;
            while N.Ref /= null loop
               Result (I) := N;
               I := I + 1;
               N := N.Next_Child;
            end loop;
         else
            declare
               T            : constant Type_Ref := Handle.Type_Of;
               Node_Members : constant Struct_Member_Ref_Array := T.Members;
            begin
               for M of Node_Members loop
                  if M.Is_Field and then not M.Is_Null_For (T) then
                     Result (I) := Handle.Child (M);
                     I := I + 1;
                  end if;
               end loop;
            end;
         end if;
      end return;
   end Children;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle_Access;
      Index  : Positive;
      Child  : Node_Rewriting_Handle_Access) is
   begin
      --  If this handle represents an already existing node, make sure it is
      --  expanded so that its children vector can be modified.

      Expand_Children (Handle);

      declare
         Child_Slot : Node_Rewriting_Handle_Access renames
            Handle.Children.Vector.Reference (Index);
      begin
         --  Untie the child to be replaced if it exists

         Untie (Child_Slot);

         --  Tie the new child if it exists

         Tie (Child, Handle, null);

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
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("Child", Child);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Pre_Check_Member_Ref
        ("Field", Field, Handle.Ref.Context_Handle.Language);
      Pre_Check_Null_Or_Untied ("Child", Child.Ref);
      Pre_Check_Null_Or_Valid_Context
        ("Child", Child.Ref, Handle.Ref.Context_Handle);

      Set_Child (Handle.Ref, Index_For (Handle.Ref, Field), Child.Ref);
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Pre_Check_Is_Token_Kind ("Handle.Type_Of", Handle.Type_Of);
      return Langkit_Support.Rewriting.Types.Text (Handle.Ref);
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
      H : Node_Rewriting_Handle_Access renames Handle.Ref;
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", H);
      Pre_Check_Is_Token_Kind ("Handle.Type_Of", H.Kind);

      --  Make sure Handle is expanded so we have a Text field to override
      Expand_Children (H);

      H.Children.Text := To_Unbounded_Text (Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
      H      : Node_Rewriting_Handle_Access renames Handle.Ref;
      N      : Node_Rewriting_Handle_Access renames New_Node.Ref;
      Parent : Node_Rewriting_Handle_Access;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("New_Node", New_Node);
      Pre_Check_NRW_Handle ("Handle", H);
      Pre_Check_Is_Tied ("Handle", H);
      Pre_Check_Null_Or_Untied ("New_Node", N);
      Pre_Check_Null_Or_Valid_Context
        ("New_Node", New_Node.Ref, Handle.Ref.Context_Handle);

      if Handle = New_Node then
         return;
      end if;

      if H.Root_Of = null then

         --  If Handle is not the root node of its owning unit, go replace it
         --  in its parent's children list.

         if Parent_Is_List (H) then
            if New_Node.Ref = null then
               Remove_Child (Handle);
               return;
            end if;

            Parent := H.Parent;
            if H.Previous = null then
               Parent.Children.First := N;
            else
               H.Previous.Next := N;
            end if;
            if H.Next = null then
               Parent.Children.Last := N;
            else
               H.Next.Previous := N;
            end if;
            Tie (N, Parent, null);
            N.Previous := H.Previous;
            N.Next := H.Next;
            Untie (H);
         else
            Set_Child (H.Parent, Index_In_Parent_List (H), N);
         end if;

      else
         --  Otherwise, replace it as a root node

         Set_Root (Wrap_URH (H.Root_Of), New_Node);
      end if;
   end Replace;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Handles : Node_Rewriting_Handle_Array) is

      function Non_Null_Tied
        (Handle : Node_Rewriting_Handle) return Boolean
      is (Handle.Ref /= null and then Tied (Handle));

      RH : Rewriting_Handle_Access := null;
   begin
      Check_Safety_Net ("Handles", Handles);

      --  Rotate is a no-op if there are less than two handles or none is tied

      if Handles'Length < 2 then
         return;
      end if;

      for H of Handles loop
         if Non_Null_Tied (H) then
            RH := H.Ref.Context_Handle;
            exit;
         end if;
      end loop;
      if RH = null then
         return;
      end if;

      --  Check that each non-null handle is present at most once in the input
      --  list.

      declare
         Handle_Set : NRH_Sets.Set;
      begin
         for H of Handles loop
            if H.Ref /= null then
               begin
                  Handle_Set.Insert (H.Ref);
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
                     Stub :=
                       Wrap_NRH (Nodes_Pools.Get (RH.Stubs, Stub_Cursor));
                     Stub_Cursor := Nodes_Pools.Next (RH.Stubs, Stub_Cursor);
                  else
                     Stub := Wrap_NRH (Allocate_Stub (RH));
                     Nodes_Pools.Append (RH.Stubs, Stub.Ref);
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
            if Handles (I).Ref /= null then
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
               if Stub.Ref /= null then
                  Repl := Handles
                    (if I = Handles'Last then Handles'First else I + 1);
                  Replace (Stub, Repl);
               end if;
            end;
         end loop;
      end;
   end Rotate;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      return Handle.Type_Of.Is_List_Node;
   end Is_List_Node;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Pre_Check_Is_List_Kind ("Handle.Type_Of", Handle.Type_Of);

      Expand_Children (Handle.Ref);
      return Wrap_NRH (Handle.Ref.Children.First);
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Pre_Check_Is_List_Kind ("Handle.Type_Of", Handle.Type_Of);

      Expand_Children (Handle.Ref);
      return Wrap_NRH (Handle.Ref.Children.Last);
   end Last_Child;

   ----------------
   -- Next_Child --
   ----------------

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Pre_Check_NRW_Handle ("Handle.Parent", Handle.Parent.Ref);
      Pre_Check_Is_List_Kind ("Handle.Parent.Type_Of", Handle.Parent.Type_Of);

      return Wrap_NRH (Handle.Ref.Next);
   end Next_Child;

   --------------------
   -- Previous_Child --
   --------------------

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Pre_Check_NRW_Handle ("Handle.Parent", Handle.Parent.Ref);
      Pre_Check_Is_List_Kind ("Handle.Parent.Type_Of", Handle.Parent.Type_Of);

      return Wrap_NRH (Handle.Ref.Previous);
   end Previous_Child;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before (Handle, New_Sibling : Node_Rewriting_Handle) is
      Old_Previous, Parent : Node_Rewriting_Handle_Access;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("New_Sibling", New_Sibling);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Parent := Handle.Ref.Parent;
      Pre_Check_NRW_Handle ("Handle.Parent", Parent);
      Pre_Check_Is_List_Kind ("Handle.Parent.Type_Of", Handle.Parent.Type_Of);
      Pre_Check_Null_Or_Untied ("New_Sibling", New_Sibling.Ref);
      Pre_Check_Null_Or_Valid_Context
        ("New_Sibling", New_Sibling.Ref, Handle.Ref.Context_Handle);

      Old_Previous := Handle.Ref.Previous;
      if Old_Previous = null then
         Handle.Ref.Parent.Children.First := New_Sibling.Ref;
      else
         Old_Previous.Next := New_Sibling.Ref;
      end if;
      New_Sibling.Ref.Previous := Old_Previous;
      New_Sibling.Ref.Next := Handle.Ref;
      Handle.Ref.Previous := New_Sibling.Ref;
      Tie (New_Sibling.Ref, Parent, null);
      Parent.Children.Count := Parent.Children.Count + 1;
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After (Handle, New_Sibling : Node_Rewriting_Handle) is
      Old_Next, Parent : Node_Rewriting_Handle_Access;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("New_Sibling", New_Sibling);
      Pre_Check_NRW_Handle ("Handle", Handle.Ref);
      Parent := Handle.Ref.Parent;
      Pre_Check_NRW_Handle ("Handle.Parent", Parent);
      Pre_Check_Is_List_Kind ("Handle.Parent.Type_Of", Handle.Parent.Type_Of);
      Pre_Check_Null_Or_Untied ("New_Sibling", New_Sibling.Ref);
      Pre_Check_Null_Or_Valid_Context
        ("New_Sibling", New_Sibling.Ref, Handle.Ref.Context_Handle);

      Old_Next := Handle.Ref.Next;
      if Old_Next = null then
         Handle.Ref.Parent.Children.Last := New_Sibling.Ref;
      else
         Old_Next.Previous := New_Sibling.Ref;
      end if;
      New_Sibling.Ref.Next := Old_Next;
      New_Sibling.Ref.Previous := Handle.Ref;
      Handle.Ref.Next := New_Sibling.Ref;
      Tie (New_Sibling.Ref, Parent, null);
      Parent.Children.Count := Parent.Children.Count + 1;
   end Insert_After;

   ------------------
   -- Insert_First --
   ------------------

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle) is
      H : Node_Rewriting_Handle_Access renames Handle.Ref;
      N : Node_Rewriting_Handle_Access renames New_Child.Ref;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("New_Child", New_Child);
      Pre_Check_NRW_Handle ("Handle", H);
      Pre_Check_Is_List_Kind ("Handle.Type_Of", H.Kind);
      Pre_Check_Null_Or_Untied ("New_Child", N);
      Pre_Check_Null_Or_Valid_Context
        ("New_Child", New_Child.Ref, Handle.Ref.Context_Handle);

      Expand_Children (H);
      if H.Children.First /= null then
         H.Children.First.Previous := N;
         N.Next := H.Children.First;
      end if;
      H.Children.First := N;
      if H.Children.Last = null then
         H.Children.Last := N;
      end if;
      Tie (N, H, null);
      H.Children.Count := H.Children.Count + 1;
   end Insert_First;

   -----------------
   -- Insert_Last --
   -----------------

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle) is
      H : Node_Rewriting_Handle_Access renames Handle.Ref;
      N : Node_Rewriting_Handle_Access renames New_Child.Ref;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("New_Child", New_Child);
      Pre_Check_NRW_Handle ("Handle", H);
      Pre_Check_Is_List_Kind ("Handle.Type_Of", H.Kind);
      Pre_Check_Null_Or_Untied ("New_Child", N);
      Pre_Check_Null_Or_Valid_Context
        ("New_Child", New_Child.Ref, Handle.Ref.Context_Handle);

      Expand_Children (H);
      if H.Children.Last /= null then
         H.Children.Last.Next := N;
         N.Previous := H.Children.Last;
      end if;
      H.Children.Last := N;
      if H.Children.First = null then
         H.Children.First := N;
      end if;
      Tie (N, H, null);
      H.Children.Count := H.Children.Count + 1;
   end Insert_Last;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle) is
      H      : Node_Rewriting_Handle_Access renames Handle.Ref;
      Parent : Node_Rewriting_Handle_Access;
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_NRW_Handle ("Handle", H);
      Parent := H.Parent;
      Pre_Check_NRW_Handle ("Handle.Parent", Parent);
      Pre_Check_Is_List_Kind ("Handle.Parent.Type_Of", Parent.Kind);

      Expand_Children (H);
      if Parent.Children.First = H then
         Parent.Children.First := H.Next;
         if H.Next = null then
            Parent.Children.Last := null;
         else
            H.Next.Previous := null;
         end if;
      elsif Parent.Children.Last = H then
         Parent.Children.Last := H.Previous;
         Parent.Children.Last.Next := null;
      else
         H.Previous.Next := H.Next;
         H.Next.Previous := H.Previous;
      end if;
      Untie (H);
      Parent.Children.Count := Parent.Children.Count - 1;
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle_Access)
      return Node_Rewriting_Handle_Access
   is
      Result : Node_Rewriting_Handle_Access;
   begin
      if Handle = null then
         return Handle;
      end if;

      --  Make sure the original handle is expanded so we can iterate on it

      Expand_Children (Handle);

      --  If the input handle is associated to a node, so should be the cloned
      --  handle, so that its formatting is copied as well.

      Result :=
        (if Handle.Node.Is_Null
         then Allocate (Handle.Kind, Handle.Context_Handle, null, null)
         else Allocate (Handle.Node, Handle.Context_Handle, null, null));
      Handle.Context_Handle.New_Nodes.Append (Result);

      --  Recursively clone children

      case Handle.Children.Kind is
         when Unexpanded =>
            raise Program_Error;

         when Expanded_Token_Node =>
            Result.Children :=
              (Kind => Expanded_Token_Node, Text => Handle.Children.Text);

         when Expanded_List =>
            declare
               First, Last, Cloned : Node_Rewriting_Handle_Access := null;
               Current             : Node_Rewriting_Handle_Access :=
                 Handle.Children.First;
            begin
               while Current /= null loop
                  Cloned := Clone (Current);
                  Tie (Cloned, Result, null);
                  if First = null then
                     First := Cloned;
                     Last := Cloned;
                  else
                     Last.Next := Cloned;
                     Cloned.Previous := Last;
                     Last := Cloned;
                  end if;
                  Current := Current.Next;
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
                  Child : constant Node_Rewriting_Handle_Access :=
                    Clone (Handle.Children.Vector.Element (I));
               begin
                  Tie (Child, Result, null);
                  Result.Children.Vector.Append (Child);
               end;
            end loop;
      end case;

      return Result;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      Check_Safety_Net ("Handle", Handle);
      return Wrap_NRH (Clone (Handle.Ref));
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Type_Ref) return Node_Rewriting_Handle
   is
      Children_Count : Natural := 0;
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);
      Pre_Check_Type_Ref ("Kind", Kind, Handle.Ref.Language);
      Pre_Check_Is_Not_Error_Kind ("Kind", Kind);

      if Is_Token_Node (Kind) then
         return Create_Token_Node (Handle, Kind, "");
      else
         for M of Members (Kind) loop
            if Is_Field (M) then
               Children_Count := Children_Count + 1;
            end if;
         end loop;
         return Create_Regular_Node
           (Handle, Kind, (1 .. Children_Count => No_Node_Rewriting_Handle));
      end if;
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Type_Ref;
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      Check_Safety_Net ("Handle", Handle);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);
      Pre_Check_Type_Ref ("Kind", Kind, Handle.Ref.Language);
      Pre_Check_Is_Token_Kind ("Kind", Kind);

      declare
         Result : constant Node_Rewriting_Handle_Access :=
           Allocate (Kind, Handle.Ref, null, null);
      begin
         Result.Children :=
           (Kind => Expanded_Token_Node, Text => To_Unbounded_Text (Text));
         Nodes_Pools.Append (Handle.Ref.New_Nodes, Result);
         return Wrap_NRH (Result);
      end;
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Type_Ref;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
   is
      List : Boolean;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("Children", Children);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);
      Pre_Check_Type_Ref ("Kind", Kind, Handle.Ref.Language);
      Pre_Check_Is_Not_Token_Kind ("Kind", Kind);
      Pre_Check_Is_Not_Error_Kind ("Kind", Kind);
      List := Kind.Is_List_Node;
      for I in Children'Range loop
         declare
            One_Child : constant Node_Rewriting_Handle := Children (I);
            Label     : constant String := "Children (" & I'Image & ")";
         begin
            Pre_Check_Null_Or_Valid_Context (Label, One_Child.Ref, Handle.Ref);
            if List then
               Pre_Check_Untied (Label, One_Child.Ref);
            else
               Pre_Check_Null_Or_Untied (Label, One_Child.Ref);
            end if;
         end;
      end loop;
      if not List then
         declare
            Expected_Children_Count : Natural := 0;
         begin
            for M of Kind.Members loop
               if M.Is_Field then
                  Expected_Children_Count := Expected_Children_Count + 1;
               end if;
            end loop;
            if Children'Length /= Expected_Children_Count then
               raise Precondition_Failure with
                 Expected_Children_Count'Image & " children expected, got"
                 & Children'Length'Image;
            end if;
         end;
      end if;

      declare
         Result : constant Node_Rewriting_Handle_Access :=
           Allocate (Kind, Handle.Ref, null, null);
      begin
         if List then
            declare
               First, Last : Node_Rewriting_Handle_Access := null;
            begin
               for C of Children loop
                  if First = null then
                     First := C.Ref;
                  else
                     Last.Next := C.Ref;
                     C.Ref.Previous := Last;
                  end if;
                  Last := C.Ref;
                  Tie (C.Ref, Result, null);
               end loop;
               Result.Children :=
                 (Kind  => Expanded_List,
                  First => First,
                  Last  => Last,
                  Count => Children'Length);
            end;
         else
            Result.Children :=
              (Kind => Expanded_Regular, Vector => <>);
            Result.Children.Vector.Reserve_Capacity (Children'Length);
            for C of Children loop
               Result.Children.Vector.Append (C.Ref);
               if C.Ref /= null then
                  Tie (C.Ref, Result, null);
               end if;
            end loop;
         end if;
         Handle.Ref.New_Nodes.Append (Result);
         return Wrap_NRH (Result);
      end;
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule_Ref) return Node_Rewriting_Handle
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

      Buffer   : Unbounded_Text_Type;
      State    : State_Type := Default;
      Next_Arg : Positive := Arguments'First;
   begin
      Check_Safety_Net ("Handle", Handle);
      Check_Safety_Net ("Arguments", Arguments);
      Pre_Check_RW_Handle ("Handle", Handle.Ref);
      for I in Arguments'Range loop
         declare
            One_Argument : constant Node_Rewriting_Handle := Arguments (I);
            Label        : constant String := "Arguments (" & I'Image & ")";
         begin
            Pre_Check_Null_Or_Valid_Context
              (Label, One_Argument.Ref, Handle.Ref);
         end;
      end loop;
      if Rule = No_Grammar_Rule_Ref then
         raise Precondition_Failure with "Rule must not be null";
      end if;
      Pre_Check_Same_Language ("Rule", Rule.Language, Handle.Ref.Language);

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
                     Unparsed_Arg : constant Text_Type :=
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
         Ctx      : constant Lk_Context := Handle.Context;
         Id       : constant Language_Id := Ctx.Language;
         Desc     : constant Language_Descriptor_Access := +Id;
         Unit     : constant Internal_Unit :=
           Desc.Context_Templates_Unit.all (Unwrap_Context (Ctx));
         Reparsed : Reparsed_Unit;
         Text     : constant Text_Type := To_Text (Buffer);
         Input    : constant Lexer_Input :=
           (Kind       => Text_Buffer,
            Text       => Text'Address,
            Text_Count => Text'Length);

         function Transform
           (Node   : Lk_Node;
            Parent : Node_Rewriting_Handle_Access)
            return Node_Rewriting_Handle_Access;
         --  Turn a node from the Reparsed unit into a recursively expanded
         --  node rewriting handle.

         ---------------
         -- Transform --
         ---------------

         function Transform
           (Node   : Lk_Node;
            Parent : Node_Rewriting_Handle_Access)
            return Node_Rewriting_Handle_Access
         is
            Result : Node_Rewriting_Handle_Access;
         begin
            if Node.Is_Null then
               return null;
            end if;

            --  Allocate the handle for Node, and don't forget to remove the
            --  backlink to Node itself as it exists only temporarily for
            --  template instantiation. Also, track the newly allocated node
            --  so that it is freed correctly upon destruction of the
            --  rewriting context.

            Result := Allocate (Node, Handle.Ref, null, Parent);
            Result.Node := No_Lk_Node;
            Handle.Ref.New_Nodes.Append (Result);

            if Node.Is_Token_Node then
               declare
                  Index : constant Natural :=
                    Natural
                      (Desc.Node_Token_Start.all (Unwrap_Node (Node).Node)
                       .Index
                       .Token);
                  Data  : constant Stored_Token_Data :=
                     Reparsed.TDH.Tokens.Get (Index);
                  Text  : constant Text_Type := Reparsed.TDH.Source_Buffer
                    (Data.Source_First .. Data.Source_Last);
               begin
                  Result.Children :=
                    (Kind => Expanded_Token_Node,
                     Text => To_Unbounded_Text (Text));
               end;

            elsif Type_Of (Node).Is_List_Node then
               declare
                  Count                : constant Natural :=
                    Node.Children_Count;
                  First, Last, Current : Node_Rewriting_Handle_Access := null;
               begin
                  for I in 1 .. Count loop
                     Current := Transform (Node.Child (I), Result);
                     if First = null then
                        First := Current;
                        Last := Current;
                     else
                        Last.Next := Current;
                        Current.Previous := Last;
                        Last := Current;
                     end if;
                     Tie (Current, Result, null);
                  end loop;
                  Result.Children :=
                    (Kind  => Expanded_List,
                     First => First,
                     Last  => Last,
                     Count => Count);
               end;

            else
               declare
                  Count : constant Natural := Node.Children_Count;
               begin
                  Result.Children := (Kind => Expanded_Regular, Vector => <>);
                  Result.Children.Vector.Reserve_Capacity
                    (Ada.Containers.Count_Type (Count));
                  for I in 1 .. Count loop
                     declare
                        C : constant Node_Rewriting_Handle_Access :=
                          Transform (Child (Node, I), Result);
                     begin
                        Tie (C, Result, null);
                        Result.Children.Vector.Append (C);
                     end;
                  end loop;
               end;
            end if;
            return Result;
         end Transform;

      begin
         Desc.Unit_Set_Rule.all (Unit, To_Index (Rule));
         Desc.Unit_Do_Parsing.all (Unit, Input, Reparsed);
         if not Reparsed.Diagnostics.Is_Empty then
            Destroy (Reparsed);
            raise Template_Instantiation_Error with "got parsing errors";
         end if;

         declare
            Result : constant Node_Rewriting_Handle_Access :=
              Transform (Wrap_Node (Id, Reparsed.Ast_Root), null);
         begin
            Destroy (Reparsed);
            return Wrap_NRH (Result);
         end;
      end;
   end Create_From_Template;

end Langkit_Support.Generic_API.Rewriting;
