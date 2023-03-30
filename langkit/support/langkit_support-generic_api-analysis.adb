--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with System;

with GNATCOLL.VFS;

with Langkit_Support.Errors;       use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Hashes;       use Langkit_Support.Hashes;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Names;        use Langkit_Support.Names;
with Langkit_Support.Types;        use Langkit_Support.Types;

package body Langkit_Support.Generic_API.Analysis is

   function Wrap_Context
     (Id : Language_Id; Context : Internal_Context) return Lk_Context
     with Export, External_Name => External_Name_Prefix & "wrap_context";
   function Unwrap_Context (Context : Lk_Context) return Internal_Context
      with Export, External_Name => External_Name_Prefix & "unwrap_context";
   --  Public/private converters for contexts

   function Wrap_Unit
     (Id : Language_Id; Unit : Internal_Unit) return Lk_Unit
     with Export, External_Name => External_Name_Prefix & "wrap_unit";
   function Unwrap_Unit (Unit : Lk_Unit) return Internal_Unit
      with Export, External_Name => External_Name_Prefix & "unwrap_unit";
   --  Public/private converters for units

   function Wrap_Node
     (Id : Language_Id; Node : Internal_Entity) return Lk_Node
     with Export, External_Name => External_Name_Prefix & "wrap_node";
   function Unwrap_Node (Node : Lk_Node) return Internal_Entity
     with Export, External_Name => External_Name_Prefix & "unwrap_node";
   --  Public/private converters for nodes

   function Wrap_Token
     (Id         : Any_Language_Id;
      Token      : Internal_Token;
      Safety_Net : Token_Safety_Net) return Lk_Token
     with Export, External_Name => External_Name_Prefix & "wrap_token";
   procedure Unwrap_Token
     (Token      : Lk_Token;
      Id         : out Any_Language_Id;
      Data       : out Internal_Token;
      Safety_Net : out Token_Safety_Net)
      with Export, External_Name => External_Name_Prefix & "unwrap_token";
   --  Public/private converters for tokens

   function Create_Node_Safety_Net
     (Unit       : Lk_Unit'Class;
      Rebindings : Env_Rebindings) return Node_Safety_Net;
   --  Create a safety net for a node, to make sure that later access to that
   --  node does not use a stale unit or stale rebindings.

   function Create_Token_Safety_Net
     (TDH  : Token_Data_Handler_Access;
      Unit : Lk_Unit'Class) return Token_Safety_Net;
   --  Create a safety net for a token, to make sure that later access to that
   --  token does not use stale data.

   procedure Check_Safety_Net (Node : Lk_Node'Class);
   --  Check that the unit and rebindings referenced by Node are not stale.
   --  Raise a ``Stale_Reference_Error`` exception if one of them are stale.

   procedure Check_Safety_Net (Token : Lk_Token'Class);
   --  Raise a ``Stale_Reference_Error`` exception in ``Token`` is stale

   procedure Reject_Null_Context (Context : Lk_Context'Class);
   procedure Reject_Null_Unit (Unit : Lk_Unit'Class);
   procedure Reject_Null_Node (Node : Lk_Node'Class);
   procedure Reject_Null_Token (Token : Lk_Token'Class);
   --  Raise a ``Precondition_Failure`` exception if
   --  ``Context``/``Unit``/``Node``/``Token`` is null.

   procedure Check_Same_Unit (Left, Right : Lk_Token'Class);
   --  Raise a ``Precondition_Failure`` exception if ``Left`` and ``Right`` do
   --  not belong to the same unit.

   function Wrap_Node
     (Node : Internal_Node; Origin_Node : Lk_Node'Class) return Lk_Node;
   --  Return a public node to wrap ``Node``. Use safety net and entity info
   --  from ``Origin_Node``.

   function Wrap
     (Token : Internal_Token; Unit : Lk_Unit'Class) return Lk_Token;
   --  Return a public token reference to wrap ``Token``. ``Unit`` must own
   --  the unit that produced this token.

   function Wrap
     (Token : Internal_Token; Node : Lk_Node'Class) return Lk_Token;
   --  Return a public token reference to wrap ``Token``. ``Node`` must belong
   --  to the same analysis unit.

   function Wrap_Token
     (Token : Internal_Token; Origin_Token : Lk_Token'Class) return Lk_Token;
   --  Return a public token reference to wrap ``Token``. Use safety net from
   --  ``Origin Token``.

   function "+" (Kind : Raw_Token_Kind) return Token_Kind_Index
   is (Token_Kind_Index (Kind + 1));
   --  ``Raw_Token_Kind`` is a 0-based index type, whereas ``Token_Kind_Index``
   --  is a 1-based one.

   ----------------------------
   -- Create_Node_Safety_Net --
   ----------------------------

   function Create_Node_Safety_Net
     (Unit       : Lk_Unit'Class;
      Rebindings : Env_Rebindings) return Node_Safety_Net
   is
      Desc    : Language_Descriptor renames Unit.Context.Desc.all;
      Context : constant Internal_Context := Unit.Context.Internal;
   begin
      return
        (Context            => Context,
         Context_Version    => Desc.Context_Version (Context),
         Unit               => Unit.Internal,
         Unit_Version       => Desc.Unit_Version (Unit.Internal),
         Rebindings_Version => (if Rebindings = null
                                then 0
                                else Rebindings.Version));
   end Create_Node_Safety_Net;

   -----------------------------
   -- Create_Token_Safety_Net --
   -----------------------------

   function Create_Token_Safety_Net
     (TDH  : Token_Data_Handler_Access;
      Unit : Lk_Unit'Class) return Token_Safety_Net
   is
      Desc    : Language_Descriptor renames Unit.Context.Desc.all;
      Context : constant Internal_Context := Unit.Context.Internal;
   begin
      return
        (Context         => Context,
         Context_Version => Desc.Context_Version (Context),
         TDH_Version     => TDH.Version);
   end Create_Token_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Node : Lk_Node'Class) is
      Desc       : constant Any_Language_Id := Node.Desc;
      Entity     : Internal_Entity renames Node.Internal;
      Safety_Net : Node_Safety_Net renames Node.Safety_Net;
   begin
      --  Nothing to check if Node is null. If it is not, we know that the unit
      --  and the context both are not null.
      if Node.Internal.Node = No_Internal_Node then
         return;
      end if;

      --  Check that the context has not been released since the creation of
      --  this safety net.
      if Desc.Context_Version (Safety_Net.Context)
         /= Safety_Net.Context_Version
      then
         raise Stale_Reference_Error with "context was released";

      --  Then check that the unit version is the same
      elsif Desc.Unit_Version (Safety_Net.Unit) /= Safety_Net.Unit_Version then
         raise Stale_Reference_Error with "unit was reparsed";

      --  Then check that the rebindings reference, if not-null, is not stale
      elsif Entity.Rebindings /= null
            and then Entity.Rebindings.Version /= Safety_Net.Rebindings_Version
      then
         raise Stale_Reference_Error with "related unit was reparsed";
      end if;
   end Check_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Token : Lk_Token'Class) is
      Desc       : constant Any_Language_Id := Token.Desc;
      Safety_Net : Token_Safety_Net renames Token.Safety_Net;
   begin
      --  Nothing to check if TDH is null. If it is not, we know that the
      --  context is not null.
      if Token.TDH = null then
         return;
      end if;

      --  Check that the context has not been released since the creation of
      --  this safety net.
      if Desc.Context_Version (Safety_Net.Context)
         /= Safety_Net.Context_Version
      then
         raise Stale_Reference_Error with "context was released";

      --  Then check that the TDH version is the same
      elsif Token.TDH.Version /= Safety_Net.TDH_Version then
         raise Stale_Reference_Error with "unit was reparsed";
      end if;
   end Check_Safety_Net;

   -------------------------
   -- Reject_Null_Context --
   -------------------------

   procedure Reject_Null_Context (Context : Lk_Context'Class) is
   begin
      if Context.Internal = No_Internal_Context then
         raise Precondition_Failure with "null context";
      end if;
   end Reject_Null_Context;

   ----------------------
   -- Reject_Null_Unit --
   ----------------------

   procedure Reject_Null_Unit (Unit : Lk_Unit'Class) is
   begin
      if Unit.Context.Internal = No_Internal_Context then
         raise Precondition_Failure with "null unit";
      end if;
   end Reject_Null_Unit;

   ----------------------
   -- Reject_Null_Node --
   ----------------------

   procedure Reject_Null_Node (Node : Lk_Node'Class) is
   begin
      if Node.Internal.Node = No_Internal_Node then
         raise Precondition_Failure with "null node";
      end if;
   end Reject_Null_Node;

   -----------------------
   -- Reject_Null_Token --
   -----------------------

   procedure Reject_Null_Token (Token : Lk_Token'Class) is
   begin
      if Token.Desc = null then
         raise Precondition_Failure with "null token";
      end if;
   end Reject_Null_Token;

   ---------------------
   -- Check_Same_Unit --
   ---------------------

   procedure Check_Same_Unit (Left, Right : Lk_Token'Class) is
   begin
      if Left.TDH /= Right.TDH then
         raise Precondition_Failure with "tokens from different units";
      end if;
   end Check_Same_Unit;

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : Internal_Node; Origin_Node : Lk_Node'Class) return Lk_Node
   is
      E : Internal_Entity renames Origin_Node.Internal;
   begin
      --  Null nodes must always be equal to the dedicated constant
      if Node = No_Internal_Node then
         return No_Lk_Node;
      end if;

      --  Create a new metadata ownership share for the result
      if E.Metadata /= Origin_Node.Desc.Null_Metadata then
         Origin_Node.Desc.Node_Metadata_Inc_Ref (E.Metadata);
      end if;

      return (Ada.Finalization.Controlled with
              Desc       => Origin_Node.Desc,
              Internal   => (Node         => Node,
                             Rebindings   => E.Rebindings,
                             From_Rebound => E.From_Rebound,
                             Metadata     => E.Metadata),
              Safety_Net => Origin_Node.Safety_Net);
   end Wrap_Node;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Token : Internal_Token; Unit : Lk_Unit'Class) return Lk_Token is
   begin
      --  Null token must always be equal to the dedicated constant
      if Token.Index = No_Token_Or_Trivia_Index then
         return No_Lk_Token;
      end if;

      declare
         Desc       : constant Any_Language_Id := Unit.Context.Desc;
         Safety_Net : constant Token_Safety_Net :=
           Create_Token_Safety_Net (Token.TDH, Unit);
      begin
         return (Desc, Token.TDH, Token.Index, Safety_Net);
      end;
   end Wrap;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Token : Internal_Token; Node : Lk_Node'Class) return Lk_Token is
   begin
      return Wrap (Token, Node.Unit);
   end Wrap;

   ----------------
   -- Wrap_Token --
   ----------------

   function Wrap_Token
     (Token : Internal_Token; Origin_Token : Lk_Token'Class) return Lk_Token is
   begin
      --  Null token must always be equal to the dedicated constant
      if Token.Index = No_Token_Or_Trivia_Index then
         return No_Lk_Token;
      end if;

      return (Desc       => Origin_Token.Desc,
              TDH        => Token.TDH,
              Index      => Token.Index,
              Safety_Net => Origin_Token.Safety_Net);
   end Wrap_Token;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Language      : Language_Id;
      Charset       : String := "";
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Natural := 0) return Lk_Context
   is
      Result : constant Internal_Context := Language.Create_Context
        (Charset, File_Reader, With_Trivia, Tab_Stop);
   begin
      return (Ada.Finalization.Controlled with
              Desc     => Language,
              Internal => Result);
   end Create_Context;

   --------------
   -- Language --
   --------------

   function Language (Self : Lk_Context) return Language_Id is
   begin
      Reject_Null_Context (Self);
      return Self.Desc;
   end Language;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Lk_Context) return Hash_Type is
   begin
      return Hash (Self.Internal);
   end Hash;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit (Self : Lk_Context; Unit_Filename : String) return Boolean
   is
   begin
      Reject_Null_Context (Self);
      return Self.Desc.Context_Has_Unit (Self.Internal, Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Self     : Lk_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule_Ref := No_Grammar_Rule_Ref) return Lk_Unit
   is
      Result : Internal_Unit;
   begin
      Reject_Null_Context (Self);

      declare
         Desc        : Language_Descriptor renames Self.Desc.all;
         Actual_Rule : Grammar_Rule_Index;
      begin
         if Rule = No_Grammar_Rule_Ref then
            Actual_Rule := Desc.Default_Grammar_Rule;
         else
            Actual_Rule := To_Index (Rule);
         end if;

         Result := Self.Desc.Context_Get_From_File
           (Self.Internal, Filename, Charset, Reparse, Actual_Rule);

         return (Result, Lk_Context (Self));
      end;
   end Get_From_File;

   --------------
   -- Language --
   --------------

   function Language (Self : Lk_Unit) return Language_Id is
   begin
      Reject_Null_Unit (Self);
      return Self.Context.Desc;
   end Language;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Lk_Unit) return Hash_Type is
   begin
      return Hash (Self.Internal);
   end Hash;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (Self : Lk_Unit; Line_Number : Positive) return Text_Type is
   begin
      Reject_Null_Unit (Self);
      return Self.Context.Desc.Unit_Get_Line (Self.Internal, Line_Number);
   end Get_Line;

   -------------
   -- Context --
   -------------

   function Context (Self : Lk_Unit'Class) return Lk_Context is
   begin
      Reject_Null_Unit (Self);
      return Self.Context;
   end Context;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Lk_Unit) return String is
   begin
      Reject_Null_Unit (Self);

      declare
         Desc : Language_Descriptor renames Self.Context.Desc.all;
      begin
         return Desc.Unit_Filename (Self.Internal);
      end;
   end Filename;

   ----------
   -- Root --
   ----------

   function Root (Self : Lk_Unit'Class) return Lk_Node is

      function Wrap (Node : Internal_Node; Unit : Lk_Unit'Class) return Lk_Node
      with Inline_Always;
      --  Return a public node to wrap ``Node``, given an existing non-null
      --  reference to its owning Unit.

      ----------
      -- Wrap --
      ----------

      function Wrap (Node : Internal_Node; Unit : Lk_Unit'Class) return Lk_Node
      is
         Desc       : constant Any_Language_Id := Unit.Context.Desc;
         Entity     : constant Internal_Entity :=
           (Node, null, False, Desc.Null_Metadata);
         Safety_Net : constant Node_Safety_Net :=
           Create_Node_Safety_Net (Unit, null);
      begin
         return (Ada.Finalization.Controlled with
                 Desc, Entity, Safety_Net);
      end Wrap;

   begin
      Reject_Null_Unit (Self);

      declare
         Desc   : Language_Descriptor renames Self.Context.Desc.all;
         Result : constant Internal_Node := Desc.Unit_Root (Self.Internal);
      begin
         return Wrap (Result, Self);
      end;
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Lk_Unit'Class) return Lk_Token is
   begin
      Reject_Null_Unit (Self);

      declare
         Desc   : Language_Descriptor renames Self.Context.Desc.all;
         Result : constant Internal_Token :=
           Desc.Unit_First_Token (Self.Internal);
      begin
         return Wrap (Result, Self);
      end;
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Self : Lk_Unit'Class) return Lk_Token is
   begin
      Reject_Null_Unit (Self);

      declare
         Desc   : Language_Descriptor renames Self.Context.Desc.all;
         Result : constant Internal_Token :=
           Desc.Unit_Last_Token (Self.Internal);
      begin
         return Wrap (Result, Self);
      end;
   end Last_Token;

   ----------
   -- Text --
   ----------

   function Text (Self : Lk_Unit) return Text_Type is
   begin
      return Text (Self.First_Token, Self.Last_Token);
   end Text;

   --------------
   -- Language --
   --------------

   function Language (Self : Lk_Node) return Language_Id is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);

      return Self.Desc;
   end Language;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Lk_Node) return Hash_Type is
   begin
      return Combine
        (Hash (Self.Internal.Node), Hash (Self.Internal.Rebindings));
   end Hash;

   ----------
   -- Unit --
   ----------

   function Unit (Self : Lk_Node'Class) return Lk_Unit is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);

      declare
         Ctx  : constant Internal_Context := Self.Safety_Net.Context;
         U    : constant Internal_Unit := Self.Safety_Net.Unit;
         Desc : constant Any_Language_Id := Self.Desc;
      begin
         --  Create an ownership share for the context embedded in the result
         --  before returning.

         Desc.Context_Inc_Ref (Ctx);
         return (Internal => U,
                 Context  => (Ada.Finalization.Controlled with
                              Desc     => Desc,
                              Internal => Ctx));
      end;
   end Unit;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Lk_Node) return Boolean is
   begin
      return Self.Internal.Node = No_Internal_Node;
   end Is_Null;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Lk_Node) return Boolean is
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);

      --  We only want to take the node into account when determining whether a
      --  node is null or not, not the rest of the entity info. In particular,
      --  for a null node, metadata might not be null (because it's a pointer
      --  to the null metadata of the specific language).
      if Left.Internal.Node = No_Internal_Node
         or else Right.Internal.Node = No_Internal_Node
      then
         return Left.Internal.Node = Right.Internal.Node;
      end if;

      return (Left.Internal.Node = Right.Internal.Node
              and then Left.Internal.Rebindings = Right.Internal.Rebindings
              and then
                --  Check that metadata values are the same
                Left.Desc.Node_Metadata_Compare
                  (Left.Internal.Metadata, Right.Internal.Metadata));
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Self : Lk_Node) return String is
   begin
      Check_Safety_Net (Self);
      return (if Self.Internal.Node = No_Internal_Node
              then "None"
              else Self.Desc.Entity_Image (Self.Internal));
   end Image;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Lk_Node;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "")
   is
      T               : constant Type_Ref := Type_Of (Node);
      Attr_Prefix     : constant String := Line_Prefix & "|";
      Children_Prefix : constant String := Line_Prefix & "|  ";
   begin
      if Node = No_Lk_Node then
         Put_Line (Line_Prefix & "None");
         return;
      end if;

      Put (Line_Prefix & Debug_Name (T));
      if Show_Slocs then
         Put ("[" & Image (Node.Sloc_Range) & "]");
      end if;

      if Node.Is_Incomplete then
         Put (" <<INCOMPLETE>>");
      end if;

      if Is_Token_Node (T) then
         Put_Line (": " & Image (Node.Text));

      elsif Is_List_Node (T) then
         if Node.Children_Count = 0 then
            Put_Line (": <empty list>");
            return;
         end if;

         New_Line;
         for Child of Node.Children loop
            if Child /= No_Lk_Node then
               Child.Print (Show_Slocs, Children_Prefix);
            end if;
         end loop;
         return;

      else
         --  This is for regular nodes: display each field

         New_Line;
         for M of Members (T) loop
            if not Is_Property (M) then
               declare
                  Child : constant Lk_Node :=
                    As_Node (Eval_Node_Member (Node, M));
                  Name  : constant String :=
                    Image (Format_Name (Member_Name (M), Lower));
               begin
                  Put (Attr_Prefix & Name & ":");
                  if Child /= No_Lk_Node then
                     New_Line;
                     Child.Print (Show_Slocs, Children_Prefix);
                  else
                     Put_Line (" <null>");
                  end if;
               end;
            end if;
         end loop;
      end if;
   end Print;

   ------------
   -- Parent --
   ------------

   function Parent (Self : Lk_Node'Class) return Lk_Node is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Wrap_Node (Self.Desc, Self.Desc.Node_Parent (Self.Internal));
   end Parent;

   -------------
   -- Parents --
   -------------

   function Parents
     (Self : Lk_Node; With_Self : Boolean := True) return Lk_Node_Array
   is
      Id : Any_Language_Id renames Self.Desc;
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);

      declare
         Parents : constant Internal_Entity_Array :=
           Id.Node_Parents (Self.Internal, With_Self);
      begin
         return Result : Lk_Node_Array (Parents'Range) do
            for I in Parents'Range loop
               Result (I) := Wrap_Node (Id, Parents (I));
            end loop;
         end return;
      end;
   end Parents;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Self : Lk_Node) return Natural is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Self.Desc.Node_Children_Count (Self.Internal.Node);
   end Children_Count;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Self            : Lk_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Lk_Node)
   is
      Internal_Result : Internal_Node;
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      Self.Desc.Node_Get_Child
        (Self.Internal.Node, Index, Index_In_Bounds, Internal_Result);
      Result := Wrap_Node (Internal_Result, Self);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child (Self : Lk_Node'Class; Index : Positive) return Lk_Node
   is
      Dummy : Boolean;
   begin
      --  The Child overload call below takes care of sanitizing Self
      return Result : Lk_Node do
         Self.Get_Child (Index, Dummy, Result);
      end return;
   end Child;

   --------------
   -- Children --
   --------------

   function Children (Self : Lk_Node) return Lk_Node_Array is
      --  The following call to Children_Count below takes care of sanitizing
      --  Self.
      Count : constant Natural := Self.Children_Count;
   begin
      return Result : Lk_Node_Array (1 .. Count) do
         for I in Result'Range loop
            Result (I) := Self.Child (I);
         end loop;
      end return;
   end Children;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (Self : Lk_Node'Class) return Lk_Node is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Wrap_Node
        (Self.Desc.Node_Fetch_Sibling (Self.Internal.Node, 1), Self);
   end Next_Sibling;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (Self : Lk_Node'Class) return Lk_Node is
   begin
      return Wrap_Node
        (Self.Desc.Node_Fetch_Sibling (Self.Internal.Node, -1), Self);
   end Previous_Sibling;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost (Self : Lk_Node) return Boolean is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Self.Desc.Node_Is_Ghost (Self.Internal.Node);
   end Is_Ghost;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Self : Lk_Node'Class) return Boolean is
   begin
      return Is_List_Node (Type_Of (Lk_Node (Self)));
   end Is_List_Node;

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Self : Lk_Node'Class) return Boolean is
   begin
      return Is_Token_Node (Type_Of (Lk_Node (Self)));
   end Is_Token_Node;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Self  : Lk_Node;
      Visit : access function (Node : Lk_Node) return Visit_Status)
      return Visit_Status
   is
      Desc : Language_Descriptor renames Self.Desc.all;

      function Process (N : Internal_Node) return Visit_Status;

      -------------
      -- Process --
      -------------

      function Process (N : Internal_Node) return Visit_Status is
         Status : Visit_Status := Into;
      begin
         if N /= No_Internal_Node then
            Status := Visit (Wrap_Node (N, Self));

            --  Skip processing the child nodes if the returned status is Over
            --  or Stop. In the former case the previous call to Visit has
            --  taken care of processing the needed children, and in the
            --  latter case we must immediately stop processing the tree.

            if Status = Into then
               for I in 1 .. Desc.Node_Children_Count (N) loop
                  declare
                     Child           : Internal_Node;
                     Index_In_Bounds : Boolean;
                  begin
                     Desc.Node_Get_Child (N, I, Index_In_Bounds, Child);
                     pragma Assert (Index_In_Bounds);
                     if Child /= No_Internal_Node then
                        Status := Process (Child);
                        exit when Status /= Into;
                     end if;
                  end;
               end loop;
            end if;
         end if;

         --  At this stage the Over status has no sense and we just continue
         --  processing the tree unless Status is Stop.

         return (if Status = Stop
                 then Stop
                 else Into);
      end Process;

   begin
      Check_Safety_Net (Self);
      return Process (Self.Internal.Node);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Self  : Lk_Node;
      Visit : access function (Node : Lk_Node) return Visit_Status)
   is
      Dummy : Visit_Status := Self.Traverse (Visit);
   begin
      null;
   end Traverse;

   -----------------
   -- Token_Start --
   -----------------

   function Token_Start (Self : Lk_Node'Class) return Lk_Token is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);

      declare
         Desc   : Language_Descriptor renames Self.Desc.all;
         Result : constant Internal_Token :=
           Desc.Node_Token_Start (Self.Internal.Node);
      begin
         return Wrap (Result, Self);
      end;
   end Token_Start;

   ---------------
   -- Token_End --
   ---------------

   function Token_End (Self : Lk_Node'Class) return Lk_Token is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);

      declare
         Desc   : Language_Descriptor renames Self.Desc.all;
         Result : constant Internal_Token :=
           Desc.Node_Token_End (Self.Internal.Node);
      begin
         return Wrap (Result, Self);
      end;
   end Token_End;

   ----------
   -- Text --
   ----------

   function Text (Self : Lk_Node) return Text_Type is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Self.Desc.Node_Text (Self.Internal.Node);
   end Text;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range (Self : Lk_Node) return Source_Location_Range is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Self.Desc.Node_Sloc_Range (Self.Internal.Node);
   end Sloc_Range;

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Self : Lk_Node) return Boolean is
      LGC : Lk_Node;
   begin
      if Is_List_Node (Type_Of (Self)) then
         LGC := (if Self.Children_Count /= 0
                 then Self.Child (Self.Children_Count)
                 else No_Lk_Node);
         return LGC /= No_Lk_Node and then LGC.Is_Incomplete;
      else
         return Self.Desc.Node_Last_Attempted_Child (Self.Internal.Node) > -1;
      end if;
   end Is_Incomplete;

   --------------
   -- Language --
   --------------

   function Language (Self : Lk_Token) return Language_Id is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);
      return Self.Desc;
   end Language;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Lk_Token) return Hash_Type is
   begin
      return Combine
        ((Hash (Self.TDH),
          Hash_Type (Self.Index.Token),
          Hash_Type (Self.Index.Trivia)));
   end Hash;

   ----------
   -- Unit --
   ----------

   function Unit (Self : Lk_Token'Class) return Lk_Unit is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);

      declare
         function "+" is new Ada.Unchecked_Conversion
           (System.Address, Internal_Unit);
         Ctx  : constant Internal_Context := Self.Safety_Net.Context;
         U    : constant Internal_Unit := +Self.TDH.Owner;
         Desc : constant Any_Language_Id := Self.Desc;
      begin
         --  Create an ownership share for the context embedded in the result
         --  before returning.

         Desc.Context_Inc_Ref (Ctx);
         return (Internal => U,
                 Context  => (Ada.Finalization.Controlled with
                              Desc     => Desc,
                              Internal => Ctx));
      end;
   end Unit;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Lk_Token) return Boolean is
   begin
      return Self.Desc = null;
   end Is_Null;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Lk_Token) return Token_Kind_Ref is
      D : Stored_Token_Data;
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);

      D := Data (Self.Index, Self.TDH.all);
      return From_Index (Self.Desc, +D.Kind);
   end Kind;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Lk_Token) return Boolean is
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      Check_Same_Unit (Left, Right);

      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Self           : Lk_Token'Class;
      Exclude_Trivia : Boolean := False) return Lk_Token
   is
      Result : Token_Or_Trivia_Index;
   begin
      Check_Safety_Net (Self);
      if Self.Is_Null then
         return No_Lk_Token;
      end if;

      Result := Next (Self.Index, Self.TDH.all, Exclude_Trivia);
      return Wrap_Token ((Self.TDH, Result), Self);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Self           : Lk_Token'Class;
      Exclude_Trivia : Boolean := False) return Lk_Token
   is
      Result : Token_Or_Trivia_Index;
   begin
      Check_Safety_Net (Self);
      if Self.Is_Null then
         return No_Lk_Token;
      end if;

      Result := Previous (Self.Index, Self.TDH.all, Exclude_Trivia);
      return Wrap_Token ((Self.TDH, Result), Self);
   end Previous;

   -----------
   -- Image --
   -----------

   function Image (Self : Lk_Token) return String is
   begin
      Check_Safety_Net (Self);
      if Self.Is_Null then
         return "<No Token>";
      end if;

      declare
         Name : constant Name_Type := Token_Kind_Name (Self.Kind);
         Text : constant Text_Type := Self.Text;
      begin
         return "<Token Kind="
                & Image (Format_Name (Name, Camel_With_Underscores))
                & " Text=" & Image (Text, With_Quotes => True) & ">";
      end;
   end Image;

   ----------
   -- Text --
   ----------

   function Text (Self : Lk_Token) return Text_Type is
      D : Stored_Token_Data;
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);

      D := Data (Self.Index, Self.TDH.all);
      return Self.TDH.Source_Buffer (D.Source_First .. D.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Lk_Token) return Text_Type is
      FD, LD : Stored_Token_Data;
   begin
      Check_Safety_Net (First);
      Check_Safety_Net (Last);
      Check_Same_Unit (First, Last);

      FD := Data (First.Index, First.TDH.all);
      LD := Data (Last.Index, First.TDH.all);
      return First.TDH.Source_Buffer (FD.Source_First .. LD.Source_Last);
   end Text;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Self : Lk_Token) return Boolean is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);
      return Self.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Self : Lk_Token) return Token_Index is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);
      return (if Self.Index.Trivia = No_Token_Index
              then Self.Index.Token
              else Self.Index.Trivia);
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range (Self : Lk_Token) return Source_Location_Range is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);
      return Sloc_Range (Self.TDH.all, Data (Self.Index, Self.TDH.all));
   end Sloc_Range;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (Left, Right : Lk_Token) return Boolean is
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      Reject_Null_Token (Left);
      Reject_Null_Token (Right);
      if Left.Desc /= Right.Desc then
         raise Precondition_Failure with "inconsistent languages";
      end if;

      return Left.Desc.Token_Is_Equivalent.all
        (Left     => (Left.TDH, Left.Index),
         Right    => (Right.TDH, Right.Index),
         Left_SN  => Left.Safety_Net,
         Right_SN => Right.Safety_Net);
   end Is_Equivalent;

   ---------------------
   -- Origin_Filename --
   ---------------------

   function Origin_Filename (Self : Lk_Token) return String is
      use GNATCOLL.VFS;
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);
      return +Self.TDH.Filename.Full_Name;
   end Origin_Filename;

   --------------------
   -- Origin_Charset --
   --------------------

   function Origin_Charset (Self : Lk_Token) return String is
      use Ada.Strings.Unbounded;
   begin
      Check_Safety_Net (Self);
      Reject_Null_Token (Self);
      return To_String (Self.TDH.Charset);
   end Origin_Charset;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Lk_Context) is
   begin
      Self.Desc := null;
      Self.Internal := No_Internal_Context;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Lk_Context) is
   begin
      if Self.Internal /= No_Internal_Context then
         Self.Desc.Context_Inc_Ref (Self.Internal);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Lk_Context) is
   begin
      if Self.Internal /= No_Internal_Context then
         Self.Desc.Context_Dec_Ref (Self.Internal);
         Self.Initialize;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Lk_Node) is
   begin
      Self.Desc := null;
      Self.Internal := No_Internal_Entity;
      Self.Safety_Net := No_Node_Safety_Net;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Lk_Node) is
   begin
      if Self.Internal.Node /= No_Internal_Node
         and then Self.Internal.Metadata /= Self.Desc.Null_Metadata
      then
         Self.Desc.Node_Metadata_Inc_Ref (Self.Internal.Metadata);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Lk_Node) is
   begin
      if Self.Internal.Node /= No_Internal_Node then
         if Self.Internal.Metadata /= Self.Desc.Null_Metadata then
            Self.Desc.Node_Metadata_Dec_Ref (Self.Internal.Metadata);
         end if;
         Self.Initialize;
      end if;
   end Finalize;

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context
     (Id : Language_Id; Context : Internal_Context) return Lk_Context
   is
   begin
      if Context = No_Internal_Context then
         return No_Lk_Context;
      end if;

      Id.Context_Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
              Desc     => Id,
              Internal => Context);
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context (Context : Lk_Context) return Internal_Context is
   begin
      return Context.Internal;
   end Unwrap_Context;

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit
     (Id : Language_Id; Unit : Internal_Unit) return Lk_Unit
   is
      Context : Internal_Context;
   begin
      if Unit = No_Internal_Unit then
         return No_Lk_Unit;
      end if;

      Context := Id.Unit_Context (Unit);
      return (Internal => Unit, Context => Wrap_Context (Id, Context));
   end Wrap_Unit;

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Lk_Unit) return Internal_Unit is
   begin
      return Unit.Internal;
   end Unwrap_Unit;

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Id : Language_Id; Node : Internal_Entity) return Lk_Node
   is
      Context : Internal_Context;
      Unit    : Internal_Unit;
   begin
      if Node.Node = No_Internal_Node then
         return No_Lk_Node;
      end if;

      Unit := Id.Node_Unit (Node.Node);
      Context := Id.Unit_Context (Unit);

      return (Ada.Finalization.Controlled with
              Id,
              Node,
              Create_Node_Safety_Net (Id, Context, Unit, Node.Rebindings));
   end Wrap_Node;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node (Node : Lk_Node) return Internal_Entity is
   begin
      Check_Safety_Net (Node);
      return Node.Internal;
   end Unwrap_Node;

   ----------------
   -- Wrap_Token --
   ----------------

   function Wrap_Token
     (Id         : Any_Language_Id;
      Token      : Internal_Token;
      Safety_Net : Token_Safety_Net) return Lk_Token is
   begin
      return (Id, Token.TDH, Token.Index, Safety_Net);
   end Wrap_Token;

   ------------------
   -- Unwrap_Token --
   ------------------

   procedure Unwrap_Token
     (Token      : Lk_Token;
      Id         : out Any_Language_Id;
      Data       : out Internal_Token;
      Safety_Net : out Token_Safety_Net) is
   begin
      Id := Token.Desc;
      Data := (Token.TDH, Token.Index);
      Safety_Net := Token.Safety_Net;
   end Unwrap_Token;

end Langkit_Support.Generic_API.Analysis;
