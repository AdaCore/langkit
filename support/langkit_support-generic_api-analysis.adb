------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Langkit_Support.Errors;       use Langkit_Support.Errors;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Types;        use Langkit_Support.Types;

package body Langkit_Support.Generic_API.Analysis is

   function "+" is new Ada.Unchecked_Conversion
     (Language_Descriptor_Access, Language_Id);

   function Create_Node_Safety_Net
     (Unit       : Lk_Unit'Class;
      Rebindings : Env_Rebindings) return Node_Safety_Net;
   --  Create a safety net for a node, to make sure that later access to that
   --  node does not use a stale unit or stale rebindings.

   procedure Check_Safety_Net (Node : Lk_Node'Class);
   --  Check that the unit and rebindings referenced by Node are not stale.
   --  Raise a Stale_Reference_Error exception if one of them are stale.

   procedure Reject_Null_Context (Context : Lk_Context'Class);
   procedure Reject_Null_Unit (Unit : Lk_Unit'Class);
   procedure Reject_Null_Node (Node : Lk_Node'Class);
   --  Raise a Precondition_Failure exception if Context/Unit/Node is null

   function Wrap (Node : Internal_Node; Unit : Lk_Unit'Class) return Lk_Node;
   --  Return a public node to wrap Node, given an existing non-null reference
   --  to its owning Unit.

   function Wrap_Node
     (Node : Internal_Node; Origin_Node : Lk_Node'Class) return Lk_Node;
   --  Return a public node to wrap Node. Use safety net and entity info from
   --  Origin_Node.

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

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Node : Lk_Node'Class) is
      Desc       : constant Language_Descriptor_Access := Node.Desc;
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

   ----------
   -- Wrap --
   ----------

   function Wrap (Node : Internal_Node; Unit : Lk_Unit'Class) return Lk_Node is
      Desc       : constant Language_Descriptor_Access := Unit.Context.Desc;
      Entity     : constant Internal_Entity :=
        (Node, null, False, No_Internal_Node_Metadata);
      Safety_Net : constant Node_Safety_Net :=
        Create_Node_Safety_Net (Unit, null);
   begin
      return (Ada.Finalization.Controlled with
              Desc, Entity, Safety_Net);
   end Wrap;

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
      if E.Metadata /= No_Internal_Node_Metadata then
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
              Desc     => Language_Descriptor_Access (Language),
              Internal => Result);
   end Create_Context;

   ------------------
   -- Language_For --
   ------------------

   function Language_For (Self : Lk_Context'Class) return Language_Id is
   begin
      Reject_Null_Context (Self);
      return +Self.Desc;
   end Language_For;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Self : Lk_Context'Class; Unit_Filename : String) return Boolean is
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

   ----------
   -- Root --
   ----------

   function Root (Self : Lk_Unit'Class) return Lk_Node is
   begin
      Reject_Null_Unit (Self);

      declare
         Desc : Language_Descriptor renames Self.Context.Desc.all;
         Result : constant Internal_Node := Desc.Unit_Root (Self.Internal);
      begin
         return Wrap (Result, Self);
      end;
   end Root;

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
         Desc : constant Language_Descriptor_Access := Self.Desc;
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

   function Is_Null (Self : Lk_Node'Class) return Boolean is
   begin
      return Self.Internal.Node = No_Internal_Node;
   end Is_Null;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Lk_Node'Class) return Boolean is
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      return (Left.Internal.Node = Right.Internal.Node
              and then Left.Internal.Rebindings = Right.Internal.Rebindings);
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Self : Lk_Node'Class) return String is
   begin
      Check_Safety_Net (Self);
      return (if Self.Internal.Node = No_Internal_Node
              then "None"
              else Self.Desc.Entity_Image (Self.Internal));
   end Image;

   ------------
   -- Parent --
   ------------

   function Parent (Self : Lk_Node'Class) return Lk_Node is
   begin
      Check_Safety_Net (Self);
      Reject_Null_Node (Self);
      return Wrap_Node (Self.Desc.Node_Parent (Self.Internal.Node), Self);
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Self : Lk_Node'Class) return Natural is
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

   function Children (Self : Lk_Node'Class) return Lk_Node_Array is
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
   -- Traverse --
   --------------

   function Traverse
     (Self  : Lk_Node'Class;
      Visit : access function (Node : Lk_Node'Class) return Visit_Status)
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
            --  taken care of processing the needed childs, and in the latter
            --  case we must immediately stop processing the tree.

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
     (Self  : Lk_Node'Class;
      Visit : access function (Node : Lk_Node'Class) return Visit_Status)
   is
      Dummy : Visit_Status := Self.Traverse (Visit);
   begin
      null;
   end Traverse;

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
         and then Self.Internal.Metadata /= No_Internal_Node_Metadata
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
         if Self.Internal.Metadata /= No_Internal_Node_Metadata then
            Self.Desc.Node_Metadata_Dec_Ref (Self.Internal.Metadata);
         end if;
         Self.Initialize;
      end if;
   end Finalize;

end Langkit_Support.Generic_API.Analysis;
