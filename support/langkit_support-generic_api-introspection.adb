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

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;

--  Even though we don't directly use entities from the Internal.Descriptor
--  package, we still need to import it to get visibility over the
--  Language_Descriptor type (and access its components).

pragma Unreferenced (Langkit_Support.Internal.Descriptor);

package body Langkit_Support.Generic_API.Introspection is

   use Langkit_Support.Errors.Introspection;

   procedure Check_Type (T : Type_Ref);
   --  Raise a ``Precondition_Failure`` if ``T`` is ``No_Type_Ref``

   procedure Check_Type (Id : Language_Id; T : Type_Index);
   --  If ``T`` is not a valid type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Enum_Type (Enum : Type_Ref);
   --  If ``Enum`` is not a valid enum type, raise a ``Precondition_Failure``
   --  exception.

   procedure Check_Enum_Value (Value : Enum_Value_Ref);
   --  If ``Value`` is not a valid enum value, raise a ``Precondition_Failure``
   --  exception.

   procedure Check_Enum_Value (Enum : Type_Ref; Index : Enum_Value_Index);
   --  If ``Enum`` is not a valid enum type or if ``Index`` is not a valid
   --  value for that type, raise a ``Precondition_Failure`` exception.

   procedure Check_Array_Type (T : Type_Ref);
   --  If ``T`` is not a valid array type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Base_Struct_Type (T : Type_Ref);
   --  If ``T`` is not a valid base struct type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Struct_Type (T : Type_Ref);
   --  If ``T`` is not a valid struct type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Node_Type (Node : Type_Ref);
   --  If ``Node`` is not a valid node type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Struct_Member (Member : Struct_Member_Ref);
   --  Raise a ``Precondition_Failure`` if ``Member`` is
   --  ``No_Struct_Member_Ref``.

   procedure Check_Struct_Member
     (Id : Language_Id; Member : Struct_Member_Index);
   --  If ``Member`` is not a valid struct member for the given language, raise
   --  a ``Precondition_Failure`` exception.

   procedure Check_Struct_Member_Argument
     (Member : Struct_Member_Ref; Argument : Argument_Index);
   --  If ``Member`` is not a valid struct member for the given language or if
   --  ``Argument`` is not a valid argument for that member, raise a
   --  ``Precondition_Failure`` exception.

   ----------------
   -- Check_Type --
   ----------------

   procedure Check_Type (T : Type_Ref) is
   begin
      if T.Id = null then
         raise Precondition_Failure with "null type reference";
      end if;
   end Check_Type;

   ----------------
   -- Check_Type --
   ----------------

   procedure Check_Type (Id : Language_Id; T : Type_Index) is
   begin
      if T > Last_Type (Id) then
         raise Precondition_Failure with "invalid type index";
      end if;
   end Check_Type;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (T : Type_Ref) return String is
   begin
      Check_Type (T);
      return T.Id.Types (T.Index).Debug_Name.all;
   end Debug_Name;

   ------------------
   -- Language_For --
   ------------------

   function Language_For (T : Type_Ref) return Language_Id is
   begin
      Check_Type (T);
      return T.Id;
   end Language_For;

   --------------
   -- To_Index --
   --------------

   function To_Index (T : Type_Ref) return Type_Index is
   begin
      Check_Type (T);
      return T.Index;
   end To_Index;

   ----------------
   -- From_Index --
   ----------------

   function From_Index (Id : Language_Id; T : Type_Index) return Type_Ref is
   begin
      Check_Type (Id, T);
      return (Id, T);
   end From_Index;

   ---------------
   -- Last_Type --
   ---------------

   function Last_Type (Id : Language_Id) return Type_Index is
   begin
      return Id.Types.all'Last;
   end Last_Type;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Node : Lk_Node) return Type_Ref is
   begin
      if Node = No_Lk_Node then
         raise Precondition_Failure with "null node";
      end if;

      declare
         Id     : constant Language_Id := Language_For (Node);
         E      : constant Internal.Analysis.Internal_Entity :=
           Unwrap_Node (Node);
         Result : constant Type_Index := Id.Node_Kind (E.Node);
      begin
         return From_Index (Id, Result);
      end;
   end Type_Of;

   ------------------
   -- Is_Enum_Type --
   ------------------

   function Is_Enum_Type (T : Type_Ref) return Boolean is
   begin
      Check_Type (T);
      return T.Index in T.Id.Enum_Types.all'Range;
   end Is_Enum_Type;

   ---------------------
   -- Check_Enum_Type --
   ---------------------

   procedure Check_Enum_Type (Enum : Type_Ref) is
   begin
      if not Is_Enum_Type (Enum) then
         raise Precondition_Failure with "invalid enum type";
      end if;
   end Check_Enum_Type;

   ----------------------
   -- Check_Enum_Value --
   ----------------------

   procedure Check_Enum_Value (Value : Enum_Value_Ref) is
   begin
      if Value.Enum.Id = null then
         raise Precondition_Failure with "null enum value reference";
      end if;
   end Check_Enum_Value;

   ----------------------
   -- Check_Enum_Value --
   ----------------------

   procedure Check_Enum_Value (Enum : Type_Ref; Index : Enum_Value_Index) is
   begin
      Check_Enum_Type (Enum);
      if Index > Enum.Id.Enum_Types.all (Enum.Index).Last_Value then
         raise Precondition_Failure with "invalid enum value index";
      end if;
   end Check_Enum_Value;

   --------------------
   -- Enum_Type_Name --
   --------------------

   function Enum_Type_Name (Enum : Type_Ref) return Name_Type is
   begin
      Check_Enum_Type (Enum);
      return Create_Name (Enum.Id.Enum_Types.all (Enum.Index).Name.all);
   end Enum_Type_Name;

   --------------
   -- Enum_For --
   --------------

   function Enum_For (Value : Enum_Value_Ref) return Type_Ref is
   begin
      return Value.Enum;
   end Enum_For;

   ------------------------
   -- Enum_Default_Value --
   ------------------------

   function Enum_Default_Value (Enum : Type_Ref) return Enum_Value_Ref is
      Index : Any_Enum_Value_Index;
   begin
      Check_Enum_Type (Enum);
      Index := Enum.Id.Enum_Types.all (Enum.Index).Default_Value;
      return (if Index = No_Enum_Value_Index
              then No_Enum_Value_Ref
              else From_Index (Enum, Index));
   end Enum_Default_Value;

   ---------------------
   -- Enum_Value_Name --
   ---------------------

   function Enum_Value_Name (Value : Enum_Value_Ref) return Name_Type is
   begin
      Check_Enum_Value (Value);

      declare
         Enum : Type_Ref renames Value.Enum;
         Desc : Enum_Type_Descriptor renames
           Enum.Id.Enum_Types.all (Enum.Index).all;
      begin
         return Create_Name (Desc.Value_Names (Value.Index).all);
      end;
   end Enum_Value_Name;

   --------------
   -- To_Index --
   --------------

   function To_Index (Value : Enum_Value_Ref) return Enum_Value_Index is
   begin
      Check_Enum_Value (Value);
      return Value.Index;
   end To_Index;

   ----------------
   -- From_Index --
   ----------------

   function From_Index
     (Enum : Type_Ref; Value : Enum_Value_Index) return Enum_Value_Ref is
   begin
      Check_Enum_Value (Enum, Value);
      return (Enum, Value);
   end From_Index;

   ---------------------
   -- Enum_Last_Value --
   ---------------------

   function Enum_Last_Value (Enum : Type_Ref) return Enum_Value_Index is
   begin
      Check_Enum_Type (Enum);
      return Enum.Id.Enum_Types.all (Enum.Index).Last_Value;
   end Enum_Last_Value;

   -------------------
   -- Is_Array_Type --
   -------------------

   function Is_Array_Type (T : Type_Ref) return Boolean is
   begin
      Check_Type (T);
      return T.Index in T.Id.Array_Types.all'Range;
   end Is_Array_Type;

   ----------------------
   -- Check_Array_Type --
   ----------------------

   procedure Check_Array_Type (T : Type_Ref) is
   begin
      if not Is_Array_Type (T) then
         raise Precondition_Failure with "invalid array type";
      end if;
   end Check_Array_Type;

   ------------------------
   -- Array_Element_Type --
   ------------------------

   function Array_Element_Type (T : Type_Ref) return Type_Ref is
   begin
      Check_Array_Type (T);
      return From_Index (T.Id, T.Id.Array_Types.all (T.Index).Element_Type);
   end Array_Element_Type;

   -------------------------
   -- Is_Base_Struct_Type --
   -------------------------

   function Is_Base_Struct_Type (T : Type_Ref) return Boolean is
   begin
      return Is_Struct_Type (T) or else Is_Node_Type (T);
   end Is_Base_Struct_Type;

   ----------------------------
   -- Check_Base_Struct_Type --
   ----------------------------

   procedure Check_Base_Struct_Type (T : Type_Ref) is
   begin
      if not Is_Base_Struct_Type (T) then
         raise Precondition_Failure with "invalid base struct type";
      end if;
   end Check_Base_Struct_Type;

   ---------------------------
   -- Base_Struct_Type_Name --
   ---------------------------

   function Base_Struct_Type_Name (T : Type_Ref) return Name_Type is
   begin
      Check_Base_Struct_Type (T);
      return Create_Name (T.Id.Struct_Types.all (T.Index).Name.all);
   end Base_Struct_Type_Name;

   --------------------
   -- Is_Struct_Type --
   --------------------

   function Is_Struct_Type (T : Type_Ref) return Boolean is
   begin
      Check_Type (T);
      return T.Index in T.Id.Struct_Types.all'First .. T.Id.First_Node - 1;
   end Is_Struct_Type;

   -----------------------
   -- Check_Struct_Type --
   -----------------------

   procedure Check_Struct_Type (T : Type_Ref) is
   begin
      if not Is_Struct_Type (T) then
         raise Precondition_Failure with "invalid struct type";
      end if;
   end Check_Struct_Type;

   ----------------------
   -- Struct_Type_Name --
   ----------------------

   function Struct_Type_Name (Struct : Type_Ref) return Name_Type is
   begin
      Check_Struct_Type (Struct);
      return Create_Name (Struct.Id.Struct_Types.all (Struct.Index).Name.all);
   end Struct_Type_Name;

   ------------------
   -- Is_Node_Type --
   ------------------

   function Is_Node_Type (T : Type_Ref) return Boolean is
   begin
      Check_Type (T);
      return T.Index in T.Id.First_Node .. T.Id.Struct_Types.all'Last;
   end Is_Node_Type;

   ---------------------
   -- Check_Node_Type --
   ---------------------

   procedure Check_Node_Type (Node : Type_Ref) is
   begin
      if not Is_Node_Type (Node) then
         raise Precondition_Failure with "invalid node type";
      end if;
   end Check_Node_Type;

   --------------------
   -- Root_Node_Type --
   --------------------

   function Root_Node_Type (Id : Language_Id) return Type_Ref is
   begin
      return From_Index (Id, Id.First_Node);
   end Root_Node_Type;

   --------------------
   -- Node_Type_Name --
   --------------------

   function Node_Type_Name (Node : Type_Ref) return Name_Type is
   begin
      Check_Node_Type (Node);
      return Create_Name (Node.Id.Struct_Types.all (Node.Index).Name.all);
   end Node_Type_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Node : Type_Ref) return Boolean is
   begin
      Check_Node_Type (Node);
      return Node.Id.Struct_Types.all (Node.Index).Is_Abstract;
   end Is_Abstract;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Node : Type_Ref) return Type_Ref is
   begin
      Check_Node_Type (Node);
      if Node = Root_Node_Type (Node.Id) then
         raise Bad_Type_Error with "trying to get base type of root node";
      end if;
      return From_Index
        (Node.Id, Node.Id.Struct_Types.all (Node.Index).Base_Type);
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Node : Type_Ref) return Type_Ref_Array is
   begin
      Check_Node_Type (Node);
      declare
         Derivations : Type_Index_Array renames
           Node.Id.Struct_Types.all (Node.Index).Derivations;
      begin
         return Result : Type_Ref_Array (Derivations'Range) do
            for I in Result'Range loop
               Result (I) := From_Index (Node.Id, Derivations (I));
            end loop;
         end return;
      end;
   end Derived_Types;

   -----------------------
   -- Last_Derived_Type --
   -----------------------

   function Last_Derived_Type (Node : Type_Ref) return Type_Index is
      --  Look for the last derivations's derivation, recursively

      Result : Any_Type_Index := Node.Index;
   begin
      Check_Node_Type (Node);

      loop
         declare
            Desc : Struct_Type_Descriptor renames
              Node.Id.Struct_Types.all (Result).all;
         begin
            exit when Desc.Derivations'Length = 0;
            Result := Desc.Derivations (Desc.Derivations'Last);
         end;
      end loop;
      return Result;
   end Last_Derived_Type;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Node, Parent : Type_Ref) return Boolean is
   begin
      Check_Node_Type (Node);
      Check_Node_Type (Parent);
      if Node.Id /= Parent.Id then
         raise Precondition_Failure with
           "Node and Parent belong to different languages";
      end if;

      declare
         Id           : constant Language_Id := Node.Id;
         Struct_Types : Struct_Type_Descriptor_Array renames
           Id.Struct_Types.all;
         Cursor       : Any_Type_Index := Node.Index;
      begin
         while Cursor /= No_Type_Index loop
            if Cursor = Parent.Index then
               return True;
            end if;

            Cursor := Struct_Types (Cursor).Base_Type;
         end loop;
         return False;
      end;
   end Is_Derived_From;

   -------------------------
   -- Check_Struct_Member --
   -------------------------

   procedure Check_Struct_Member (Member : Struct_Member_Ref) is
   begin
      if Member.Id = null then
         raise Precondition_Failure with "null struct member reference";
      end if;
   end Check_Struct_Member;

   -------------------------
   -- Check_Struct_Member --
   -------------------------

   procedure Check_Struct_Member
     (Id : Language_Id; Member : Struct_Member_Index) is
   begin
      if Member not in Id.Struct_Members.all'Range then
         raise Precondition_Failure with "invalid struct member index";
      end if;
   end Check_Struct_Member;

   ----------------------------------
   -- Check_Struct_Member_Argument --
   ----------------------------------

   procedure Check_Struct_Member_Argument
     (Member : Struct_Member_Ref; Argument : Argument_Index) is
   begin
      Check_Struct_Member (Member);
      declare
         Desc : Struct_Member_Descriptor renames
           Member.Id.Struct_Members.all (Member.Index).all;
      begin
         if Argument not in Desc.Arguments'Range then
            raise Precondition_Failure with "invalid struct member argument";
         end if;
      end;
   end Check_Struct_Member_Argument;

   -----------------
   -- Is_Property --
   -----------------

   function Is_Property (Member : Struct_Member_Ref) return Boolean is
   begin
      Check_Struct_Member (Member);
      return Member.Index >= Member.Id.First_Property;
   end Is_Property;

   -------------
   -- Members --
   -------------

   function Members (Struct : Type_Ref) return Struct_Member_Ref_Array is
      Id : Language_Id;

      Current_Struct : Any_Type_Index := Struct.Index;
      --  Cursor to "climb up" the derivation hierarchy for ``Struct``: we want
      --  ``Struct``'s own fields, but also the inheritted ones.

      Next : Natural;
      --  Index in ``Result`` (see below) for the next member to add
   begin
      Check_Base_Struct_Type (Struct);
      Id := Struct.Id;
      return Result : Struct_Member_Ref_Array
        (1 .. Id.Struct_Types.all (Struct.Index).Inherited_Members)
      do
         --  Go through the derivation chain and collect field in ``Result``.
         --  Add them in reverse order so that in the end, inherited members
         --  are first, and are in declaration order.

         Next := Result'Last;
         while Current_Struct /= No_Type_Index loop
            for M of reverse Id.Struct_Types.all (Current_Struct).Members loop
               Result (Next) := From_Index (Id, M);
               Next := Next - 1;
            end loop;
            Current_Struct := Id.Struct_Types.all (Current_Struct).Base_Type;
         end loop;
      end return;
   end Members;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Member : Struct_Member_Ref) return Name_Type is
   begin
      Check_Struct_Member (Member);
      return Create_Name
        (Member.Id.Struct_Members.all (Member.Index).Name.all);
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type (Member : Struct_Member_Ref) return Type_Ref is
   begin
      Check_Struct_Member (Member);
      return From_Index
        (Member.Id, Member.Id.Struct_Members.all (Member.Index).Member_Type);
   end Member_Type;

   --------------
   -- To_Index --
   --------------

   function To_Index (Member : Struct_Member_Ref) return Struct_Member_Index is
   begin
      Check_Struct_Member (Member);
      return Member.Index;
   end To_Index;

   ----------------
   -- From_Index --
   ----------------

   function From_Index
     (Id : Language_Id; Member : Struct_Member_Index) return Struct_Member_Ref
   is
   begin
      Check_Struct_Member (Id, Member);
      return (Id, Member);
   end From_Index;

   ------------------------
   -- Last_Struct_Member --
   ------------------------

   function Last_Struct_Member (Id : Language_Id) return Struct_Member_Index is
   begin
      return Id.Struct_Members.all'Last;
   end Last_Struct_Member;

   --------------------------
   -- Member_Argument_Type --
   --------------------------

   function Member_Argument_Type
     (Member : Struct_Member_Ref; Argument : Argument_Index) return Type_Ref
   is
      Id : Language_Id;
   begin
      Check_Struct_Member (Member);
      Check_Struct_Member_Argument (Member, Argument);
      Id := Member.Id;
      return From_Index
        (Id,
         Id.Struct_Members.all
           (Member.Index).Arguments (Argument).Argument_Type);
   end Member_Argument_Type;

   --------------------------
   -- Member_Argument_Name --
   --------------------------

   function Member_Argument_Name
     (Member : Struct_Member_Ref; Argument : Argument_Index) return Name_Type
   is
   begin
      Check_Struct_Member (Member);
      Check_Struct_Member_Argument (Member, Argument);
      return Create_Name
        (Member.Id.Struct_Members.all
           (Member.Index).Arguments (Argument).Name.all);
   end Member_Argument_Name;

   --------------------------
   -- Member_Last_Argument --
   --------------------------

   function Member_Last_Argument
     (Member : Struct_Member_Ref) return Any_Argument_Index is
   begin
      Check_Struct_Member (Member);
      return Member.Id.Struct_Members.all (Member.Index).Last_Argument;
   end Member_Last_Argument;

end Langkit_Support.Generic_API.Introspection;
