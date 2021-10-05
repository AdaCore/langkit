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

   procedure Check_Value_Type (Id : Language_Id; T : Value_Type);
   --  If ``T`` is not a valid value type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Enum_Type (Id : Language_Id; Enum : Value_Type);
   --  If ``Enum`` is not a valid enum type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Enum_Value
     (Id : Language_Id; Enum : Value_Type; Index : Enum_Value_Index);
   --  If ``Enum`` is not a valid enum type for the given language or if
   --  ``Index`` is not a valid value for that type, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Array_Type (Id : Language_Id; T : Value_Type);
   --  If ``T`` is not a valid array type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Base_Struct_Type (Id : Language_Id; T : Value_Type);
   --  If ``T`` is not a valid base struct type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Struct_Type (Id : Language_Id; T : Value_Type);
   --  If ``T`` is not a valid struct type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Node_Type (Id : Language_Id; Node : Value_Type);
   --  If ``Node`` is not a valid node type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Struct_Member (Id : Language_Id; Member : Struct_Member);
   --  If ``Member`` is not a valid struct member for the given language, raise
   --  a ``Precondition_Failure`` exception.

   procedure Check_Struct_Member_Argument
     (Id : Language_Id; Member : Struct_Member; Argument : Argument_Index);
   --  If ``Member`` is not a valid struct member for the given language or if
   --  ``Argument`` is not a valid argument for that member, raise a
   --  ``Precondition_Failure`` exception.

   ---------------------
   -- Last_Value_Type --
   ---------------------

   function Last_Value_Type (Id : Language_Id) return Value_Type is
   begin
      return Id.Value_Types.all'Last;
   end Last_Value_Type;

   ----------------------
   -- Check_Value_Type --
   ----------------------

   procedure Check_Value_Type (Id : Language_Id; T : Value_Type) is
   begin
      if T > Last_Value_Type (Id) then
         raise Precondition_Failure with "invalid value type";
      end if;
   end Check_Value_Type;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Id : Language_Id; T : Value_Type) return String is
   begin
      Check_Value_Type (Id, T);
      return Id.Value_Types (T).Debug_Name.all;
   end Debug_Name;

   ------------------
   -- Is_Enum_Type --
   ------------------

   function Is_Enum_Type (Id : Language_Id; T : Value_Type) return Boolean is
   begin
      Check_Value_Type (Id, T);
      return T in Id.Enum_Types.all'Range;
   end Is_Enum_Type;

   ---------------------
   -- Check_Enum_Type --
   ---------------------

   procedure Check_Enum_Type (Id : Language_Id; Enum : Value_Type) is
   begin
      if not Is_Enum_Type (Id, Enum) then
         raise Precondition_Failure with "invalid enum type";
      end if;
   end Check_Enum_Type;

   ----------------------
   -- Check_Enum_Value --
   ----------------------

   procedure Check_Enum_Value
     (Id : Language_Id; Enum : Value_Type; Index : Enum_Value_Index)
   is
   begin
      Check_Enum_Type (Id, Enum);
      if Index > Id.Enum_Types.all (Enum).Last_Value then
         raise Precondition_Failure with "invalid enum value index";
      end if;
   end Check_Enum_Value;

   --------------------
   -- Enum_Type_Name --
   --------------------

   function Enum_Type_Name
     (Id : Language_Id; Enum : Value_Type) return Name_Type is
   begin
      Check_Enum_Type (Id, Enum);
      return Create_Name (Id.Enum_Types.all (Enum).Name.all);
   end Enum_Type_Name;

   ---------------------
   -- Enum_Last_Value --
   ---------------------

   function Enum_Last_Value
     (Id : Language_Id; Enum : Value_Type) return Enum_Value_Index
   is
   begin
      Check_Enum_Type (Id, Enum);
      return Id.Enum_Types.all (Enum).Last_Value;
   end Enum_Last_Value;

   ------------------------
   -- Enum_Default_Value --
   ------------------------

   function Enum_Default_Value
     (Id : Language_Id; Enum : Value_Type) return Any_Enum_Value_Index
   is
   begin
      Check_Enum_Type (Id, Enum);
      return Id.Enum_Types.all (Enum).Default_Value;
   end Enum_Default_Value;

   ---------------------
   -- Enum_Value_Name --
   ---------------------

   function Enum_Value_Name
     (Id    : Language_Id;
      Enum  : Value_Type;
      Index : Enum_Value_Index) return Name_Type is
   begin
      Check_Enum_Value (Id, Enum, Index);
      return Create_Name (Id.Enum_Types.all (Enum).Value_Names (Index).all);
   end Enum_Value_Name;

   -------------------
   -- Is_Array_Type --
   -------------------

   function Is_Array_Type (Id : Language_Id; T : Value_Type) return Boolean is
   begin
      Check_Value_Type (Id, T);
      return T in Id.Array_Types.all'Range;
   end Is_Array_Type;

   ----------------------
   -- Check_Array_Type --
   ----------------------

   procedure Check_Array_Type (Id : Language_Id; T : Value_Type) is
   begin
      if not Is_Array_Type (Id, T) then
         raise Precondition_Failure with "invalid array type";
      end if;
   end Check_Array_Type;

   ------------------------
   -- Array_Element_Type --
   ------------------------

   function Array_Element_Type
     (Id : Language_Id; T : Value_Type) return Value_Type is
   begin
      Check_Array_Type (Id, T);
      return Id.Array_Types.all (T).Element_Type;
   end Array_Element_Type;

   -------------------------
   -- Is_Base_Struct_Type --
   -------------------------

   function Is_Base_Struct_Type
     (Id : Language_Id; T : Value_Type) return Boolean
   is
   begin
      return Is_Struct_Type (Id, T) or else Is_Node_Type (Id, T);
   end Is_Base_Struct_Type;

   ----------------------------
   -- Check_Base_Struct_Type --
   ----------------------------

   procedure Check_Base_Struct_Type (Id : Language_Id; T : Value_Type) is
   begin
      if not Is_Base_Struct_Type (Id, T) then
         raise Precondition_Failure with "invalid base struct type";
      end if;
   end Check_Base_Struct_Type;

   ---------------------------
   -- Base_Struct_Type_Name --
   ---------------------------

   function Base_Struct_Type_Name
     (Id : Language_Id; T : Value_Type) return Name_Type is
   begin
      Check_Base_Struct_Type (Id, T);
      return Create_Name (Id.Struct_Types.all (T).Name.all);
   end Base_Struct_Type_Name;

   --------------------
   -- Is_Struct_Type --
   --------------------

   function Is_Struct_Type (Id : Language_Id; T : Value_Type) return Boolean is
   begin
      Check_Value_Type (Id, T);
      return T in Id.Struct_Types.all'First .. Id.First_Node - 1;
   end Is_Struct_Type;

   -----------------------
   -- Check_Struct_Type --
   -----------------------

   procedure Check_Struct_Type (Id : Language_Id; T : Value_Type) is
   begin
      if not Is_Struct_Type (Id, T) then
         raise Precondition_Failure with "invalid struct type";
      end if;
   end Check_Struct_Type;

   ----------------------
   -- Struct_Type_Name --
   ----------------------

   function Struct_Type_Name
     (Id : Language_Id; Struct : Value_Type) return Name_Type is
   begin
      Check_Struct_Type (Id, Struct);
      return Create_Name (Id.Struct_Types.all (Struct).Name.all);
   end Struct_Type_Name;

   ------------------
   -- Is_Node_Type --
   ------------------

   function Is_Node_Type (Id : Language_Id; T : Value_Type) return Boolean is
   begin
      Check_Value_Type (Id, T);
      return T in Id.First_Node .. Id.Struct_Types.all'Last;
   end Is_Node_Type;

   ---------------------
   -- Check_Node_Type --
   ---------------------

   procedure Check_Node_Type (Id : Language_Id; Node : Value_Type) is
   begin
      if not Is_Node_Type (Id, Node) then
         raise Precondition_Failure with "invalid node type";
      end if;
   end Check_Node_Type;

   --------------------
   -- Root_Node_Type --
   --------------------

   function Root_Node_Type (Id : Language_Id) return Value_Type is
   begin
      return Id.First_Node;
   end Root_Node_Type;

   --------------------
   -- Node_Type_Name --
   --------------------

   function Node_Type_Name
     (Id : Language_Id; Node : Value_Type) return Name_Type is
   begin
      Check_Node_Type (Id, Node);
      return Create_Name (Id.Struct_Types.all (Node).Name.all);
   end Node_Type_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract
     (Id : Language_Id; Node : Value_Type) return Boolean is
   begin
      Check_Node_Type (Id, Node);
      return Id.Struct_Types.all (Node).Is_Abstract;
   end Is_Abstract;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type
     (Id : Language_Id; Node : Value_Type) return Value_Type is
   begin
      Check_Node_Type (Id, Node);
      if Node = Root_Node_Type (Id) then
         raise Bad_Type_Error with "trying to get base type of root node";
      end if;
      return Id.Struct_Types.all (Node).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types
     (Id : Language_Id; Node : Value_Type) return Value_Type_Array is
   begin
      Check_Node_Type (Id, Node);
      return Id.Struct_Types.all (Node).Derivations;
   end Derived_Types;

   -----------------------
   -- Last_Derived_Type --
   -----------------------

   function Last_Derived_Type
     (Id : Language_Id; Node : Value_Type) return Value_Type
   is
      --  Look for the last derivations's derivation, recursively

      Result : Value_Type := Node;
   begin
      Check_Node_Type (Id, Node);

      loop
         declare
            Desc : Struct_Type_Descriptor renames
              Id.Struct_Types.all (Result).all;
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

   function Is_Derived_From
     (Id : Language_Id; Node, Parent : Value_Type) return Boolean
   is
      Struct_Types : Struct_Type_Descriptor_Array renames Id.Struct_Types.all;
      Cursor       : Any_Value_Type := Node;
   begin
      Check_Node_Type (Id, Node);
      Check_Node_Type (Id, Parent);

      while Cursor /= No_Value_Type loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Struct_Types (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

   -------------------------
   -- Check_Struct_Member --
   -------------------------

   procedure Check_Struct_Member (Id : Language_Id; Member : Struct_Member) is
   begin
      if Member not in Id.Struct_Members.all'Range then
         raise Precondition_Failure with "invalid struct member";
      end if;
   end Check_Struct_Member;

   ----------------------------------
   -- Check_Struct_Member_Argument --
   ----------------------------------

   procedure Check_Struct_Member_Argument
     (Id : Language_Id; Member : Struct_Member; Argument : Argument_Index)
   is
   begin
      Check_Struct_Member (Id, Member);
      if Argument not in Id.Struct_Members.all (Member).Arguments'Range then
         raise Precondition_Failure with "invalid struct member argument";
      end if;
   end Check_Struct_Member_Argument;

   -----------------
   -- Is_Property --
   -----------------

   function Is_Property
     (Id : Language_Id; Member : Struct_Member) return Boolean is
   begin
      Check_Struct_Member (Id, Member);
      return Member >= Id.First_Property;
   end Is_Property;

   -------------
   -- Members --
   -------------

   function Members
     (Id : Language_Id; Struct : Value_Type) return Struct_Member_Array
   is
      Current_Struct : Any_Value_Type := Struct;
      --  Cursor to "climb up" the derivation hierarchy for ``Struct``: we want
      --  ``Struct``'s own fields, but also the inheritted ones.

      Next : Natural;
      --  Index in ``Result`` (see below) for the next member to add
   begin
      Check_Base_Struct_Type (Id, Struct);

      return Result : Struct_Member_Array
        (1 .. Id.Struct_Types.all (Struct).Inherited_Members)
      do
         --  Go through the derivation chain and collect field in ``Result``.
         --  Add them in reverse order so that in the end, inherited members
         --  are first, and are in declaration order.

         Next := Result'Last;
         while Current_Struct /= No_Value_Type loop
            for M of reverse Id.Struct_Types.all (Current_Struct).Members loop
               Result (Next) := M;
               Next := Next - 1;
            end loop;
            Current_Struct := Id.Struct_Types.all (Current_Struct).Base_Type;
         end loop;
      end return;
   end Members;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name
     (Id : Language_Id; Member : Struct_Member) return Name_Type is
   begin
      Check_Struct_Member (Id, Member);
      return Create_Name (Id.Struct_Members.all (Member).Name.all);
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type
     (Id : Language_Id; Member : Struct_Member) return Value_Type is
   begin
      Check_Struct_Member (Id, Member);
      return Id.Struct_Members.all (Member).Member_Type;
   end Member_Type;

   --------------------------
   -- Member_Last_Argument --
   --------------------------

   function Member_Last_Argument
     (Id : Language_Id; Member : Struct_Member) return Any_Argument_Index is
   begin
      Check_Struct_Member (Id, Member);
      return Id.Struct_Members.all (Member).Last_Argument;
   end Member_Last_Argument;

   --------------------------
   -- Member_Argument_Type --
   --------------------------

   function Member_Argument_Type
     (Id       : Language_Id;
      Member   : Struct_Member;
      Argument : Argument_Index) return Value_Type is
   begin
      Check_Struct_Member_Argument (Id, Member, Argument);
      return Id.Struct_Members.all (Member).Arguments (Argument).Argument_Type;
   end Member_Argument_Type;

   --------------------------
   -- Member_Argument_Name --
   --------------------------

   function Member_Argument_Name
     (Id       : Language_Id;
      Member   : Struct_Member;
      Argument : Argument_Index) return Name_Type is
   begin
      Check_Struct_Member_Argument (Id, Member, Argument);
      return Create_Name
        (Id.Struct_Members.all (Member).Arguments (Argument).Name.all);
   end Member_Argument_Name;

end Langkit_Support.Generic_API.Introspection;
