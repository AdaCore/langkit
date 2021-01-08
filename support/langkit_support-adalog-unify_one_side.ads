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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Relations;

--  Internal implementation package, not to be used directly by users a-priori.
--  This package implements what we call "one side" simple relations, which
--  means:
--
--  1. They're simple because they're not composed of several sub-relations.
--  2. They're "one side" because only one logic variable is involved.
--

generic
   type L_Type is private;
   type R_Type is private;

   type R_Convert_Data is private;
   --  Private type containing data associated to the Conversion function. Not
   --  necessary but useful if your conversion function has state.

   with function Convert
     (C_Data : R_Convert_Data; From : R_Type) return L_Type is <>;
   --  Conversion function, to get an L_Type from an R_Type

   type Equals_Data is private;
   --  Private type containing data associated to the Equals function. Not
   --  necessary, but useful if your equality function has state.

   with function Equals (Data : Equals_Data; L, R : L_Type) return Boolean
   is <>;

   Convert_Image : String := "";
   Equals_Image  : String := "";

   with package Var is new Logic_Var (Element_Type => L_Type, others => <>);
   --  Logic variable formal package

   with function R_Image (Self : R_Type) return String is <>;
   with function L_Image (Self : L_Type) return String is <>;
   --  Images functions for element types. Used for debugging

   Invert_Equals : Boolean := False;
   --  By default, equality on L and R value are done via Equals (Convert (R),
   --  L) If this is passed, it will be done by Equals (L, Convert (R)). This
   --  can be useful if order is important in your equality function.

   with procedure R_Inc_Ref (R : R_Type);
   with procedure L_Dec_Ref (L : in out L_Type);
   with procedure R_Dec_Ref (R : in out R_Type);

package Langkit_Support.Adalog.Unify_One_Side is

   type R_Type_Array is array (Positive range <>) of R_Type;

   -----------
   -- Unify --
   -----------

   --  Unify is a simple relation that will ensure that the logic variable
   --  it binds is equal to a certain value. It is like a version of "Member"
   --  above, but restricted to a length 1 domain.

   type Unify is new Base_Relation with private;
   function Create
     (Left      : Var.Var;
      Right     : R_Type;
      R_Data    : R_Convert_Data;
      Eq_Data   : Equals_Data;
      Sloc_Info : String_Access := null) return Relation;

   ------------
   -- Member --
   ------------

   --  Member is a simple relation that ensures that the value of a logic
   --  variable is in a certain domain.

   --  TODO??? Why is member not implemented in terms of
   --  Relation.Stateful_Relation?

   type R_Type_Array_Access is access all R_Type_Array;
   type Member_T is new Base_Relation with private;

   function Member
     (R      : Var.Var;
      Vals   : R_Type_Array;
      R_Data : R_Convert_Data;
      Eq_Data : Equals_Data) return Relation;

   overriding function Solve_Impl
     (Self    : in out Member_T;
      Context : in out Solving_Context) return Solving_State;
   overriding procedure Reset (Self : in out Member_T);
   overriding procedure Cleanup (Self : in out Member_T);
   overriding function Custom_Image (Self : Member_T) return String;

private

   -----------
   -- Unify --
   -----------

   type Unify_Rec is record
      Left    : Var.Var;
      Right   : R_Type;
      Changed : Boolean := False;
      R_Data  : R_Convert_Data;
      Eq_Data : Equals_Data;
   end record;

   function Apply (Self : in out Unify_Rec) return Solving_State;
   procedure Revert (Self : in out Unify_Rec);
   procedure Free (Self : in out Unify_Rec);

   function Custom_Image (Self : Unify_Rec) return String;

   package Rel is new Relations.Stateful_Relation (Unify_Rec);
   type Unify is new Rel.Rel with null record;

   type Member_T is new Base_Relation with record
      Left           : Var.Var;
      --  Logic variable that must be one of the given values

      Values         : R_Type_Array_Access;
      --  Possible set of values for the logic variable

      Current_Index  : Positive := 1;
      --  Index in Values of the next value to try to assign to Left

      Changed        : Boolean := False;
      --  Whether we assigned a value to Left last time Solve was called

      Domain_Checked : Boolean := False;
      --  Whether the last time Solve was called, we checked the value of Left
      --  without assigning it ourselves.

      R_Data         : R_Convert_Data;
      --  Data to convert a value from Values into a value that can be assigned
      --  to Left.

      Eq_Data        : Equals_Data;
      --  Data to check values equality
   end record;

end Langkit_Support.Adalog.Unify_One_Side;
