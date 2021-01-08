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
with Langkit_Support.Adalog.Logic_Ref;
with Langkit_Support.Adalog.Unify;

--  Convenience wrapper generic package that, from a type implementing
--  equality, will instantiate all the needed stuff to create logical
--  equations containing that type, namely:
--
--  * A Logic_Ref instantiation so that you can have logical variables holding
--  values of the type.
--
--  * An instantiation of Unify, so that you can do logical equality comparison
--  between logic references and real instances of the type.

generic
   type LR_Type is private;
   with function Element_Image (E : LR_Type) return String is <>;
   with procedure Inc_Ref (E : LR_Type) is null;
   with procedure Dec_Ref (E : in out LR_Type) is null;
package Langkit_Support.Adalog.Eq_Same is

   package Refs is new Logic_Ref (LR_Type, Inc_Ref, Dec_Ref, Element_Image);

   type Dummy_Convert_Data is null record;
   No_Data : constant Dummy_Convert_Data := (null record);

   function Convert
     (C_Data : Dummy_Convert_Data; From : LR_Type) return LR_Type
      with Inline;

   type Dummy_Equals_Data is null record;
   No_Equals_Data : constant Dummy_Equals_Data := (null record);

   function Equals
     (Dummy_Data : Dummy_Equals_Data; L, R : LR_Type) return Boolean
   is (L = R);

   package Raw_Impl is new Unify
     (LR_Type, LR_Type,
      Dummy_Convert_Data, Dummy_Convert_Data, No_Data, No_Data,
      Dummy_Equals_Data, No_Equals_Data,
      Left_Var  => Refs.Raw_Logic_Var,
      Right_Var => Refs.Raw_Logic_Var,
      L_Inc_Ref => Inc_Ref,
      R_Inc_Ref => Inc_Ref,
      L_Dec_Ref => Dec_Ref,
      R_Dec_Ref => Dec_Ref);

   subtype Raw_Member_Array is Raw_Impl.Unify_Left.R_Type_Array;

   --  This package can be used to provide custom bind operations, with a
   --  custom conversion from LR_Type to LR_Type.

   generic
      type Converter is private;
      No_Data : Converter;

      type Equals_Data is private;
      No_Equals_Data : Equals_Data;

      with function Convert (Data : Converter; From : LR_Type) return LR_Type;
      with function Equals
        (Eq_Data : Equals_Data; L, R : LR_Type) return Boolean is <>;

      Convert_Image : String := "";
      Equals_Image  : String := "";

      One_Side_Convert : Boolean := False;

   package Raw_Custom_Bind is

      package Impl is new Unify
        (LR_Type, LR_Type,
         Converter, Converter, No_Data, No_Data,
         Equals_Data, No_Equals_Data,
         Convert, Convert, Equals, Equals,
         Convert_Image, Equals_Image,
         Refs.Raw_Logic_Var, Refs.Raw_Logic_Var,
         L_Inc_Ref => Inc_Ref,
         R_Inc_Ref => Inc_Ref,
         L_Dec_Ref => Dec_Ref,
         R_Dec_Ref => Dec_Ref,
         One_Side_Convert => One_Side_Convert);

      function Create
        (L, R      : Refs.Raw_Logic_Var.Var;
         Data      : Converter;
         Eq_Data   : Equals_Data;
         Sloc_Info : String_Access := null) return Relation
      is
        (Impl.Equals (L, R, Data, Data, Eq_Data, Sloc_Info));

      function Create
        (L         : Refs.Raw_Logic_Var.Var;
         R         : LR_Type;
         Data      : Converter;
         Eq_Data   : Equals_Data;
         Sloc_Info : String_Access := null) return Relation
      is
        (Impl.Equals (L, R, Data, Eq_Data, Sloc_Info));

      function Create
        (L         : LR_Type;
         R         : Refs.Raw_Logic_Var.Var;
         Data      : Converter;
         Eq_Data   : Equals_Data;
         Sloc_Info : String_Access := null) return Relation
      is
        (Impl.Equals (L, R, Data, Eq_Data, Sloc_Info));

   end Raw_Custom_Bind;

end Langkit_Support.Adalog.Eq_Same;
