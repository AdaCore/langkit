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

with Langkit_Support.Adalog.Refcounted_Logic_Ref;
with Langkit_Support.Adalog.Unify;
with Langkit_Support.Adalog.Unify_LR;

generic
   type LR_Type is private;
   with function Element_Image (E : LR_Type) return String is <>;
package Langkit_Support.Adalog.Refcounted_Eq_Same is

   procedure Inc_Ref (E : LR_Type) is null;
   procedure Dec_Ref (E : in out LR_Type) is null;

   package Refs is new Refcounted_Logic_Ref (LR_Type, Element_Image);

   type Dummy_Convert_Data is null record;
   No_Data : constant Dummy_Convert_Data := (null record);

   function Convert
     (Dummy_C_Data : Dummy_Convert_Data; From : LR_Type) return LR_Type
   is (From) with Inline;

   type Dummy_Equals_Data is null record;
   No_Equals_Data : constant Dummy_Equals_Data := (null record);

   function Equals
     (Dummy_Data : Dummy_Equals_Data; L, R : LR_Type) return Boolean
   is (L = R);

   package Refcounted_Impl is new Unify
     (LR_Type, LR_Type,
      Dummy_Convert_Data, Dummy_Convert_Data, No_Data, No_Data,
      Dummy_Equals_Data, No_Equals_Data,
      Left_Var  => Refs.Refcounted_Logic_Var,
      Right_Var => Refs.Refcounted_Logic_Var,
      L_Inc_Ref => Inc_Ref,
      R_Inc_Ref => Inc_Ref,
      L_Dec_Ref => Dec_Ref,
      R_Dec_Ref => Dec_Ref);

   subtype Refcounted_Member_Array is Refcounted_Impl.Unify_Left.R_Type_Array;

   generic

      type Converter is private;
      with function Convert (Data : Converter; From : LR_Type) return LR_Type;

      Convert_Image : String := "";
      Equals_Image  : String := "";

   package Refcounted_Custom_Bind is
      package Impl is new Unify_LR
        (LR_Type, LR_Type,
         Converter, Converter,
         Convert, Convert,
         Dummy_Equals_Data, Equals,
         Convert_Image, Equals_Image,
         Refs.Refcounted_Logic_Var, Refs.Refcounted_Logic_Var,
         L_Dec_Ref => Dec_Ref,
         R_Dec_Ref => Dec_Ref);
   end Refcounted_Custom_Bind;
end Langkit_Support.Adalog.Refcounted_Eq_Same;
