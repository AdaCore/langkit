------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;
with Langkit_Support.Adalog.Unify_LR;
with Langkit_Support.Adalog.Unify_One_Side;

--  This package makes the unification algorithms of Adalog accessible under
--  the form of equality operators, and a membership function. Equality works
--  between logic variables or between logic variables and real values. It is
--  the base building block of constructing equations in Adalog.

--  See Eq_Same for an example of use

generic
   type L_Type is private;
   type R_Type is private;

   type Left_C_Data is private;
   type Right_C_Data is private;

   No_L_Data : Left_C_Data;
   No_R_Data : Right_C_Data;

   type Equals_Data is private;
   No_Equals_Data : Equals_Data;

   with function Convert
     (C_Data : Right_C_Data; From : R_Type) return L_Type is <>;
   with function Convert
     (C_Data : Left_C_Data; From : L_Type) return R_Type is <>;

   with function Image (Data : Right_C_Data) return String is <>;
   with function Image (Data : Left_C_Data) return String is <>;

   with function Equals (Data : Equals_Data; L, R : L_Type) return Boolean
   is <>;

   with function Equals (Data : Equals_Data; L, R : R_Type) return Boolean
     is <>;

   with function Image (Data : Equals_Data) return String is <>;

   with package Left_Var is new Logic_Var (Value_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Value_Type => R_Type, others => <>);

   with procedure L_Inc_Ref (Self : L_Type);
   with procedure R_Inc_Ref (Self : R_Type);
   with procedure Left_C_Data_Inc_Ref (Self : Left_C_Data);
   with procedure Right_C_Data_Inc_Ref (Self : Right_C_Data);
   with procedure Equals_Data_Inc_Ref (Self : Equals_Data);
   with procedure L_Dec_Ref (Self : in out L_Type);
   with procedure R_Dec_Ref (Self : in out R_Type);
   with procedure Left_C_Data_Dec_Ref (Self : in out Left_C_Data);
   with procedure Right_C_Data_Dec_Ref (Self : in out Right_C_Data);
   with procedure Equals_Data_Dec_Ref (Self : in out Equals_Data);

   One_Side_Convert : Boolean := False;

package Langkit_Support.Adalog.Unify is

   --  TODO HACK FIXME??? P418-022 Removing the body for this package causes a
   --  generic instantiation error.
   procedure What;

   package Simple_Unify is new Adalog.Unify_LR
     (L_Type, R_Type, Left_C_Data, Right_C_Data,
      Convert, Convert, Image,
      Equals_Data, Equals, Image,
      Left_Var, Right_Var,
      Left_C_Data_Inc_Ref,
      Right_C_Data_Inc_Ref,
      Equals_Data_Inc_Ref,
      L_Dec_Ref, R_Dec_Ref,
      Left_C_Data_Dec_Ref,
      Right_C_Data_Dec_Ref,
      Equals_Data_Dec_Ref,
      One_Side_Convert => One_Side_Convert);
   use Simple_Unify;

   package Unify_Left is new Unify_One_Side
     (L_Type, R_Type,
      Right_C_Data, Convert, Image,
      Equals_Data, Equals, Image,
      Left_Var, Right_Var.Value_Image, Left_Var.Value_Image,
      Invert_Equals          => False,
      R_Inc_Ref              => R_Inc_Ref,
      R_Convert_Data_Inc_Ref => Right_C_Data_Inc_Ref,
      Equals_Data_Inc_Ref    => Equals_Data_Inc_Ref,
      L_Dec_Ref              => L_Dec_Ref,
      R_Dec_Ref              => R_Dec_Ref,
      R_Convert_Data_Dec_Ref => Right_C_Data_Dec_Ref,
      Equals_Data_Dec_Ref    => Equals_Data_Dec_Ref);

   package Unify_Right is new Unify_One_Side
     (R_Type, L_Type,
      Left_C_Data, Convert, Image,
      Equals_Data, Equals, Image,
      Right_Var, Left_Var.Value_Image, Right_Var.Value_Image,
      Invert_Equals => True,
      R_Inc_Ref              => L_Inc_Ref,
      R_Convert_Data_Inc_Ref => Left_C_Data_Inc_Ref,
      Equals_Data_Inc_Ref    => Equals_Data_Inc_Ref,
      L_Dec_Ref              => R_Dec_Ref,
      R_Dec_Ref              => L_Dec_Ref,
      R_Convert_Data_Dec_Ref => Left_C_Data_Dec_Ref,
      Equals_Data_Dec_Ref    => Equals_Data_Dec_Ref);

   ------------------
   -- Eq predicate --
   ------------------

   function Equals
     (L         : Left_Var.Logic_Var;
      R         : Right_Var.Logic_Var;
      L_Data    : Left_C_Data := No_L_Data;
      R_Data    : Right_C_Data := No_R_Data;
      Eq_Data   : Equals_Data := No_Equals_Data;
      Sloc_Info : String_Access := null) return Relation
   is
     (new Unify_LR_Rel.Rel'
        (Rel    => Create (L, R, L_Data, R_Data, Eq_Data),
         Sloc_Info => Sloc_Info,
         others => <>))
      with Inline;

   function Equals
     (L         : Left_Var.Logic_Var;
      R         : R_Type;
      R_Data    : Right_C_Data := No_R_Data;
      Eq_Data   : Equals_Data := No_Equals_Data;
      Sloc_Info : String_Access := null)
      return Relation
   is
     (Unify_Left.Create (L, R, R_Data, Eq_Data, Sloc_Info))
      with Inline;

   function Equals
     (L         : L_Type;
      R         : Right_Var.Logic_Var;
      L_Data    : Left_C_Data := No_L_Data;
      Eq_Data   : Equals_Data := No_Equals_Data;
      Sloc_Info : String_Access := null)
      return Relation
   is
     (Unify_Right.Create (R, L, L_Data, Eq_Data, Sloc_Info))
      with Inline;

   ------------
   -- Member --
   ------------

   function Member
     (R       : Left_Var.Logic_Var;
      Vals    : Unify_Left.R_Type_Array;
      R_Data  : Right_C_Data := No_R_Data;
      Eq_Data : Equals_Data := No_Equals_Data) return Relation
   is
      (Unify_Left.Member (R, Vals, R_Data, Eq_Data));

end Langkit_Support.Adalog.Unify;
