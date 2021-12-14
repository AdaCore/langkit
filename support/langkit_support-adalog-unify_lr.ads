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

--  This package represents the unification logic for the case where both
--  operands are logic variables. See Adalog.Unify for more details.

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Relations;

generic
   type L_Type is private;
   type R_Type is private;

   type Left_C_Data is private;
   type Right_C_Data is private;

   with function Convert
     (C_Data : Right_C_Data; From : R_Type) return L_Type is <>;
   with function Convert
     (C_Data : Left_C_Data; From : L_Type) return R_Type is <>;
   with function Image
     (C_Data : Right_C_Data) return String is <>;

   type Equals_Data is private;
   with function Equals (Eq_Data : Equals_Data; L, R : R_Type) return Boolean
     is <>;
   with function Image
     (C_Data : Equals_Data) return String is <>;

   with package Left_Var is new Adalog.Logic_Var
     (Value_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Value_Type => R_Type, others => <>);

   with procedure Left_C_Data_Inc_Ref (Self : Left_C_Data);
   with procedure Right_C_Data_Inc_Ref (Self : Right_C_Data);
   with procedure Equals_Data_Inc_Ref (Self : Equals_Data);

   with procedure L_Dec_Ref (Self : in out L_Type);
   with procedure R_Dec_Ref (Self : in out R_Type);
   with procedure Left_C_Data_Dec_Ref (Self : in out Left_C_Data);
   with procedure Right_C_Data_Dec_Ref (Self : in out Right_C_Data);
   with procedure Equals_Data_Dec_Ref (Self : in out Equals_Data);

   One_Side_Convert : Boolean := False;

package Langkit_Support.Adalog.Unify_LR is

   --------------
   -- Unify_LR --
   --------------

   type LR_State is (No_Change, Left_Changed, Right_Changed);
   type Unify_LR is record
      Left    : Left_Var.Logic_Var;
      Right   : Right_Var.Logic_Var;
      L_Data  : Left_C_Data;
      R_Data  : Right_C_Data;
      Eq_Data : Equals_Data;
      State   : LR_State := No_Change;
   end record;

   function Apply (Self : in out Unify_LR) return Solving_State;
   procedure Revert (Self : in out Unify_LR);
   procedure Free (Self : in out Unify_LR);

   function Create
     (Left    : Left_Var.Logic_Var;
      Right   : Right_Var.Logic_Var;
      L_Data  : Left_C_Data;
      R_Data  : Right_C_Data;
      Eq_Data : Equals_Data) return Unify_LR;

   function Custom_Image (Self : Unify_LR) return String;

   package Unify_LR_Rel is new Relations.Stateful_Relation (Unify_LR);

   function Create
     (Left    : Left_Var.Logic_Var;
      Right   : Right_Var.Logic_Var;
      L_Data  : Left_C_Data;
      R_Data  : Right_C_Data;
      Eq_Data : Equals_Data) return Relation
   is
     (new Unify_LR_Rel.Rel'
        (Rel    => Create (Left, Right, L_Data, R_Data, Eq_Data),
         others => <>));

end Langkit_Support.Adalog.Unify_LR;
