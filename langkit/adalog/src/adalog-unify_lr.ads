------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package represents the unification logic for the case where both
--  operands are logic variables. See Adalog.Unify for more details.

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Logic_Var;
with Adalog.Relations;

generic
   type L_Type is private;
   type R_Type is private;

   type Left_C_Data is private;
   type Right_C_Data is private;

   with function Convert
     (C_Data : Right_C_Data; From : R_Type) return L_Type is <>;
   with function Convert
     (C_Data : Left_C_Data; From : L_Type) return R_Type is <>;

   with package Left_Var is new Adalog.Logic_Var
     (Element_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Element_Type => R_Type, others => <>);
package Adalog.Unify_LR is

   --------------
   -- Unify_LR --
   --------------

   type LR_State is (No_Change, Left_Changed, Right_Changed);
   type Unify_LR is record
      Left   : Left_Var.Var;
      Right  : Right_Var.Var;
      L_Data : Left_C_Data;
      R_Data : Right_C_Data;
      State  : LR_State := No_Change;
   end record;

   function Apply (Self : in out Unify_LR) return Boolean;
   procedure Revert (Self : in out Unify_LR);
   procedure Free (Self : in out Unify_LR) is null;

   function Create
     (Left   : Left_Var.Var;
      Right  : Right_Var.Var;
      L_Data : Left_C_Data;
      R_Data : Right_C_Data) return Unify_LR
   is ((Left  => Left, Right => Right, State => No_Change,
        L_Data => L_Data, R_Data => R_Data));

   function Custom_Image (Self : Unify_LR) return String
   is
     ("<Unify_LR Left: " & Left_Var.Image (Self.Left)
      & " Right: " & Right_Var.Image (Self.Right) & ">");

   package Unify_LR_Rel is new Relations.Stateful_Relation (Unify_LR);

   function Create
     (Left   : Left_Var.Var;
      Right  : Right_Var.Var;
      L_Data : Left_C_Data;
      R_Data : Right_C_Data) return Relation
   is
     (new Unify_LR_Rel.Rel'(Rel => Create (Left, Right, L_Data, R_Data),
                            others => <>));

end Adalog.Unify_LR;
