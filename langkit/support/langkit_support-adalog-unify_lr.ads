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

   type Equals_Data is private;
   with function Equals (Eq_Data : Equals_Data; L, R : R_Type) return Boolean
   is <>;

   Convert_Image : String := "";
   Equals_Image  : String := "";

   with package Left_Var is new Adalog.Logic_Var
     (Element_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Element_Type => R_Type, others => <>);

   with procedure L_Dec_Ref (L : in out L_Type);
   with procedure R_Dec_Ref (R : in out R_Type);

package Langkit_Support.Adalog.Unify_LR is

   --------------
   -- Unify_LR --
   --------------

   type LR_State is (No_Change, Left_Changed, Right_Changed);
   type Unify_LR is record
      Left    : Left_Var.Var;
      Right   : Right_Var.Var;
      L_Data  : Left_C_Data;
      R_Data  : Right_C_Data;
      Eq_Data : Equals_Data;
      State   : LR_State := No_Change;
   end record;

   function Apply (Self : in out Unify_LR) return Solving_State;
   procedure Revert (Self : in out Unify_LR);
   procedure Free (Self : in out Unify_LR) is null;

   function Create
     (Left    : Left_Var.Var;
      Right   : Right_Var.Var;
      L_Data  : Left_C_Data;
      R_Data  : Right_C_Data;
      Eq_Data : Equals_Data) return Unify_LR
   is ((Left    => Left,
        Right   => Right,
        L_Data  => L_Data,
        R_Data  => R_Data,
        Eq_Data => Eq_Data,
        State   => No_Change));

   function Custom_Image (Self : Unify_LR) return String;

   package Unify_LR_Rel is new Relations.Stateful_Relation (Unify_LR);

   function Create
     (Left    : Left_Var.Var;
      Right   : Right_Var.Var;
      L_Data  : Left_C_Data;
      R_Data  : Right_C_Data;
      Eq_Data : Equals_Data) return Relation
   is
     (new Unify_LR_Rel.Rel'
        (Rel    => Create (Left, Right, L_Data, R_Data, Eq_Data),
         others => <>));

end Langkit_Support.Adalog.Unify_LR;
