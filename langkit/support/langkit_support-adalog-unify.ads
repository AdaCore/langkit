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

   with function Convert
     (C_Data : Right_C_Data; From : R_Type) return L_Type is <>;
   with function Convert
     (C_Data : Left_C_Data; From : L_Type) return R_Type is <>;

   with function Equals (L, R : L_Type) return Boolean is <>;
   with function Equals (L, R : R_Type) return Boolean is <>;

   with package Left_Var is new Logic_Var
     (Element_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Element_Type => R_Type, others => <>);

package Langkit_Support.Adalog.Unify is

   --  TODO HACK FIXME??? P418-022 Removing the body for this package causes a
   --  generic instantiation error.
   procedure What;

   package Simple_Unify is new Adalog.Unify_LR
     (L_Type, R_Type, Left_C_Data, Right_C_Data,
      Convert, Convert, Left_Var, Right_Var, Equals);
   use Simple_Unify;

   package Unify_Left is new Unify_One_Side
     (L_Type, R_Type, Equals, Right_C_Data,
      Convert, Left_Var, Right_Var.Element_Image, Left_Var.Element_Image);

   package Unify_Right is new Unify_One_Side
     (R_Type, L_Type, Equals, Left_C_Data,
      Convert, Right_Var, Left_Var.Element_Image, Right_Var.Element_Image,
      Invert_Equals => True);

   ------------------
   -- Eq predicate --
   ------------------

   function Equals
     (L      : Left_Var.Var; R : Right_Var.Var;
      L_Data : Left_C_Data := No_L_Data;
      R_Data : Right_C_Data := No_R_Data)
      return access Base_Relation'Class
   is
     (new Unify_LR_Rel.Rel'
        (Rel => Create (L, R, No_L_Data, No_R_Data),
         others => <>))
   with Inline;

   function Equals
     (L : Left_Var.Var; R : R_Type; R_Data : Right_C_Data := No_R_Data)
      return access Base_Relation'Class
   is
     (new Unify_Left.Rel.Rel'
        (Rel => Unify_Left.Create (L, R, R_Data), others => <>))
   with Inline;

   function Equals
     (L : L_Type; R : Right_Var.Var; L_Data : Left_C_Data := No_L_Data)
      return access Base_Relation'Class
   is
     (new Unify_Right.Rel.Rel'
        (Rel => Unify_Right.Create (R, L, L_Data), others => <>))
   with Inline;

   ------------
   -- Member --
   ------------

   function Member
     (R      : Left_Var.Var;
      Vals   : Unify_Left.R_Type_Array;
      R_Data : Right_C_Data := No_R_Data) return Relation
   is
      (Unify_Left.Member (R, Vals, R_Data));

end Langkit_Support.Adalog.Unify;
