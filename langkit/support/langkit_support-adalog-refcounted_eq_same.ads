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
     (C_Data : Dummy_Convert_Data; From : LR_Type) return LR_Type
   is (From) with Inline;

   function Equals (L, R : LR_Type) return Boolean is (L = R);

   package Refcounted_Impl is new Unify
     (LR_Type, LR_Type,
      Dummy_Convert_Data, Dummy_Convert_Data, No_Data, No_Data,
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
   package Refcounted_Custom_Bind is
      package Impl is new Unify_LR
        (LR_Type, LR_Type,
         Converter, Converter,
         Convert, Convert,
         Refs.Refcounted_Logic_Var, Refs.Refcounted_Logic_Var,
         L_Dec_Ref => Dec_Ref,
         R_Dec_Ref => Dec_Ref);
   end Refcounted_Custom_Bind;
end Langkit_Support.Adalog.Refcounted_Eq_Same;
