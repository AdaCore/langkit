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

   function Equals (Data : Dummy_Equals_Data; L, R : LR_Type) return Boolean
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
         R_Dec_Ref => Dec_Ref);

      function Create
        (L, R    : Refs.Raw_Logic_Var.Var;
         Data    : Converter;
         Eq_Data : Equals_Data) return Relation
      is
        (Relation (Impl.Equals (L, R, Data, Data, Eq_Data)));

      function Create
        (L       : Refs.Raw_Logic_Var.Var;
         R       : LR_Type;
         Data    : Converter;
         Eq_Data : Equals_Data) return Relation
      is
        (Relation (Impl.Equals (L, R, Data, Eq_Data)));

      function Create
        (L       : LR_Type;
         R       : Refs.Raw_Logic_Var.Var;
         Data    : Converter;
         Eq_Data : Equals_Data) return Relation
      is
        (Relation (Impl.Equals (L, R, Data, Eq_Data)));

   end Raw_Custom_Bind;

end Langkit_Support.Adalog.Eq_Same;
