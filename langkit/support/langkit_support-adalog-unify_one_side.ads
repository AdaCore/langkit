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
     (Left    : Var.Var;
      Right   : R_Type;
      R_Data  : R_Convert_Data;
      Eq_Data : Equals_Data) return Relation;

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
      R_Data : R_Convert_Data) return Relation;

   overriding function Solve_Impl (Self : in out Member_T) return Boolean;
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

   function Apply (Self : in out Unify_Rec) return Boolean;
   procedure Revert (Self : in out Unify_Rec);
   procedure Free (Self : in out Unify_Rec);

   function Custom_Image (Self : Unify_Rec) return String
   is ("Unify " & Var.Image (Self.Left) & " {" & R_Image (Self.Right) & "}");

   package Rel is new Relations.Stateful_Relation (Unify_Rec);
   type Unify is new Rel.Rel with null record;

   type Member_T is new Base_Relation with record
      Left           : Var.Var;
      Values         : R_Type_Array_Access;
      Current_Index  : Positive := 1;
      Changed        : Boolean := False;
      Domain_Checked : Boolean := False;
      R_Data         : R_Convert_Data;
   end record;

end Langkit_Support.Adalog.Unify_One_Side;
