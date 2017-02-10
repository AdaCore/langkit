with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Relations;

--  Internal implementation package, not to be used directly by users a-priori.
--  TODO??? document the inner workings a bit more.

generic
   type L_Type is private;
   type R_Type is private;

   with function Equals (L, R : L_Type) return Boolean is <>;

   type Right_C_Data is private;

   with function Convert
     (C_Data : Right_C_Data; From : R_Type) return L_Type is <>;

   with package Var is new Logic_Var (Element_Type => L_Type, others => <>);

   with function R_Image (Self : R_Type) return String is <>;
   with function L_Image (Self : L_Type) return String is <>;

   Invert_Equals : Boolean := False;
package Langkit_Support.Adalog.Unify_One_Side is

   type R_Type_Array is array (Positive range <>) of R_Type;

   -----------
   -- Unify --
   -----------

   type Unify is record
      Left    : Var.Var;
      Right   : R_Type;
      Changed : Boolean := False;
      R_Data  : Right_C_Data;
   end record;

   function Create
     (Left : Var.Var; Right : R_Type; R_Data : Right_C_Data) return Unify
   is
     ((Left => Left, Right => Right, Changed => False, R_Data => R_Data));

   function Apply (Self : in out Unify) return Boolean;
   procedure Revert (Self : in out Unify);
   procedure Free (Self : in out Unify) is null;

   function Custom_Image (Self : Unify) return String
   is ("Unify " & Var.Image (Self.Left) & " {" & R_Image (Self.Right) & "}");

   package Rel is new Relations.Stateful_Relation (Unify);

   ------------
   -- Member --
   ------------

   --  TODO??? Why is member not implemented in terms of
   --  Relation.Stateful_Relation?

   type R_Type_Array_Access is access all R_Type_Array;

   type Member_T is new Base_Relation with record
      Left           : Var.Var;
      Values         : R_Type_Array_Access;
      Current_Index  : Positive := 1;
      Changed        : Boolean := False;
      Domain_Checked : Boolean := False;
      R_Data         : Right_C_Data;
   end record;

   overriding function Solve_Impl (Self : in out Member_T) return Boolean;
   overriding procedure Reset (Self : in out Member_T);
   overriding procedure Cleanup (Self : in out Member_T);
   overriding function Custom_Image (Self : Member_T) return String;

   function Member
     (R : Var.Var; Vals : R_Type_Array; R_Data : Right_C_Data) return Relation;

end Langkit_Support.Adalog.Unify_One_Side;
