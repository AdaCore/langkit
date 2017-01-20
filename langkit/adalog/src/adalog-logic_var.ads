pragma Warnings (Off);

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Logic_Var_Predicate; use Adalog.Logic_Var_Predicate;

generic
   type Logic_Var_Type is private;
   type Element_Type is private;

   with procedure Reset (Self : in out Logic_Var_Type) is <>;
   with function Is_Defined (Self : Logic_Var_Type) return Boolean is <>;

   with function SetL
     (Self : in out Logic_Var_Type; Data : Element_Type) return Boolean
     is <> with Inline => True;

   with function GetL (Self : Logic_Var_Type) return Element_Type
     is <> with Inline => True;

   with function Create return Logic_Var_Type is <>;

   with function Get_Pending_Predicates
     (Self : Logic_Var_Type) return Pred_Sets.Set is <>;
   --  Get the predicates associated to this logic variables

   with procedure Add_Predicate (Self : Logic_Var_Type; Pred : Var_Predicate)
     is <>;
   --  Add a new predicate to the predicates associated to this logic variable

   with procedure Remove_Predicate
     (Self : Logic_Var_Type; Pred : Var_Predicate) is <>;
   --  Remove the predicate Pred from the set of predicates associated to the
   --  logic variable.

   with function Image (Self : Logic_Var_Type) return String is <>;

   with function Element_Image (Self : Element_Type) return String is <>;

package Adalog.Logic_Var is
   subtype Var is Logic_Var_Type;

   type Var_Array is array (Natural range <>) of Var;
   type Val_Array is array (Natural range <>) of Element_Type;
   --  Array types for array of variables and array of values of this variable,
   --  for convenience. To be used in other generic packages taking a formal
   --  Logic_Var package as argument.

end Adalog.Logic_Var;
