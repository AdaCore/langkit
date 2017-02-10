with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Refcount;

with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Logic_Var_Predicate;
use Langkit_Support.Adalog.Logic_Var_Predicate;

generic
   type Element_Type is private;
   with function Element_Image (E : Element_Type) return String;
package Langkit_Support.Adalog.Logic_Ref is

   type Var is record
      Reset             : Boolean := True;
      El                : Element_Type;

      --  List of relations which applications are pending on this variable
      --  being defined. When the variable will be set, relations will be
      --  evaluated.
      Pending_Relations : Pred_Sets.Set;

      Dbg_Name          : String_Access;
   end record;

   procedure Reset (Self : in out Var);
   function Is_Defined (Self : Var) return Boolean;
   function Set_Value (Self : access Var; Data : Element_Type) return Boolean;
   function GetL (Self : Var) return Element_Type;
   procedure Destroy (Self : in out Var);
   function Image (Self : Var) return String is
     (if Self.Dbg_Name /= null then Self.Dbg_Name.all else "None");

   --  Var predicates functions

   procedure Add_Predicate (Self : in out Var; Pred : Var_Predicate);
   function Get_Pending_Predicates (Self : Var) return Pred_Sets.Set
   is (Self.Pending_Relations);
   procedure Remove_Predicate (Self : in out Var; Pred : Var_Predicate);

   type Refcounted_El is new GNATCOLL.Refcount.Refcounted with record
      Content : Var;
   end record;

   package Refs is new GNATCOLL.Refcount.Shared_Pointers (Refcounted_El);
   type Ref is new Refs.Ref with null record;

   procedure Reset (Self : in out Ref);
   function Is_Defined (Self : Ref) return Boolean;
   function SetL (Self : in out Ref; Data : Element_Type) return Boolean;
   function GetL (Self : Ref) return Element_Type;

   function Get_Pending_Predicates (Self : Ref) return Pred_Sets.Set
   is (Get_Pending_Predicates (Self.Unchecked_Get.Content));

   procedure Remove_Predicate (Self : Ref; Pred : Var_Predicate);
   procedure Add_Predicate (Self : Ref; Pred : Var_Predicate);

   function Image (Self : Ref) return String is
     (Image (Self.Unchecked_Get.Content));

   function Create return Ref;

   type Raw_Var is access all Var;
   procedure Reset (Self : in out Raw_Var);
   function Is_Defined (Self : Raw_Var) return Boolean;
   function SetL
     (Self : in out Raw_Var; Data : Element_Type) return Boolean;
   function GetL (Self : Raw_Var) return Element_Type;
   function Create return Raw_Var;

   function Get_Pending_Predicates (Self : Raw_Var) return Pred_Sets.Set
   is (Get_Pending_Predicates (Self.all));

   procedure Remove_Predicate (Self : Raw_Var; Pred : Var_Predicate);
   procedure Add_Predicate (Self : Raw_Var; Pred : Var_Predicate);

   function Image (Self : Raw_Var) return String is
     (Image (Self.all));

   package Refcounted_Logic_Var is new Adalog.Logic_Var
     (Ref, Element_Type);
   package Raw_Logic_Var is new Adalog.Logic_Var (Raw_Var, Element_Type);

end Langkit_Support.Adalog.Logic_Ref;
