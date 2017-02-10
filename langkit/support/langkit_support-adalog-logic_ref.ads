with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Refcount;

with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Logic_Var_Predicate;
use Langkit_Support.Adalog.Logic_Var_Predicate;

--  This package contains the implementation of logic variables. This is done
--  by implementing base simple types, and instantiating the Adalog.Logic_Var
--  formal package.
--
--  There are two different implementations that vary only in terms of memory
--  management:
--
--  - One is a refcounted, controlled-object based logic variable, so its
--  memory management is automatic.
--
--  - The other is a naked access, so memory management is left to the user.
--

generic
   type Element_Type is private;
   with function Element_Image (E : Element_Type) return String;
package Langkit_Support.Adalog.Logic_Ref is

   ------------------------------
   -- Base logic variable type --
   ------------------------------

   --  This type implements the common logic for a logic variable, and the
   --  common needed operations. See Adalog.Logic_Var for the documentation
   --  of those operations.

   --  This type, however, has by-value semantics, where we want the end
   --  implementations to have by-reference semantics.

   type Var is record
      Reset             : Boolean := True;
      --  Whether this variable is set or not. Reset is True when the variable
      --  has no value.

      Value             : Element_Type;
      --  The value of this logic variable, when it is set

      Pending_Relations : Pred_Sets.Set;
      --  List of relations which applications are pending on this variable
      --  being defined. When the variable will be set, relations will be
      --  evaluated.

      Dbg_Name          : String_Access;
      --  Access to a string representing the name of this variable. Using
      --  this, you can name your variable with human readable names, and
      --  the debugging facilities of Adalog will use it to display it in
      --  equations.
   end record;

   -------------------------------
   -- Base primitive operations --
   -------------------------------

   procedure Reset (Self : in out Var);
   function Is_Defined (Self : Var) return Boolean;
   function Set_Value (Self : in out Var; Data : Element_Type) return Boolean;
   function GetL (Self : Var) return Element_Type;
   procedure Destroy (Self : in out Var);
   function Image (Self : Var) return String is
     (if Self.Dbg_Name /= null then Self.Dbg_Name.all else "None");

   ------------------------------
   -- Var predicates functions --
   ------------------------------

   procedure Add_Predicate (Self : in out Var; Pred : Var_Predicate);
   function Get_Pending_Predicates (Self : Var) return Pred_Sets.Set
   is (Self.Pending_Relations);
   procedure Remove_Predicate (Self : in out Var; Pred : Var_Predicate);

   --------------------------------------
   -- Referenced counted variable type --
   --------------------------------------

   --  This type is a reference counted logic variable type, to use if you
   --  don't care about performance and want automatic deallocation.

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

   -----------------------
   -- Raw variable type --
   -----------------------

   --  This type is a reference to a logic variable implemented with a simple
   --  unsafe access. To use if you want maximum performance and are ready
   --  to manage your memory manually.

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

   ------------------------------------
   -- Formal packages instantiations --
   ------------------------------------

   --  Refcounted one

   package Refcounted_Logic_Var is new Adalog.Logic_Var
     (Ref, Element_Type);

   --  Raw one

   package Raw_Logic_Var is new Adalog.Logic_Var (Raw_Var, Element_Type);

end Langkit_Support.Adalog.Logic_Ref;
