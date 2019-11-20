with Langkit_Support.Adalog.Logic_Var;

generic
   with package Logic_Vars
     is new Langkit_Support.Adalog.Logic_Var (<>);
package Langkit_Support.Adalog.Solver_Interface is

   subtype Value_Type is Logic_Vars.Element_Type;

   use Logic_Vars;

   subtype Value_Array is Logic_Vars.Val_Array;
   subtype Variable_Array is Logic_Vars.Var_Array;

   -------------------
   -- Functor types --
   -------------------

   --  The solver contains a number of abstract functor types, that are meant
   --  to be derived by the client to provide functionality.
   --
   --  The reason functor types are exposed is if you need to store state along
   --  with your function. If you don't ,there are convenience constructors
   --  that take access to functions.

   type Base_Functor_Type is abstract tagged null record;
   procedure Destroy (Self : in out Base_Functor_Type) is null;

   --------------------
   -- Predicate_Type --
   --------------------

   type Predicate_Type is abstract new Base_Functor_Type with null record;
   function Call
     (Self : Predicate_Type; Val : Value_Type) return Boolean is abstract;
   function Image (Self : Predicate_Type) return String is ("");
   function Full_Image (Self : Predicate_Type; Dummy_Var : Var) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  value, returning whether the predicate succeeds.

   ----------------------
   -- N_Predicate_Type --
   ----------------------

   type N_Predicate_Type is abstract new Base_Functor_Type with null record;
   function Call
     (Self : N_Predicate_Type; Vals : Logic_Vars.Val_Array) return Boolean
      is abstract;
   function Image (Self : N_Predicate_Type) return String is ("");
   function Full_Image
     (Self : N_Predicate_Type; Dummy_Vars : Var_Array) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  value, returning whether the predicate succeeds.

   -------------------
   -- Comparer_Type --
   -------------------

   type Comparer_Type is abstract new Base_Functor_Type with null record;
   function Compare
     (Self : Comparer_Type; L, R : Value_Type) return Boolean is abstract;
   --  Type to compare two values of Value_Type, returning whether they're
   --  equal or not.
   function Image (Self : Comparer_Type) return String is ("");

   function No_Comparer return Comparer_Type'Class;

   --------------------
   -- Converter_Type --
   --------------------

   type Converter_Type is abstract new Base_Functor_Type with null record;
   function Convert
     (Self : Converter_Type; From : Value_Type) return Value_Type
      is abstract;
   --  Type to convert one value to another
   function Image (Self : Converter_Type) return String is ("");
   function No_Converter return Converter_Type'Class;

   -------------------------------------
   -- Stateless functors constructors --
   -------------------------------------

   --  Those constructors are a shortcut to avoid creating custom functor types
   --  when you have no state to store.

   function Predicate
     (Pred      : access function (V : Value_Type) return Boolean;
      Pred_Name : String := "Predicate")
      return Predicate_Type'Class;
   --  Create a Predicate relation. A Predicate relation will solve
   --  successfully if the ``Predicate`` applied to the value of
   --  ``Logic_Var`` yields ``True``.

   function N_Predicate
     (Pred      : access function (V : Value_Array) return Boolean;
      Pred_Name : String := "N_Predicate")
      return N_Predicate_Type'Class;

   function Comparer
     (Pred      : access function (L, R : Value_Type) return Boolean;
      Pred_Name : String := "Comparer") return Comparer_Type'Class;

   function Converter
     (Pred      : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class;

   --------------------------
   -- Solving options type --
   --------------------------

   type Solve_Options_Type is record
      Cut_Dead_Branches : Boolean := True;
   end record;

   Default_Options : constant Solve_Options_Type := (others => <>);

end Langkit_Support.Adalog.Solver_Interface;
