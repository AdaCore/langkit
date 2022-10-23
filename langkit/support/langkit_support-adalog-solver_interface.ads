--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Adalog.Logic_Var;

generic
   with package Logic_Vars is new Langkit_Support.Adalog.Logic_Var (<>);
package Langkit_Support.Adalog.Solver_Interface is

   use Logic_Vars;

   -------------------
   -- Functor types --
   -------------------

   --  The solver contains a number of abstract functor types, that are meant
   --  to be derived by the client to provide functionality.
   --
   --  The reason functor types are exposed is if you need to store state along
   --  with your function. If you don't, there are convenience constructors
   --  that take access to functions.

   type Base_Functor_Type is abstract tagged record
      Ref_Count : Natural;
      --  Functors are generally dynamically allocated and handled through
      --  access types. This tracks the number of references to a functor, so
      --  that we know when to deallocate it.
   end record;
   procedure Destroy (Self : in out Base_Functor_Type) is null;

   --------------------
   -- Predicate_Type --
   --------------------

   type Predicate_Type is abstract new Base_Functor_Type with record
      Cache_Set   : Boolean;
      Cache_Key   : Value_Type;
      Cache_Value : Boolean;
   end record;

   function Call
     (Self : Predicate_Type; Val : Value_Type) return Boolean is abstract;
   --  Derived types must override this to implement the predicate

   function Call_Wrapper
     (Self : in out Predicate_Type'Class; Val  : Value_Type) return Boolean;
   --  Converter users must call this instead of ``Convert`` to use the cache

   function Image (Self : Predicate_Type) return String is ("");
   function Full_Image
     (Self : Predicate_Type; Dummy_Var : Logic_Vars.Logic_Var) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  value, returning whether the predicate succeeds.

   ----------------------
   -- N_Predicate_Type --
   ----------------------

   type N_Predicate_Type (N : Positive) is
     abstract new Base_Functor_Type with
   record
      Cache_Set   : Boolean;
      Cache_Key   : Value_Array (1 .. N);
      Cache_Value : Boolean;
   end record;
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  list of values, returning whether the predicate succeeds.

   function Call
     (Self : N_Predicate_Type; Vals : Logic_Vars.Value_Array) return Boolean
   is abstract;
   --  Derived types must override this to implement the predicate

   function Call_Wrapper
     (Self : in out N_Predicate_Type'Class;
      Vals : Logic_Vars.Value_Array) return Boolean;
   --  Converter users must call this instead of ``Convert`` to use the cache

   function Image (Self : N_Predicate_Type) return String is ("");
   function Full_Image
     (Self : N_Predicate_Type; Dummy_Vars : Logic_Var_Array) return String
   is ("");

   --------------------
   -- Converter_Type --
   --------------------

   type Converter_Type is abstract new Base_Functor_Type with record
      Cache_Set              : Boolean;
      Cache_Key, Cache_Value : Value_Type;
   end record;
   --  Type to convert one value to another

   function Convert
     (Self : Converter_Type; From : Value_Type) return Value_Type
      is abstract;
   --  Derived types must override this to implement the conversion

   function Convert_Wrapper
     (Self : in out Converter_Type; From : Value_Type) return Value_Type;
   --  Converter users must call this instead of ``Convert`` to use the cache

   function Image (Self : Converter_Type) return String is ("");

   function No_Converter return Converter_Type'Class;
   --  Return a special converter that just raises a ``Program_Error`` when
   --  called.

   function Is_No_Converter (Self : Converter_Type'Class) return Boolean;
   --  Return whether ``Self`` comes from ``No_Converter``

   -------------------
   -- Combiner_Type --
   -------------------

   type Combiner_Type (N : Positive) is
     abstract new Base_Functor_Type with
   record
      Cache_Set   : Boolean;
      Cache_Key   : Value_Array (1 .. N);
      Cache_Value : Value_Type;
   end record;
   --  Type to compute a value from multiple input values

   function Combine
     (Self : Combiner_Type;
      Vals : Logic_Vars.Value_Array) return Value_Type is abstract;
   --  Derived types must override this to implement the value computation

   function Combine_Wrapper
     (Self : in out Combiner_Type'Class;
      Vals : Logic_Vars.Value_Array) return Value_Type;
   --  Combiner users must call this instead of ``Combine`` to use the cache

   function Image (Self : Combiner_Type) return String is ("");

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
      Arity     : Positive;
      Pred_Name : String := "N_Predicate") return N_Predicate_Type'Class;

   function Converter
     (Pred      : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class;

   function Combiner
     (Comb      : access function (V : Value_Array) return Value_Type;
      Arity     : Positive;
      Comb_Name : String := "Combiner") return Combiner_Type'Class;

end Langkit_Support.Adalog.Solver_Interface;
