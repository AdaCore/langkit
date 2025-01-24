--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  The root package of the Liblktlang_AdaSAT library. Defines the base data structures
--  that are used everywhere else.
--  You should instantiate the `Liblktlang_AdaSAT.DPLL` package with your own theory or
--  use the `Liblktlang_AdaSAT.Helpers.DPLL_Solve` function to start solving!

package Liblktlang_AdaSAT is
   type Variable is new Positive;
   --  A boolean variable, which can appear positively or negatively in a
   --  literal. For example, ``-2`` represents ``¬2``. From literals, we can
   --  build clauses, and then formulas. Ultimately, the solver's job is to
   --  assign the value ``True`` or ``False`` to each variable so that the
   --  given formula is satisfied. The set of assignments is called a model.
   --
   --  Note that we represent variables as positive integers so that we can
   --  directly use them as keys in arrays (for example in the ``Model`` type),
   --  rather than, say strings (which would require replacing arrays by
   --  hash maps) or records (which would require extra indirections).

   subtype Variable_Or_Null is Variable'Base range 0 .. Variable'Last;

   type Literal is private;
   --  A positive or negative occurrence of a variable

   function "+" (V : Variable) return Literal
      with Inline;
   --  Create a literal with a positive polarity occurrence of a variable

   function "-" (V : Variable) return Literal
      with Inline;
   --  Create a literal with a negative polarity occurrence of a variable

   function Get_Var (L : Literal) return Variable
      with Inline;
   --  Get the variable that occurs in this literal

   type Literal_Array is array (Positive range <>) of Literal;
   type Literal_Array_Access is access Literal_Array;
   --  Note that we don't use a generalized access type to avoid users from
   --  giving stack-allocated clauses to the solver.

   subtype Clause is Literal_Array_Access;
   --  A clause represents a disjunction of literals

   function Image (C : Clause) return String;
   --  Returns a string representation of the clause

   type Variable_Value is (True, False, Unset);
   --  A variable can either be set to True or False, or Unset

   type Model is array (Variable range <>) of Variable_Value;
   --  A mapping from variable to its value

   function Image (M : Model) return String;
   --  Returns a string representation of the model

   procedure Free (C : in out Clause);
   --  Free memory allocated for the given clause

private

   type Literal is new Integer;
   --  Packed representation of literals: instead of having a literal be a
   --  record with a variable and a field that indicates whether it is a
   --  positive or negative occurrence of it, we use the positive value to
   --  represent a positive occurrence of a variable, and a negative value
   --  for a negative occurrence.
   --
   --  Note that while both literals and variables are "just" integers, we
   --  cannot mix them up in our code thanks to Ada's strong type system.
end Liblktlang_AdaSAT;
