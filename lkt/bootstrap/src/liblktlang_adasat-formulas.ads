--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Liblktlang_AdaSAT.Vectors;

--  Defines the structure used to represent a formula in CNF (Conjunctive
--  Normal Form). The content cannot directly lie in the root `Liblktlang_AdaSAT` package
--  because it would create a circular dependency on the `Liblktlang_AdaSAT.Vectors`
--  package.

package Liblktlang_AdaSAT.Formulas is
   type Clause_Array is array (Positive range <>) of Clause;

   package Clause_Vectors is new Liblktlang_AdaSAT.Vectors
     (Clause, Clause_Array);

   subtype Formula is Clause_Vectors.Vector;
   --  A boolean formula in conjunctive normal form (CNF), represented by
   --  a vector of clauses. This is the representation that the solver will
   --  accept as input.
   --
   --  For example, the formula `(1 | ¬2) & (2 | 3)` is represented by the
   --  vector `[(+1, -2), (2, 3)]`.
   --
   --  It is recommended to use the `Liblktlang_AdaSAT.Builders` package to build
   --  formulas.
   --
   --  TODO: Ideally this type would be private and users should only create
   --  formulas using routines in the `Liblktlang_AdaSAT.Builders` package, however I
   --  could not come up with a satisfying way to do this kind of encapsulation
   --  in Ada yet.

   function Image (F : Formula) return String;
   --  Returns a string representation of the formula

   type SAT_Result is (SAT, UNSAT, UNKNOWN);
   --  The result of solving a formula

   function Satisfies (F : Formula; M : Model) return SAT_Result;
   --  Given a formula and a model, evaluates whether the model
   --  satisfies the formula or not.

   procedure Free_All (F : in out Formula);
   --  Free all the clauses inside this formula and destroy the vector itself
end Liblktlang_AdaSAT.Formulas;
