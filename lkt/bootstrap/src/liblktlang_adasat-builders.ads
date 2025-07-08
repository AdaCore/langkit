--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Liblktlang_AdaSAT.Formulas; use Liblktlang_AdaSAT.Formulas;
private with Liblktlang_AdaSAT.Internals;

--  This package provides some useful data types to build formulas ready to be
--  fed to the solver. Example usage:
--
--  .. code:: ada
--
--     declare
--        My_Clause : Clause_Builder;
--        My_Formula : Formula_Builder;
--     begin
--        My_Clause.Add (+1);
--        My_Clause.Add (-2);
--        My_Clause.Add (+3);
--        My_Formula.Add (My_Clause.Build);
--        --  `My_Formula` now contains `(1 | ¬2 | 3)`
--
--        --  We can use My_Clause to build a second clause since its
--        --  content was cleared when built and added to the formula builder.
--        My_Clause.Add_Simplify (-1);
--        My_Clause.Add_Simplify (-1);  --  No-op since `-1` is already there
--        My_Clause.Add_Simplify (+2);
--        My_Formula.Add (My_Clause.Build);
--        --  `My_Formula` now contains `(1 | ¬2 | 3) & (¬1 | 2)`
--
--        My_Clause.Add (-1);
--        My_Clause.Add (+2);
--        My_Clause.Add (+3);
--        My_Formula.Add_Simplify (My_Clause.Build);
--        --  `My_Formula` still contains `(1 | ¬2 | 3) & (¬1 | 2)`, because
--        --  the clause we want to add `(¬1 | 2 | 3)` is already implied by
--        --  the clause `(¬1 | 2)`.
--
--        My_Formula.Add_At_Most_One (1, 3);
--        --  `My_Formula` now additionally contains a canonical representation
--        --  of `(¬1 | ¬2) & (¬1 | ¬3) & (¬2 | |3)`.
--     end;
--

package Liblktlang_AdaSAT.Builders is
   type Clause_Builder is tagged private;
   --  Type that provide some useful operations to construct a Clause object.
   --  Note that copying the clause builder must be done using the `Copy`
   --  primitive defined below.

   Empty_Clause_Builder : constant Clause_Builder;
   --  An initially empty clause builder

   procedure Reserve (C : in out Clause_Builder; Size : Natural);
   --  Reserve enough space in the internal vector to hold `Size` literals

   procedure Add (C : in out Clause_Builder; L : Literal);
   --  Add the given literal to the clause, without checking if it is already
   --  present.

   procedure Add_Simplify (C : in out Clause_Builder; L : Literal);
   --  Add the given literal to the clause, unless it is already present

   function Copy (C : Clause_Builder) return Clause_Builder;
   --  Copy this clause builder and its internal clause in a new independent
   --  builder.

   procedure Destroy (C : in out Clause_Builder);
   --  Free the memory associated to this builder. Note that you don't need
   --  to call it if you already build the clause.

   function Build (C : in out Clause_Builder) return Clause;
   --  Build the final clause. You do not need to explicitly destroy the
   --  clause builder afterwards.

   type Formula_Builder is tagged private;
   --  Type that provide some useful operations to construct a Formula object

   Empty_Formula_Builder : constant Formula_Builder;
   --  An initially empty formula builder

   procedure Add (F : in out Formula_Builder; C : Clause);
   --  Add the given clause to the formula builder without checking if it can
   --  simplify or be simplified by the clauses already present in the formula.
   --  Note that the given clause is now owned by the builder, along with its
   --  associated memory.

   procedure Add_Simplify (F : in out Formula_Builder; C : Clause);
   --  Add the given clause to the formula builder but also check if it can
   --  simplify or be simplified by the clauses already present in the formula.
   --  Note that the given clause is now owned by the builder, along with its
   --  associated memory. In particular, it could be freed immediatly if is
   --  deemed redundant.

   procedure Add_At_Most_One
     (F        : in out Formula_Builder;
      From, To : Variable);
   --  Add an at-most-one constraint between all variables for ``From`` to
   --  ``To`` to the formula being built.
   --  For example if ``From = 1`` and ``To = 3``, this is equivalent to
   --  adding the clauses ``(¬1 | ¬2) & (¬1 | ¬3) & (¬2 | ¬3)``
   --  (represented using the naive, pairwise-encoding of at-most-one
   --  constraints), but in reality it generates an optimized representation
   --  of this clause which benefits from built-in support inside the DPLL
   --  solvers to allow for efficient resolution.

   function Is_Feasible (F : Formula_Builder; L : Literal) return Boolean;
   --  Run a simple analysis to determine the value of the given literal.
   --  In particular, this will return False if we can know for sure that
   --  the literal cannot be true by looking at the clauses already present
   --  in the formula, and will consevatively return True for all other cases.

   function Copy (F : Formula_Builder) return Formula_Builder;
   --  Copy this formula builder and its internal clauses in a new independent
   --  builder.

   procedure Destroy (F : in out Formula_Builder);
   --  Free the memory associated to this builder. This will also free
   --  all the clauses that it was managing.

   function Build (F : in out Formula_Builder) return Formula;
   --  Build the final formula. You do not need to explicitly destroy the
   --  clause builder afterwards. Note that the clauses are not managed by
   --  the builder anymore, so they should be freed if you don't plan on
   --  feeding them back to the solver.

private
   use Liblktlang_AdaSAT.Internals;

   type Clause_Builder is tagged record
      V : Literal_Vectors.Vector;
   end record;

   type Formula_Builder is tagged record
      V : Clause_Vectors.Vector;
   end record;

   Empty_Clause_Builder  : constant Clause_Builder :=
     (V => Literal_Vectors.Empty_Vector);

   Empty_Formula_Builder : constant Formula_Builder :=
     (V => Clause_Vectors.Empty_Vector);
end Liblktlang_AdaSAT.Builders;
