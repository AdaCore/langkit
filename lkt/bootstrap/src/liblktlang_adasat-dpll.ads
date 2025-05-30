--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Liblktlang_AdaSAT.Decisions;
with Liblktlang_AdaSAT.Formulas; use Liblktlang_AdaSAT.Formulas;
with Liblktlang_AdaSAT.Theory;

--  This is the main package of this library. Users should instantiate their
--  package with their custom theory (or use the SAT-only solver instantiated
--  in package `Liblktlang_AdaSAT.Helpers`) and call `Solve` with their formula.
--  A custom decision procedure can be given, else the default one will be
--  used (see `Liblktlang_AdaSAT.Decisions` for more information).

generic
   with package User_Theory is new Theory (<>);
   --  The theory against which SAT models will be checked
   with function Next_Decision
     (M : Model; First_Unset : in out Variable) return Variable_Or_Null
   is Liblktlang_AdaSAT.Decisions.First_Unassigned;
   --  The heuristic to use in order to choose which variable will be
   --  decided next. For problems that accept several solutions, this can be
   --  used guide the solver towards a specific one. For example, Langkit
   --  requires that we find them in a specific order that corresponds to the
   --  order in which its high-level equations would be traversed by a naive
   --  recursive-descent solver. On the other hand, a "wave function collapse"
   --  implementation requires that we collapse the tile with the lowest
   --  entropy (see the corresponding test).
package Liblktlang_AdaSAT.DPLL is
   function Solve
     (F        : Formula;
      Ctx      : in out User_Theory.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean;
   --  Determine whether the formula is satisfiable or not.
   --  In case it is satisfiable, fill the given model with a solution.
   --  if ``Min_Vars`` is not 0, the solver does not decide variables
   --  that after the given ``Min_Vars`` index.
   --
   --  Note that the given formula becomes owned by the solver, and therefore
   --  its memory and that of its inner clauses will be deallocated when the
   --  solving terminates.
end Liblktlang_AdaSAT.DPLL;
