--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package defines some decision routines that can be used when
--  instantiating a DPLL solver. If none is provided, ``First_Unassigned`` will
--  be used by default.
--  A decision routine should return the unset variable that the DPLL solver
--  should decide upon next. To make this choice, it is given the current
--  `Model`, which it can use to find variables that are unset, and a variable
--  that serves as a lower bound, meaning the routine does not need to inspect
--  the model before this variable as we know those are all already set.
--  The routine can optionnally update this variable with a new, higher value
--  if it discovers that all variables up to this new value are set, so as to
--  optimize subsequent invocations.

package Liblktlang_AdaSAT.Decisions is
   function First_Unassigned
     (M : Model; First_Unset : in out Variable) return Variable_Or_Null;
   --  Return the first unassigned variable in the given model. Start looking
   --  after ``First_Unset``.
end Liblktlang_AdaSAT.Decisions;
