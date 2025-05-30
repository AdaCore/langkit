--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Liblktlang_AdaSAT.Formulas;

--  Provide various helpers for using the Liblktlang_AdaSAT library

package Liblktlang_AdaSAT.Helpers is
   use Liblktlang_AdaSAT.Formulas;

   function DPLL_Solve
     (F        : Formula;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean;
   --  Solve the given Formula and provide a model for its variables.
   --  It is assumed that this is a pure SAT problem. This simply invokes
   --  the `Solve` function on an instantiation of the `Liblktlang_AdaSAT.DPLL` package
   --  with a theory that always returns True.

end Liblktlang_AdaSAT.Helpers;
