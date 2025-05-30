--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Liblktlang_AdaSAT.Formulas; use Liblktlang_AdaSAT.Formulas;

generic
   type User_Context is private;
   pragma Warnings (Off, "referenced");
   --  GNAT complains that the `Check` subprogam below is unreferenced if we
   --  don't disable the warnings, although it is actually referenced (but
   --  outside of this unit).
   with function Check
     (Context     : in out User_Context;
      Assignments : Model;
      Explanation : in out Formula) return Boolean;
   --  Callback invoked by the SAT solver once a model has been found.
   --  This function should indicate whether the model also satisfies the
   --  theory using its return value.
   --  If it does not satisfy it, the function should fill the ``Explanation``
   --  so as to contradict the proposed assignment in order to constrain the
   --  SAT problem further, or let it empty to indicate that the problem cannot
   --  be satisfied in the theory.
   pragma Warnings (On, "referenced");
package Liblktlang_AdaSAT.Theory is
end Liblktlang_AdaSAT.Theory;
