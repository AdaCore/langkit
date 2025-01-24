--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Liblktlang_AdaSAT.Vectors;

--  This factors out some data structures and routines that are used by all
--  instantiations of the `Liblktlang_AdaSAT.DPLL` generic package but that are common
--  to each of them and therefore which do not need to be inside the instances.
--  This could theoretically be inside the root `Liblktlang_AdaSAT` package but cannot
--  because of a circular dependency on the `Liblktlang_AdaSAT.Vectors` package.

private package Liblktlang_AdaSAT.Internals is
   package Literal_Vectors is new Liblktlang_AdaSAT.Vectors
     (Literal, Literal_Array);

   function Get_Literal_Vector_Array is new Literal_Vectors.Internal_Array
     (Literal_Array_Access);
end Liblktlang_AdaSAT.Internals;
