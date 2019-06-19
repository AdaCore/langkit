------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  Provide common support material for unit tests

with Ada.Containers.Vectors;

with Langkit_Support.Adalog.Logic_Ref;
with Langkit_Support.Adalog.Solver;

package Langkit_Support.Adalog.Main_Support is

   function Element_Image (I : Integer) return String;
   package Refs is new Langkit_Support.Adalog.Logic_Ref (Integer);
   function Create (Name : String) return Refs.Raw_Var;

   package Int_Solver is new Langkit_Support.Adalog.Solver
     (Integer, Refs.Raw_Logic_Var);

   use Int_Solver;

   function "+" (R : Relation) return Relation;
   --  Register R and return it. This is used to keep track of allocated
   --  relations in testcases.

   function R_All (Rels : Relation_Array) return Relation
   is (+Create_All (Rels));
   function R_Any (Rels : Relation_Array) return Relation
   is (+Create_Any (Rels));
   function "or" (L, R : Relation) return Relation is (+Create_Any ((L, R)));
   function "and" (L, R : Relation) return Relation is (+Create_All ((L, R)));
   function Domain (Var  : Refs.Raw_Var; Rels : Value_Array) return Relation
   is (+Create_Domain (Var, Rels));
   function "=" (Var  : Refs.Raw_Var; Val : Integer) return Relation
   is (+Create_Assign (Var, Val));
   function "=" (L, R : Refs.Raw_Var) return Relation
   is (+Create_Unify (L, R));
   function Propagate
     (L, R : Refs.Raw_Var;
      Conv : Converter_Type'Class := No_Converter;
      Eq   : Comparer_Type'Class := No_Comparer) return Relation
   is
      (+Create_Propagate (L, R, Conv, Eq));

   function Assign
     (L    : Refs.Raw_Var;
      R    : Integer;
      Conv : Converter_Type'Class := No_Converter;
      Eq   : Comparer_Type'Class := No_Comparer) return Relation
   is
     (+Create_Assign (L, R, Conv, Eq));

   function Predicate
     (L : Refs.Raw_Var; P : Predicate_Type'Class) return Relation
   is
     (+Create_Predicate (L, P));

   procedure Solve_All (Rel : Relation; Show_Relation : Boolean := False);

   type Is_Even_Pred is new Predicate_Type with null record;
   function Call (Self : Is_Even_Pred; Value : Integer) return Boolean
   is (Value mod 2 = 0);
   function Image (Self : Is_Even_Pred) return String is ("Is_Even");
   Is_Even : constant Predicate_Type'Class := Is_Even_Pred'(null record);

private

   package Relation_Vectors is new Ada.Containers.Vectors
     (Positive, Relation);

   package Variable_Vectors is new Ada.Containers.Vectors
     (Positive, Refs.Raw_Var, Refs."=");

   Relations : Relation_Vectors.Vector;
   Variables : Variable_Vectors.Vector;

end Langkit_Support.Adalog.Main_Support;
