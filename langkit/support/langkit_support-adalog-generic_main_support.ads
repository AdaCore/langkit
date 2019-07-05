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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type T is private;
   with function Image (I : T) return String is <>;
package Langkit_Support.Adalog.Generic_Main_Support is

   package Refs is new Langkit_Support.Adalog.Logic_Ref
     (T, Element_Image => Image);
   function Create (Name : String) return Refs.Raw_Var;

   package T_Solver is new Langkit_Support.Adalog.Solver
     (Refs.Raw_Logic_Var);

   use T_Solver;

   function "+" (R : Relation) return Relation;
   --  Register R and return it. This is used to keep track of allocated
   --  relations in testcases.

   function "-" (S : String) return String_Access is (new String'(S));

   function R_All
     (Rels : Relation_Array; Dbg_String : String := "") return Relation
   is (+Create_All (Rels, -Dbg_String));

   function R_Any
     (Rels : Relation_Array; Dbg_String : String := "") return Relation
   is (+Create_Any (Rels, -Dbg_String));

   function "or" (L, R : Relation) return Relation is (+Create_Any ((L, R)));
   function "and" (L, R : Relation) return Relation is (+Create_All ((L, R)));

   function Domain (Var        : Refs.Raw_Var;
                    Rels       : Value_Array;
                    Dbg_String : String := "") return Relation
   is (+Create_Domain (Var, Rels, -Dbg_String));

   function "=" (Var  : Refs.Raw_Var; Val : T) return Relation
   is (+Create_Assign (Var, Val));

   function "=" (L, R : Refs.Raw_Var) return Relation
   is (+Create_Unify (L, R));

   function Propagate
     (L, R : Refs.Raw_Var;
      Conv : Converter_Type'Class := No_Converter;
      Eq         : Comparer_Type'Class := No_Comparer;
      Dbg_String : String := "") return Relation
   is
     (+Create_Propagate (L, R, Conv, Eq, -Dbg_String));

   function Unify
     (L, R : Refs.Raw_Var; Dbg_String : String := "") return Relation
   is (+Create_Unify (L, R, -Dbg_String));

   function Assign
     (L    : Refs.Raw_Var;
      R    : T;
      Conv : Converter_Type'Class := No_Converter;
      Eq         : Comparer_Type'Class := No_Comparer;
      Dbg_String : String := "") return Relation
   is
     (+Create_Assign (L, R, Conv, Eq, -Dbg_String));

   function Predicate
     (L          : Refs.Raw_Var;
      P          : Predicate_Type'Class;
      Dbg_String : String := "") return Relation
   is
     (+Create_Predicate (L, P, -Dbg_String));

   function N_Predicate
     (Vars       : Variable_Array;
      P          : N_Predicate_Type'Class;
      Dbg_String : String := "") return Relation
   is
     (+Create_N_Predicate (Vars, P, -Dbg_String));

   procedure Solve_All (Rel : Relation; Show_Relation : Boolean := False);

private

   package Relation_Vectors is new Ada.Containers.Vectors
     (Positive, Relation);

   package Variable_Vectors is new Ada.Containers.Vectors
     (Positive, Refs.Raw_Var, Refs."=");

   Relations : Relation_Vectors.Vector;
   Variables : Variable_Vectors.Vector;

end Langkit_Support.Adalog.Generic_Main_Support;
