--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Provide common support material for Adalog unit tests

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Solver;
with Langkit_Support.Adalog.Solver_Interface;

generic
   type T is private;
   with function Image (I : T) return String is <>;
package Langkit_Support.Adalog.Generic_Main_Support is

   package Refs is new Langkit_Support.Adalog.Logic_Var
     (T, Value_Image => Image);
   function Create (Name : String) return Refs.Logic_Var;

   package Solver_Ifc is new Solver_Interface (Refs);

   package T_Solver is new Langkit_Support.Adalog.Solver (Solver_Ifc);

   use Solver_Ifc, T_Solver, Refs;

   function "+" (R : Relation) return Relation;
   --  Register R and return it. This is used to keep track of allocated
   --  relations in testcases, to be released in Finalize.

   function "-" (S : String) return String_Access;
   --  Return a dynamically allocated string for S, keeping track of it to be
   --  released in Finalize.

   function R_All
     (Rels : Relation_Array; Dbg_String : String := "") return Relation
   is (+Create_All (Rels, -Dbg_String));

   function R_Any
     (Rels : Relation_Array; Dbg_String : String := "") return Relation
   is (+Create_Any (Rels, -Dbg_String));

   function "or" (L, R : Relation) return Relation is (+Create_Any ((L, R)));
   function "and" (L, R : Relation) return Relation is (+Create_All ((L, R)));

   function Domain (Var        : Refs.Logic_Var;
                    Rels       : Value_Array;
                    Dbg_String : String := "") return Relation
   is (+Create_Domain (Var, Rels, -Dbg_String));

   function "=" (Var  : Refs.Logic_Var; Val : T) return Relation
   is (+Create_Assign (Var, Val));

   function "=" (L, R : Refs.Logic_Var) return Relation
   is (+Create_Unify (L, R));

   function Propagate
     (L, R       : Refs.Logic_Var;
      Conv       : Converter_Type'Class := No_Converter;
      Dbg_String : String := "") return Relation
   is
     (+Create_Propagate (L, R, Conv, -Dbg_String));

   function N_Propagate
     (To   : Refs.Logic_Var;
      Comb : Combiner_Type'Class;
      Vars : Logic_Var_Array;
      Dbg_String : String := "") return Relation
   is (+Create_N_Propagate (To, Comb, Vars, -Dbg_String));

   function Unify
     (L, R : Refs.Logic_Var; Dbg_String : String := "") return Relation
   is (+Create_Unify (L, R, -Dbg_String));

   function Assign
     (L          : Refs.Logic_Var;
      R          : T;
      Conv       : Converter_Type'Class := No_Converter;
      Dbg_String : String := "") return Relation
   is
     (+Create_Assign (L, R, Conv, -Dbg_String));

   function Predicate
     (L          : Refs.Logic_Var;
      P          : Predicate_Type'Class;
      Dbg_String : String := "") return Relation
   is
     (+Create_Predicate (L, P, -Dbg_String));

   function N_Predicate
     (Vars       : Logic_Var_Array;
      P          : N_Predicate_Type'Class;
      Dbg_String : String := "") return Relation
   is
     (+Create_N_Predicate (Vars, P, -Dbg_String));

   function Logic_False return Relation is (+Create_False);
   function Logic_True return Relation is (+Create_True);

   procedure Solve_All (Rel : Relation; Timeout : Natural := 0);

   procedure Run_Main (Main : access procedure);
   procedure Setup_Traces;
   procedure Finalize;

private

   package Relation_Vectors is new Ada.Containers.Vectors
     (Positive, Relation);

   package Variable_Vectors is new Ada.Containers.Vectors
     (Positive, Refs.Logic_Var, Refs."=");

   package String_Access_Vectors is new Ada.Containers.Vectors
     (Positive, String_Access);

   Relations : Relation_Vectors.Vector;
   Variables : Variable_Vectors.Vector;
   Strings   : String_Access_Vectors.Vector;

end Langkit_Support.Adalog.Generic_Main_Support;
