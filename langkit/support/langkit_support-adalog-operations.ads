with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

--  This package implement high level logical relation operators on other
--  relations, namely `logical and` and `logical or`.

package Langkit_Support.Adalog.Operations is

   type Working_Queue_Type is array (Positive range <>) of Positive;

   type Base_Aggregate_Rel (Count : Positive) is abstract new Base_Relation
     with record
      Next : Natural := 1;
      --  Index in Working_Queue of the next sub-relation to evaluate

      Sub_Rels : Relation_Array (1 .. Count);
      --  List of sub-relations that make up this ALL/ANY operation

      Working_Queue : Working_Queue_Type (1 .. Count);
      --  Queue of indexes for sub-relations to evaluate.
      --
      --  This queue of indexes is maintained so that at any time, the index of
      --  Satisfied/Unsatisfied relations come first while the index of
      --  Progress/No_Progress ones come last.
      --
      --  When a sub-relation evaluates to Satisfied/Unsatisfied, its index is
      --  swapped with the index of the first sub-relation that is still to be
      --  (re)evaluated and Next is bumped.
      --
      --  If, at the end of an evaluation iteration, all sub-relation returned
      --  No_Progress, it means that this aggregate relation is not standalone:
      --  the parent must continue evaluating other relations before attempting
      --  to re-evaluate it.
   end record;

   overriding procedure Reset (Self : in out Base_Aggregate_Rel);
   overriding procedure Cleanup (Self : in out Base_Aggregate_Rel);
   overriding function Children
     (Self : Base_Aggregate_Rel) return Relation_Array
   is (Self.Sub_Rels);

   -------------
   -- Any_Rel --
   -------------

   type Any_Rel is new Base_Aggregate_Rel with null record;

   overriding function Solve_Impl (Self : in out Any_Rel) return Solving_State;
   overriding function Custom_Image (Self : Any_Rel) return String;

   ---------
   -- All --
   ---------

   type All_Rel is new Base_Aggregate_Rel with null record;

   overriding function Solve_Impl (Self : in out All_Rel) return Solving_State;
   overriding function Custom_Image (Self : All_Rel) return String;

   ------------------
   -- Constructors --
   ------------------

   --  These constructors just borrow their parameters just for the call.
   --  "Logic_X (L, R)" will return a relation that has one new ownership share
   --  for both L and R. As for all constructors, the created object has only
   --  one ownership share which is given to the caller.

   function Logic_Or (L, R : Relation) return Relation;
   function Logic_And (L, R : Relation) return Relation;

   function "or" (L, R : Relation) return Relation renames Logic_Or;
   function "and" (L, R : Relation) return Relation renames Logic_And;

   function Logic_Any (Rels : Relation_Array) return Relation;
   function Logic_All (Rels : Relation_Array) return Relation;

end Langkit_Support.Adalog.Operations;
