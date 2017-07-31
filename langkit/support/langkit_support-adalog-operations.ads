with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

--  This package implement high level logical relation operators on other
--  relations, namely `logical and` and `logical or`.

package Langkit_Support.Adalog.Operations is

   type Base_Aggregate_Rel (Count : Positive) is abstract new Base_Relation
     with record
      Sub_Rels : Relation_Array (1 .. Count);
      State    : Positive := 1;
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

   function Logic_Or (L, R : Relation) return access Base_Relation'Class;
   function Logic_And (L, R : Relation) return access Base_Relation'Class;

   function "or" (L, R : Relation) return access Base_Relation'Class
      renames Logic_Or;

   function "and" (L, R : Relation) return access Base_Relation'Class
                   renames Logic_And;

   function Logic_Any
     (Rels : Relation_Array) return access Base_Relation'Class;
   function Logic_All
     (Rels : Relation_Array) return access Base_Relation'Class;

end Langkit_Support.Adalog.Operations;
