with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

--  This package implement high level logical relation operators on other
--  relations, namely `logical and` and `logical or`.

package Langkit_Support.Adalog.Operations is

   type Base_Aggregate_Rel (N : Positive) is abstract new I_Relation
     with record
      Sub_Rels    : Relation_Array (1 .. N);
      State       : Positive := 1;
   end record;

   overriding procedure Reset (Self : in out Base_Aggregate_Rel);
   overriding procedure Cleanup (Self : in out Base_Aggregate_Rel);
   overriding function Children
     (Self : Base_Aggregate_Rel) return Relation_Array
   is (Self.Sub_Rels);

   ----------
   --  Any --
   ----------

   type Any is new Base_Aggregate_Rel with null record;

   overriding function Solve_Impl (Self : in out Any) return Boolean;
   overriding function Custom_Image (Self : Any) return String;

   ----------
   --  All --
   ----------

   type All_Rel is new Base_Aggregate_Rel with null record;

   overriding function Solve_Impl (Self : in out All_Rel) return Boolean;
   overriding function Custom_Image (Self : All_Rel) return String;

   ----------------------------------------
   --  Operator overloading constructors --
   ----------------------------------------

   --  These constructors just borrow their parameters just for the call.
   --  "Logic_X (L, R)" will return a relation that has one new ownership share
   --  for both L and R. As for all constructors, the created object has only
   --  one ownership share which is given to the caller.

   function Logic_Or (L, R : Relation) return access I_Relation'Class;
   function Logic_And (L, R : Relation) return access I_Relation'Class;

   function "or" (L, R : Relation) return access I_Relation'Class
      renames Logic_Or;

   function "and" (L, R : Relation) return access I_Relation'Class
                   renames Logic_And;

   function Logic_Any (Rels : Relation_Array) return access I_Relation'Class;
   function Logic_All (Rels : Relation_Array) return access I_Relation'Class;

end Langkit_Support.Adalog.Operations;
