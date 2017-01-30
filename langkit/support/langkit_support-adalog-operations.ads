with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

--  This package implement high level logical relation operators on other
--  relations, namely `logical and` and `logical or`.

package Langkit_Support.Adalog.Operations is

   ---------------------------------
   --  Or relation implementation --
   ---------------------------------

   type Or_Rec is new I_Relation with record
      Left, Right : Relation;
      State       : Integer := 0;
   end record;

   overriding function Solve_Impl (Inst : in out Or_Rec) return Boolean;
   overriding procedure Reset (Inst : in out Or_Rec);
   overriding procedure Cleanup (Inst : in out Or_Rec);
   overriding function Children (Inst : Or_Rec) return Relation_Array
   is ((Inst.Left, Inst.Right));

   ----------------------------------
   --  And relation implementation --
   ----------------------------------

   type And_Rec is new I_Relation with record
      Left, Right : Relation;
      State       : Integer := 0;
   end record;

   overriding function Solve_Impl (Inst : in out And_Rec) return Boolean;
   overriding procedure Reset (Inst : in out And_Rec);
   overriding procedure Cleanup (Inst : in out And_Rec);
   overriding function Children (Inst : And_Rec) return Relation_Array
   is ((Inst.Left, Inst.Right));

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

end Langkit_Support.Adalog.Operations;
