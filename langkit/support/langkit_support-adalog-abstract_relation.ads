--  This file contains the base class representing a logic relation. Equations
--  systems are basically a tree of Base_Relation subclasses instances.

package Langkit_Support.Adalog.Abstract_Relation is

   -------------------
   -- Base_Relation --
   -------------------

   type Solving_State is (Try_Again, Satisfied, Unsatisfied);

   function "+" (B : Boolean) return Solving_State is
     (if B then Satisfied else Unsatisfied);

   type Base_Relation is abstract tagged record
      Ref_Count : Natural := 1;
   end record;

   type Relation is access all Base_Relation'Class;
   --  Since relations are trees, they're not manipulated by value, but instead
   --  via this class-wide access type.

   type Relation_Array is array (Positive range <>) of Relation;
   --  Some relations will need to keep/provide arrays of sub-relations

   Empty_Array : constant Relation_Array (1 .. 0) := (others => <>);

   --  Base type for a type implementing the relation interface. A relation has
   --  the following properties:
   --
   --  - It keeps references to zero or more sub-relations, relations that
   --    it will expand. For example, an AND relation will have two (or more)
   --    sub-relations.
   --
   --  - It keeps reference to zero or more logical variables, that it will
   --    bound when solved.
   --
   --  - It can be solved, and that implies eventually solving sub relations,
   --    and eventually binding logic variables.
   --
   --  - It keeps the current state of solving, which is necessary since
   --    relation systems possibly have multiple solutions. This state can
   --    be reset via the Reset primitive.

   function Solve (Self : in out Base_Relation'Class) return Solving_State;
   --  Solve the relation system. If the resolution process could not solve the
   --  equation because some input data is missing, return Try_Again. If it
   --  could determine that the equation has no correct solution, return
   --  Unsatisfied.  Otherwise, initialize all logic variables with solution
   --  values and return Satisfied.

   function Solve_Impl
     (Self : in out Base_Relation) return Solving_State is abstract;
   --  Solve function that must be implemented by relations

   procedure Reset (Self : in out Base_Relation) is abstract;
   --  Reset the state of the relation and all sub-relations

   procedure Cleanup (Self : in out Base_Relation) is abstract;
   --  Perform necessary cleanup before free, like releasing references and
   --  freeing owned resources. This needs to be implemented by Base_Relation
   --  implementers, and it will be called just before freeing Self in Dec_Ref.

   function Children (Self : Base_Relation) return Relation_Array
   is (Empty_Array);

   function Custom_Image (Self : Base_Relation) return String is ("");
   --  Implementers of relations can overload this function if they want the
   --  default image provided by the Print_Relation function to be overloaded.

   function Solve (Self : Relation) return Boolean;
   --  Function to solve the toplevel relation, used by Langkit

   procedure Print_Relation
     (Self : Relation; Current_Relation : Relation := null);

   procedure Inc_Ref (Self : Relation);
   procedure Dec_Ref (Self : in out Relation);
   --  Reference counting primitives to be used by Langkit. A Dec_Ref call
   --  bringing the reference count to 0 will Destroy the referenced relation
   --  object and put the pointer to null, hence the in out mode.

end Langkit_Support.Adalog.Abstract_Relation;
