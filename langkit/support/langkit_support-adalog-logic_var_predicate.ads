with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Cheap_Sets;

--  This package defines a support interface for logic variables. Logic
--  variables can have predicates associated to them. This is the interface
--  that such predicates must implement. It is quite close to the relation
--  interface generally, but a distinction is made because the inner workings
--  should not be the same.
--
--  In particular, it is expected that Var_Predicates be idempotent, eg. the
--  Apply operation should always return the same result for the same value
--  of the logic variable it is linked to.
--
--  It is also expected that predicates keep a reference to the logic var
--  they're linked to at construction time, which is why operations on
--  predicates do not take a logic var formal.

package Langkit_Support.Adalog.Logic_Var_Predicate is

   type Var_Predicate_Type is abstract tagged null record;

   function Apply
     (Self : in out Var_Predicate_Type) return Solving_State is abstract;
   --  Apply the predicate, and return whether it succeeded or not

   type Var_Predicate is access all Var_Predicate_Type'Class;
   --  Access type that is meant to be used by clients. Predicates are meant to
   --  be manipulated through accesses.

   package Pred_Sets is new Langkit_Support.Cheap_Sets (Var_Predicate, null);
   --  Logic variables will want to manipulate sets of predicates associated to
   --  them.

end Langkit_Support.Adalog.Logic_Var_Predicate;
