--  Internal implementation package, not to be used directly by users a-priori.
--  TODO??? document the inner workings a bit more.

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

package Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   generic
      type Ty is private;
      with function Apply (Inst : in out Ty) return Boolean is <>;
      with procedure Free (Inst : in out Ty) is <>;
      with function Custom_Image (Inst : Ty) return String is <>;
   package Pure_Relation is
      --  This generic package represents a relation that will always
      --  yield the same result, and does not produce any side effects.

      type Rel is new I_Relation with record
         Done : Boolean := False;
         Rel  : Ty;
      end record;

      overriding function Solve_Impl (Inst : in out Rel) return Boolean;
      overriding procedure Reset (Inst : in out Rel);
      overriding procedure Cleanup (Inst : in out Rel);
      overriding function Custom_Image (Inst : Rel) return String;
   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   generic
      type Ty is private;
      with function Apply (Inst : in out Ty) return Boolean is <>;
      with procedure Revert (Inst : in out Ty) is <>;
      with procedure Free (Inst : in out Ty) is <>;
      with function Custom_Image (Inst : Ty) return String is <>;
   package Stateful_Relation is

      --  This package represents a relation that has state,
      --  and that needs to be reset to be reused.

      type State_Type is (Start, Success, Finish);

      type Rel is new I_Relation with record
         State : State_Type := Start;
         Rel   : Ty;
      end record;

      overriding function Solve_Impl (Inst : in out Rel) return Boolean;
      overriding procedure Reset (Inst : in out Rel);
      overriding procedure Cleanup (Inst : in out Rel);

      overriding function Custom_Image (Inst : Rel) return String
      is (Custom_Image (Inst.Rel));

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
