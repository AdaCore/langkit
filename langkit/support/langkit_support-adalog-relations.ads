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
      with function Apply (Self : in out Ty) return Boolean is <>;
      with procedure Free (Self : in out Ty) is <>;
      with function Custom_Image (Self : Ty) return String is <>;
   package Pure_Relation is
      --  This generic package represents a relation that will always
      --  yield the same result, and does not produce any side effects.

      type Rel is new I_Relation with record
         Done : Boolean := False;
         Rel  : Ty;
      end record;

      overriding function Solve_Impl (Self : in out Rel) return Boolean;
      overriding procedure Reset (Self : in out Rel);
      overriding procedure Cleanup (Self : in out Rel);
      overriding function Custom_Image (Self : Rel) return String;
   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   generic
      type Ty is private;
      with function Apply (Self : in out Ty) return Boolean is <>;
      with procedure Revert (Self : in out Ty) is <>;
      with procedure Free (Self : in out Ty) is <>;
      with function Custom_Image (Self : Ty) return String is <>;
   package Stateful_Relation is

      --  This package represents a relation that has state,
      --  and that needs to be reset to be reused.

      type State_Type is (Start, Success, Finish);

      type Rel is new I_Relation with record
         State : State_Type := Start;
         Rel   : Ty;
      end record;

      overriding function Solve_Impl (Self : in out Rel) return Boolean;
      overriding procedure Reset (Self : in out Rel);
      overriding procedure Cleanup (Self : in out Rel);

      overriding function Custom_Image (Self : Rel) return String
      is (Custom_Image (Self.Rel));

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
