with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Unify works correctly, specially that variable aliasing is well
--  reset after evaluating a solution.

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Even (Val : Integer) return Boolean is (Val mod 2 = 0);

   function Is_Even (Var : Refs.Logic_Var) return Relation
   is (Predicate (Var, Predicate (Is_Even'Access, "Is_Even")));

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   Relations : constant array (Positive range <>) of Relation :=

     (X = Y and Domain (X, (1, 2, 3)),
      --  Simple dynamic scheduling: the second relation must be evaluated
      --  before the first one.

      (Domain (X, (1, 2, 3))
       and
         ((Domain (X, (10, 20)) or Is_Even (Y))
          and Domain (Y, (1, 3, 5, 10)))),
      --  The second AND relation (OR) cannot be evaluated completely, but it
      --  makes progress.

      "and" (Is_Even (Y), Domain (X, (1, 2, 3))),
      --  Unsolvable equation: nothing provides a value for Y, but the equation
      --  still makes progress.

      "and" (Is_Even (Y), Is_Even (X)),
      --  Likewise, but the equation makes no progress at all

      "or" (Is_Even (Y), Domain (X, (1, 2))),
      --  Likewise, but for ANY relations

      "or" (Is_Even (X), Is_Even (Y)),

      "or" (Is_Even (X),
             "and" (Domain (X, (1, 2, 3)),
                     Is_Even (Y))),

      "and" (Domain (X, (1, 2, 3)),
              "and" (Is_Even (Y),
                      "and" (Domain (X, (1 => 2)),
                              X = Y)))
      --  Make sure that back-tracking, which happens for the second Member,
      --  properly resets the state so that the second evaluation of this
      --  second Member actually checks something. Without a proper reset, this
      --  stateful relation just yields Unsatisfied.
     );

begin
   for R of Relations loop
      Put_Line ((1 .. 72 => '='));
      New_Line;
      Reset (X);
      Reset (Y);
      Solve_All (R);
   end loop;
end Main;
