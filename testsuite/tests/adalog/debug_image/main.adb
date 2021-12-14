with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Source_Info;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Copy of the dyn_scheduling test. The interest is to see that debug
--  information in relation printing works correclty, i.e. that it is correctly
--  shown when printing a relation.

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function S return String renames GNAT.Source_Info.Source_Location;

   function Is_Even (Val : Integer) return Boolean is (Val mod 2 = 0);

   function Is_Even (Var : Refs.Logic_Var; Dbg_String : String) return Relation
   is (Predicate
        (Var, Predicate (Is_Even'Access, "Is_Even"), Dbg_String));

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   Relations : constant array (Positive range <>) of Relation :=

     (Unify (X, Y, Dbg_String => S) and Domain (X, (1, 2, 3), S),
      --  Simple dynamic scheduling: the second relation must be evaluated
      --  before the first one.

      R_All((Domain (X, (1, 2, 3), S),
             R_Any ((Domain (X, (10, 20)), Is_Even (Y, S)), S),
             Domain (Y, (1, 3, 5, 10), S)), S),
      --  The second AND relation (OR) cannot be evaluated completely, but it
      --  makes progress.

      R_All ((Is_Even (Y, S), Domain (X, (1, 2, 3), S)), S),
      --  Unsolvable equation: nothing provides a value for Y, but the equation
      --  still makes progress.

      R_All ((Is_Even (Y, S), Is_Even (X, S)), S),
      --  Likewise, but the equation makes no progress at all

      R_Any ((Is_Even (Y, S), Domain (X, (1, 2), S)), S),
      --  Likewise, but for ANY relations

      R_Any ((Is_Even (X, S), Is_Even (Y, S)), S),

      R_Any ((Is_Even (X, S),
              R_All ((Domain (X, (1, 2, 3), S), Is_Even (Y, S)), S)), S),

      R_All ((Domain (X, (1, 2, 3), S),
              Is_Even (Y, S),
              Domain (X, (1 => 2), S),
              Unify (X, Y, S)), S)
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
