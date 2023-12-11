--  Check that Adalog can create meaningful contradictions when exceptions
--  arise when evaluating a partial solution, instead of falling back to a
--  generic "this particular candidate is not valid" explanation that could
--  end up causing time outs in complex cases.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Langkit_Support.Errors; use Langkit_Support.Errors;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Even (Dummy_I : Integer) return Boolean
   is (raise Property_Error);
   P_Is_Even : constant Predicate_Type'Class :=
     Predicate (Is_Even'Access, "Is_Even");

   X1 : constant Refs.Logic_Var := Create ("X1");
   X2 : constant Refs.Logic_Var := Create ("X2");
   X3 : constant Refs.Logic_Var := Create ("X3");
   X4 : constant Refs.Logic_Var := Create ("X4");
   X5 : constant Refs.Logic_Var := Create ("X5");
   X6 : constant Refs.Logic_Var := Create ("X6");

   Thirty_Ints : constant Value_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27, 28, 29, 30);

   R1 : constant Relation := R_All ((
      Domain (X1, Thirty_Ints),
      Domain (X2, Thirty_Ints),
      Domain (X3, Thirty_Ints),
      Domain (X4, Thirty_Ints),
      Domain (X5, Thirty_Ints),
      Predicate (X3, P_Is_Even),
      Predicate (X6, P_Is_Even)
   ));

begin
   --  When trying to solve this equation, the theory will be proposed
   --  candidates which all have orphan relations (because X6 is never set,
   --  but is used in the last predicate). However, it will still try to
   --  evaluate the partial solutions (without the orphan atoms) to try to
   --  find contradictions. But here, many of these attempts will end with a
   --  property error (due to how the predicate is implemented). In that case
   --  the solver used to fallback to a generic contradiction which simply
   --  states that the particular attempted candidate did not work out.
   --  However, this is too "precise" and does not allow preventing the actual
   --  reason for the failure, thus the theory will have to evaluate a huge
   --  number of candidates which contain the exact same problem and end up
   --  triggering a timeout.
   --
   --  Now, in case of an exception when evaluating a partial solution, the
   --  solver will instead build a contradiction that forbids the orphan
   --  atoms in the candidate. In this particular scenario, this allows
   --  concluding in a single round that the equation is unsatisfiable.
   Solve_All (R1, Timeout => 10000);
end Main;
