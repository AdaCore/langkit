--  Check that Adalog aborts solving with a ``Timeout_Error`` exception when
--  the number of solving steps involved exceeds the requested limit.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Even (I : Integer) return Boolean
   is (I mod 2 = 0);
   P_Is_Even : constant Predicate_Type'Class :=
     Predicate (Is_Even'Access, "Is_Even");

   function Is_Next (Values : Value_Array) return Boolean
   is (Values (1) + 1 = Values (2));
   P_Is_Next : constant N_Predicate_Type'Class :=
     N_Predicate (Is_Next'Access, 2, "Is_Next");

   X1 : constant Refs.Logic_Var := Create ("X1");
   X2 : constant Refs.Logic_Var := Create ("X2");
   X3 : constant Refs.Logic_Var := Create ("X3");
   X4 : constant Refs.Logic_Var := Create ("X4");
   X5 : constant Refs.Logic_Var := Create ("X5");
   X6 : constant Refs.Logic_Var := Create ("X6");
   X7 : constant Refs.Logic_Var := Create ("X7");
   X8 : constant Refs.Logic_Var := Create ("X8");
   X9 : constant Refs.Logic_Var := Create ("X9");

   Thirty_Ints : constant Value_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27, 28, 29, 30);

   R1 : constant Relation := R_All ((
      Predicate (X1, P_Is_Even),
      N_Predicate ((X1, X2), P_Is_Next),
      Predicate (X3, P_Is_Even),

      R_Any ((R_All ((X1 = 2, X2 = 4), "A1.1"),
              R_All ((X1 = 1, X2 = 2), "A1.2"),
              R_All ((X1 = 0, X2 = 2), "A1.3"),
              R_All ((X1 = 10, X2 = 11), "A1.4")), "A1"),

      Domain (X3, (1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 20, 21, 23, 25, 27, 29),
              "A2"),

      Domain (X2, (4, 2, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
              "A3"),

      N_Predicate ((X4, X5), P_Is_Next),
      N_Predicate ((X5, X6), P_Is_Next),
      N_Predicate ((X6, X7), P_Is_Next),
      N_Predicate ((X7, X8), P_Is_Next),
      N_Predicate ((X8, X9), P_Is_Next),
      X6 = 19,

      Domain (X4, Thirty_Ints, "A4"),
      Domain (X5, Thirty_Ints, "A5"),
      Domain (X6, Thirty_Ints, "A6"),
      Domain (X7, Thirty_Ints, "A7"),
      Domain (X8, Thirty_Ints, "A8"),
      Domain (X9, Thirty_Ints, "A9")
   ));

   R2 : constant Relation := R_All ((
      Domain (X1, Thirty_Ints),
      Domain (X2, Thirty_Ints),
      Predicate (X1, P_Is_Even),
      N_Predicate ((X1, X2), P_Is_Next)
   ));

begin
   --  Check that the timeout triggers on R1: if it did not, solving this
   --  equation would take a looong time because of all the disjunctions
   --  involved (``Domain``).

   Solve_All (R1, Timeout => 10000);

   --  But check that it does not trigger on R2, a smaller equation
   Solve_All (R2, Timeout => 10000);
end Main;
