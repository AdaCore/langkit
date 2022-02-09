with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Unify works correctly, specially that variable aliasing is well
--  reset after evaluating a solution.

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Abs_Equal (Vals : Value_Array) return Boolean is
     (abs Vals (1) = abs Vals (2));

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   R : constant Relation := R_All
     ((R_Any ((X = Y, Logic_True)),
       R_Any ((Domain (X, (1, 2)), Logic_True)),
       R_Any ((Domain (Y, (-1, -2)), Logic_True)),
       N_Predicate ((X, Y), N_Predicate (Abs_Equal'Access, 2))));
begin
   Solve_All (R);
end Main;
