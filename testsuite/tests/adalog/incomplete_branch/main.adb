with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an equation resolves correctly even if not all variables are
--  defined in all branches.

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   X : constant Raw_Var := Create ("X");
   Y : constant Raw_Var := Create ("Y");

   function Is_Even (V : Integer) return Boolean is (V mod 2 = 0);

   R : constant Relation :=
     ((X = 1 or (Predicate (X, Predicate(Is_Even'Access, "Is_Even"))
                 and Y = 1)));

begin
   Solve_All (R);
end Main;
